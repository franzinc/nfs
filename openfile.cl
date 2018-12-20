(in-package :user)

;; keys are file handles.
(defparameter *open-file-cache* (make-hash-table :test #'eq))
(defparameter *open-file-cache-lock* (mp:make-process-lock))

(defstruct openfile
  (lock (mp:make-process-lock))
  direction
  stream
  (lastaccess (excl::cl-internal-real-time))
  (refcount 0))


;; Call with *open-file-cache-lock* held
(defmacro locate-open-file (fh)
  `(gethash ,fh *open-file-cache*))

;; Call with *open-file-cache-lock* held
(defmacro put-open-file (fh of)
  `(setf (gethash ,fh *open-file-cache*) ,of))

;; Only called via the with-nfs-open-file macro.  
(defun get-open-file (fh direction)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (mp:with-process-lock (*open-file-cache-lock*)
    (let ((of (locate-open-file fh)))
      (when (null of)
	;; no entry found.. make a new one.
	(setf of (make-openfile :direction direction))
	(setf (openfile-stream of)
	  (if* (eq direction :input)
	     then (unicode-open (fh-pathname fh) :direction :input)
	     else (unicode-open (fh-pathname fh) :direction :io
			:if-exists :open
			:if-does-not-exist :create)))
	#+ignore
	(format t "Opened for ~a ~a~%" direction (fh-pathname fh))
	(put-open-file fh of))
      ;; common

      (if* (and (not (eq direction (openfile-direction of)))
		(eq direction :output))
	 then ;; Escalate from read-only to read-write for open type,
	      ;; because we don't want to open read-write unless we have
	      ;; to.
	      #+ignore
	      (progn
		(format t "Escalating open type for ~a~%" (fh-pathname fh))
		(format t "Closing...~%"))
	      (close (openfile-stream of))
	      ;; Remove from hash in case the the reopen fails.
	      (remhash fh *open-file-cache*)
	      #+ignore (format t "Reopening for output...~%")
	      ;; This could possibly result in an error.
	      (setf (openfile-stream of)
		(unicode-open (fh-pathname fh) :direction :io
		      :if-exists :open
		      :if-does-not-exist :create))
	      (put-open-file fh of)
	      #+ignore (format t "Escalation complete.~%"))
      
      (setf (openfile-lastaccess of) (excl::cl-internal-real-time))
      (values (openfile-stream of) of))))

(defmacro with-nfs-open-file ((var fh direction &key (of (gensym)))
			      &body body)
  (let* ((g-fh (gensym))
	 (g-direction (gensym)))
    `(let* ((,g-fh ,fh)
	    (,g-direction ,direction))
       (multiple-value-bind (,var ,of)
	   (get-open-file ,g-fh ,g-direction)
	 (declare (ignore-if-unused ,var))
	 (mp:with-process-lock ((openfile-lock ,of))
	   (incf (the fixnum (openfile-refcount ,of)))
	   (unwind-protect (progn ,@body)
	     (decf (the fixnum (openfile-refcount ,of)))
	     (when (and (= 0 (the fixnum (openfile-refcount ,of)))
			(= 0 *open-file-reap-time*))
	       (mp:with-process-lock (*open-file-cache-lock*)
		 (reap-open-file ,g-fh ,of)))))))))

;; Called by:
;; nfsd-rename, :operator
;; nfsd-remove, :operator
(defun close-open-file (fh &key check-refcount)
  (mp:with-process-lock (*open-file-cache-lock*)
    (let ((of (locate-open-file fh)))
      (when of
	(if (and check-refcount (not (zerop (openfile-refcount of))))
	    (return-from close-open-file :still-open))
	(reap-open-file fh of)))))

;; NOTE: callers are responsible for calling this function only when
;;       they hold the *open-file-cache-lock* lock.
(defun reap-open-file (fh of)
  (when (not (zerop (openfile-refcount of)))
    (error "reap-open-file called when refcount is non-zero"))
  #+ignore (format t "Closing ~a~%" (fh-pathname fh))
  (close (openfile-stream of))
  (remhash fh *open-file-cache*))


(defun reap-open-files ()
  (mp:with-process-lock (*open-file-cache-lock*)
    (let ((now (excl::cl-internal-real-time))
	  (reaptime *open-file-reap-time*))
      (maphash
       #'(lambda (fh of)
	   (when (and (>= now (+ reaptime (openfile-lastaccess of)))
		      (zerop (openfile-refcount of)))
	     (reap-open-file fh of)))
       *open-file-cache*))))

(defun initialize-reaper-process ()
  (mp:process-run-function "open file reaper" #'nfsd-open-file-reaper))

(defun nfsd-open-file-reaper ()
  (loop
    (if* (= 0 *open-file-reap-time*)
       then ;; The idea is, that if *open-file-reap-time* is 0, the only
	    ;; way we can do real work in this loop is if the configuration
	    ;; program changes the value, and checking it once a minute
	    ;; seems reasonable, in this case.
	    (sleep 60)
       else (sleep (max *open-file-reap-time* 1))
	    (reap-open-files))))
