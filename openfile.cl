(in-package :user)

(defparameter *openfilereaptime* 2) ;; seconds

;; keys are file handles.
(defparameter *open-file-cache-read* (make-hash-table :test #'eq))
(defparameter *open-file-cache-write* (make-hash-table :test #'eq))

(defparameter *open-file-cache-lock* (mp:make-process-lock))

(defmacro direction-to-cache-hash (dir)
  `(ecase ,dir
     (:input
      *open-file-cache-read*)
     (:output
      *open-file-cache-write*)))

(defstruct openfile
  (lock (mp:make-process-lock))
  direction
  stream
  (lastaccess (excl::cl-internal-real-time))
  (refcount 0))

;; All locate-open-file* functions should be called
;; with *open-file-cache-lock* held
(defmacro locate-open-file-input (fh)
  `(gethash ,fh *open-file-cache-read*))

(defmacro locate-open-file-output (fh)
  `(gethash ,fh *open-file-cache-write*))

(defmacro locate-open-file-any (fh)
  (let ((fhvar (gensym)))
    `(let ((,fhvar ,fh))
       (or (locate-open-file-input ,fhvar)
	   (locate-open-file-output ,fhvar)))))

(defmacro locate-open-file (fh direction)
  (let ((fhsym (gensym))
	(dirsym (gensym)))
    `(let ((,fhsym ,fh)
	   (,dirsym ,direction))
       (if* (eq ,dirsym :any)
	  then (locate-open-file-any ,fhsym)
	  else (gethash ,fhsym (direction-to-cache-hash ,dirsym))))))

(defmacro put-open-file (fh of)
  (let ((fhvar (gensym))
	(ofvar (gensym)))
    `(let ((,fhvar ,fh)
	   (,ofvar ,of))
       (setf (gethash ,fhvar 
		      (direction-to-cache-hash (openfile-direction ,ofvar)))
	 ,ofvar))))


;; Only called via the with-nfs-open-file macro.  
(defun get-open-file (fh direction)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (mp:with-process-lock (*open-file-cache-lock*)
    (let ((of (locate-open-file fh direction)))
      (when (null of)
	;; no entry found.. make a new one.
	(if (eq direction :any)
	    (error "Can't create an open file entry with direction :any"))
	(setf of (make-openfile :direction direction))
	(setf (openfile-stream of)
	  (if* (eq direction :input)
	     then (open (fh-pathname fh) :direction :input)
	     else (open (fh-pathname fh) :direction :output
		  :if-exists :overwrite)))
	(put-open-file fh of))
      ;; common
      (setf (openfile-lastaccess of) (excl::cl-internal-real-time))
      (values (openfile-stream of) of))))

(defmacro with-nfs-open-file ((var fh direction &key of) &body body) 
  (if (null of)
      (setf of (gensym)))
  `(multiple-value-bind (,var ,of)
       (get-open-file ,fh ,direction)
     (declare (ignore-if-unused ,var))
     (mp:with-process-lock ((openfile-lock ,of))
       (incf (the fixnum (openfile-refcount ,of)))
       (unwind-protect
	   (progn ,@body)
	 (decf (the fixnum (openfile-refcount ,of)))))))

;; Called by:
;; nfsd-rename, :operator
;; nfsd-remove, :operator
;; set-file-attributes, :operator
(defun close-open-file (fh &key check-refcount)
  (mp:with-process-lock (*open-file-cache-lock*)
    (let (of)
      (while (setf of (locate-open-file-any fh))
	(if (and check-refcount (not (zerop (openfile-refcount of))))
	    (return :still-open))
	(reap-open-file 
	 fh of (direction-to-cache-hash (openfile-direction of)))))))

;; Should be called with *open-file-cache-lock* held.
(defun reap-open-file (fh of hash)
  (if (not (zerop (openfile-refcount of)))
      (error "reap-open-file called when refcount is non-zero"))
  (close (openfile-stream of))
  (remhash fh hash))


(defun reap-open-files ()
  (mp:with-process-lock (*open-file-cache-lock*)
    (let ((now (excl::cl-internal-real-time)))
      (dolist (dir '(:input :output))
	(let ((hash (direction-to-cache-hash dir)))
	  (maphash
	   #'(lambda (fh of)
	       (when (and (>= now (+ *openfilereaptime* 
				     (openfile-lastaccess of)))
			  (zerop (openfile-refcount of)))
		 (reap-open-file fh of hash)))
	   hash))))))

(defun nfsd-open-file-reaper ()
  (loop
    (sleep *openfilereaptime*)
    (reap-open-files)))
