(in-package :user)

(defparameter *openfilereaptime* 2) ;; seconds

;; keys are file handles.
(defparameter *open-file-cache-read* (make-hash-table :test #'eq))
(defparameter *open-file-cache-write* (make-hash-table :test #'eq))

(defmacro direction-to-cache-hash (dir)
  `(ecase ,dir
     (:input
      *open-file-cache-read*)
     (:output
      *open-file-cache-write*)))

(defstruct openfile
  direction
  stream
  (lastaccess (excl::cl-internal-real-time)))

(defparameter *open-file-cache-lock* (mp:make-process-lock))

(defmacro locate-open-file-input (fh)
  `(gethash ,fh *open-file-cache-read*))

(defmacro locate-open-file-output (fh)
  `(gethash ,fh *open-file-cache-write*))

(defmacro locate-open-file (fh direction)
  `(gethash ,fh (direction-to-cache-hash ,direction)))

(defmacro locate-open-file-any (fh)
  (let ((fhvar (gensym)))
    `(let ((,fhvar ,fh))
       (or (locate-open-file-input ,fhvar)
	   (locate-open-file-output ,fhvar)))))

(defmacro put-open-file (fh of)
  (let ((fhvar (gensym))
	(ofvar (gensym)))
    `(let ((,fhvar ,fh)
	   (,ofvar ,of))
       (setf (gethash ,fhvar 
		      (direction-to-cache-hash (openfile-direction ,ofvar)))
	 ,ofvar))))


;; Should be called with *open-file-cache-lock* held.
(defun get-open-file (fh direction)
  (let ((of (locate-open-file fh direction)))
    (when (null of)
      ;; no entry found.. make a new one.
      (setf of (make-openfile :direction direction))
      (setf (openfile-stream of)
	(if (eq direction :input)
	    (open (fh-pathname fh) :direction :input)
	  (open (fh-pathname fh) :direction :output
		:if-exists :overwrite)))
      (put-open-file fh of))
    ;; common
    (setf (openfile-lastaccess of) (excl::cl-internal-real-time))
    (openfile-stream of)))

(defmacro with-nfs-open-file ((var fh direction) &body body) 
  `(mp:with-process-lock (*open-file-cache-lock*)
     (let ((,var (get-open-file ,fh ,direction)))
       ,@body)))

(defun close-open-file (fh)
  (mp:with-process-lock (*open-file-cache-lock*)
    (let (of)
      (while (setf of (locate-open-file-any fh))
	(reap-open-file fh of 
			(direction-to-cache-hash (openfile-direction of)))))))

;; should be called with lock held.
(defun reap-open-file (fh of hash)
  (let ((pathname (fh-pathname fh)))
    (close (openfile-stream of))
    (if (eq (openfile-direction of) :output)
	(let ((attr (lookup-attr fh)))
	  (utime pathname 
		 (nfs-attr-atime attr) 
		 (nfs-attr-mtime attr))))
    (remhash fh hash)))


(defun reap-open-files ()
  (mp:with-process-lock (*open-file-cache-lock*)
    (let ((now (excl::cl-internal-real-time)))
      (dolist (dir '(:input :output))
	(let ((hash (direction-to-cache-hash dir)))
	  (maphash
	   #'(lambda (fh of)
	       (when (>= now (+ *openfilereaptime* (openfile-lastaccess of)))
		 (reap-open-file fh of hash)))
	   hash))))))
    


(defun nfsd-open-file-reaper ()
  (loop
    (sleep *openfilereaptime*)
    (reap-open-files)))










