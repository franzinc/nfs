(in-package :user)

(defparameter *configfile* nil)

(defun read-nfs-cfg (configfile)
  (let ((host-lists (make-hash-table :test #'equalp))
	(user-lists (make-hash-table :test #'equalp))
	config cmd name)
    (prepare-exports)
    (setf config (read-from-string (file-contents configfile)))
    (dolist (entry config)
      (when (not (listp entry))
	(error "Invalid configuration entry: ~S~%" entry))
      (setf cmd (pop entry))
      (case cmd
	(define-host-list
	    (setf name (pop entry))
	    (setf (gethash name host-lists)
	      (mapcar #'parse-addr entry)))
	(define-user-list
	    (setf name (pop entry))
	    (setf (gethash name user-lists) entry))
	(define-export
	    (setf entry (append entry (list :host-lists host-lists
					    :user-lists user-lists)))
	    (apply #'define-export entry))
	(t
	 (set cmd (pop entry)))))
    (finalize-exports)))
  
