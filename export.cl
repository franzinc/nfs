(in-package :user)

(eval-when (compile) (declaim (optimize (speed 3))))

(defparameter *exports* nil)
(defvar *exports-lock* 
    (mp:make-process-lock :name "*exports* lock"))
(defparameter *old-exports* nil)
(defparameter *next-export-id* 1)


;; couldn't use "export" or "exp"
(defstruct nfs-export
  id
  name
  path
  uid
  gid
  umask
  set-mode-bits
  hosts-allow
  rw-users
  ro-users
  match)

(defun prepare-exports ()
  (mp:with-process-lock (*exports-lock*)
    (setf *old-exports* (coerce *exports* 'list))
    (setf *exports* nil)))

;; selections will be a list of strings (or a single string, which
;; will be converted to a list).
(defun expand-access-list (selections table)
  (if (not (listp selections))
      (setf selections (list selections)))
  (let (res)
    (dolist (selection selections)
      (multiple-value-bind (expansion found)
	  (gethash selection table)
	(when (not found)
	  (error "~S is not a defined list" selection))
	(setf res (append res expansion))))
    ;; check for special case.
    (if* (member t res)
       then
	    t
       else
	    (remove-duplicates res :test #'equalp))))

(defun build-export-match (name)
  "Builds a regular expression for use."
  (compile-re (format nil "^~A(/.*)?$" name)))

(defun define-export (&key name path (uid 9999) (gid 9999)
			   (umask 0) (set-mode-bits 0)
			   hosts-allow rw-users ro-users 
			   host-lists user-lists)
  (when (null name)
    (error ":name must be specified for define-export"))
  (when (null path)
    (error ":path must be specified for define-export"))
  (let ((canonical-name (canonicalize-name name)))
    (if canonical-name
	(mp:with-process-lock (*exports-lock*)
	  (setf *exports*
	    (append *exports*
		    (list
		     (make-nfs-export
		      :id (select-export-id)
		      :name canonical-name
		      :path (cleanup-dir path)
		      :uid uid
		      :gid gid
		      :umask umask
		      :set-mode-bits set-mode-bits
		      :hosts-allow (expand-access-list hosts-allow host-lists)
		      :rw-users (expand-access-list rw-users user-lists)
		      :ro-users (expand-access-list ro-users user-lists)
		      :match (build-export-match canonical-name))))))
      (logit-stamp "Export with name '~A' isn't an acceptable name, flush your configuration and reconfigure!"
		   name))))


;; used as fsid in getattr calls.
(defun select-export-id ()
  (prog1 
      *next-export-id* 
    (incf *next-export-id*)))

(defun exports-match-p (exp1 exp2)
  (and 
   (string= (nfs-export-name exp1) (nfs-export-name exp2))
   (equalp (nfs-export-path exp1) (nfs-export-path exp2))))

(defun finalize-exports ()
  ;; Check for exports that have been removed or changed.
  (let (new-exports)
    (mp:with-process-lock (*exports-lock*)
      (dolist (exp *exports*)
	(let ((oldexp (find exp *old-exports* :test #'exports-match-p)))
	  (if* oldexp
	     then
		  ;; a (possibly) changed export.   Update the existing (old)
		  ;; export structure with the new information.
		  (setf (nfs-export-uid oldexp) (nfs-export-uid exp))
		  (setf (nfs-export-gid oldexp) (nfs-export-gid exp))
		  (setf (nfs-export-umask oldexp) (nfs-export-umask exp))
		  (setf (nfs-export-set-mode-bits oldexp)
		    (nfs-export-set-mode-bits exp))
		  (setf (nfs-export-hosts-allow oldexp)
		    (nfs-export-hosts-allow exp))
		  (setf (nfs-export-rw-users oldexp) (nfs-export-rw-users exp))
		  (setf (nfs-export-ro-users oldexp) (nfs-export-ro-users exp))
		  ;; Remove entry from old exports.
		  (setf *old-exports* (remove oldexp *old-exports* :test #'eq))
		  ;; add to list of good exports
		  (push oldexp new-exports)
	     else
		  ;; new
		  (push exp new-exports))))

      ;; Convert to an array for efficient access and sort by longest
      ;; export first, which allows regexp searching to function properly.
      (setf *exports* (coerce new-exports 'vector))
      (sort *exports* #'string> :key #'nfs-export-name))

    ;; *old-exports* is now the list of exports that existed before but
    ;; which don't now.  The file handles for that export need to be 
    ;; invalidated.
    (dolist (exp *old-exports*)
      (invalidate-export-fhandles exp))))

(defun locate-nearest-export (path)
  (mp:with-process-lock (*exports-lock*)
    (if (null *exports*)
	(return-from locate-nearest-export nil))
    (let (exp)
      (dotimes (n (length *exports*))
	(setf exp (svref *exports* n))
	(multiple-value-bind (ok match tail) 
	    (match-re (nfs-export-match exp) path)
	  (when (and ok match)
	    (return-from locate-nearest-export 
	      (values exp
		      (or tail "")))))))))

(defun export-host-access-allowed-p (exp addr)
  (declare (optimize speed (safety 0) (debug 0)))
  (dolist (allow (nfs-export-hosts-allow exp))
    (if (addr-in-network-p addr allow)
	(return t))))

;; uid is an integer (or nil if auth-unix auth info was
;; not supplied)
(defun export-user-write-access-allowed-p (exp uid)
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((rw-users (nfs-export-rw-users exp)))
    (or (eq rw-users t)
	(eq uid (nfs-export-uid exp))
	(member uid rw-users :test #'eq))))

;; uid is an integer (or nil if auth-unix auth info was
;; not supplied)
(defun export-user-read-access-allowed-p (exp uid)
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((ro-users (nfs-export-ro-users exp)))
    (or (eq ro-users t)
	(export-user-write-access-allowed-p exp uid)
	(member uid ro-users :test #'eq))))

