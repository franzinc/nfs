(in-package :cg-user)

(defparameter *exports* nil)

;; couldn't use "export" or "exp"
(defstruct nfs-export
  index
  name
  path
  uid
  gid
  umask
  set-mode-bits
  hosts-allow
  rw-users
  ro-users)

(defun cleanup-dir (dir)
  ;; Change all forward slashes to backslashes.  
  (setf dir (substitute #\\ #\/ dir))
  
  (multiple-value-bind (matched dummy remainder)
      (match-regexp "^[a-z]:\\(.*\\)" dir :case-fold t)
    (declare (ignore dummy))
    (if (not matched)
	(error "~A is not a valid directory specification." dir))
    (cond
     ((string= remainder "")
      (concatenate 'string dir "\\"))
     ((string= remainder "\\")
      dir)
     ((char= (schar dir (1- (length dir))) #\\)
      ;; strip trailing backslash
      (subseq dir 0 (1- (length dir))))
     (t ;; already in canonical form
      dir))))
      

(defun define-export (&key name path (uid 9999) (gid 9999)
                           (umask 0) (set-mode-bits 0)
                           hosts-allow rw-users ro-users)
  (when (null name)
    (error ":name must be specified for define-export"))
  (when (null path)
    (error ":path must be specified for define-export"))
  ;; canonicalize
  (if (and hosts-allow (not (listp hosts-allow)))
      (setf hosts-allow (list hosts-allow)))
  (if (and rw-users (not (listp rw-users)))
      (setf rw-users (list rw-users)))
  (if (and ro-users (not (listp ro-users)))
      (setf ro-users (list ro-users)))
  (setf *exports*
    (append *exports*
            (list
             (make-nfs-export
              :index (length *exports*)
              :name name
              :path (cleanup-dir path)
              :uid uid
              :gid gid
              :umask umask
              :set-mode-bits set-mode-bits
              :hosts-allow hosts-allow
              :rw-users rw-users
              :ro-users ro-users)))))

  

  

