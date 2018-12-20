(in-package :cg-user)

(defparameter *exports* nil)

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

(defun define-export (&key name path (uid 9999) (gid 9999)
                           (umask 0) (set-mode-bits 0)
                           hosts-allow rw-users ro-users)
  (when (null name)
    (error ":name must be specified for define-export"))
  (when (null path)
    (error ":path must be specified for define-export"))
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
              :path (user::cleanup-dir path)
              :uid uid
              :gid gid
              :umask umask
              :set-mode-bits set-mode-bits
              :hosts-allow hosts-allow
              :rw-users rw-users
              :ro-users ro-users)))))

(defun find-export (name)
  (setf name (directory-tree:canonicalize-path name))
  
  (dolist (exp *exports*)
    (when (equalp (directory-tree:canonicalize-path (nfs-export-name exp)) name)
      (return exp))))
