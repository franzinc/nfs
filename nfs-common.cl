(in-package :user)

(defconstant NFS_OK 0)
(defconstant NFSERR_PERM 1)
(defconstant NFSERR_NOENT 2)
(defconstant NFSERR_IO 5)
(defconstant NFSERR_NXIO 6)
(defconstant NFSERR_ACCES 13) ;; permission denied
(defconstant NFSERR_EXIST 17)
(defconstant NFSERR_NODEV 19)
(defconstant NFSERR_NOTDIR 20)
(defconstant NFSERR_ISDIR 21)
(defconstant NFSERR_FBIG 27)
(defconstant NFSERR_NOSPC 28)
(defconstant NFSERR_ROFS 30)
(defconstant NFSERR_NAMETOOLONG 63)
(defconstant NFSERR_NOTEMPTY 66)
(defconstant NFSERR_DQUOT 69)
(defconstant NFSERR_STALE 70)
(defconstant NFSERR_WFLUSH 99)

(defconstant *blocksize* 8192)

(defparameter *nfs-debug* nil)
(defparameter *nfs-gc-debug* nil)

(defun map-errno-to-nfs-error-code (errno)
  (case errno
    (#.*enoent* NFSERR_NOENT) 
    (#.*eio* NFSERR_IO)   
    (#.*eacces* NFSERR_ACCES)
    (#.*enfile* NFSERR_ACCES)
    (#.*enotempty* NFSERR_NOTEMPTY) 
    (t NFSERR_IO))) ;; very general... avoid

;; Need proper error reporting.
(eval-when (compile load eval)
  (setf excl::*strict-probe-file* t))
    
(defun roundup (value multiple)
  (let ((mod (mod value multiple)))
    (+ value (if (> mod 0) (- multiple mod) 0))))

;;; return how many blocks are required to contain 'value' items
;;; given a particular blocksize
(defun howmany (value blocksize)
  (/ (roundup value blocksize) blocksize))
