(in-package :user)

;;;
(defvar *nfsd-version* "4.3")
(defvar *nfsd-long-version* (format nil "~a (NFSv2/NFSv3)" *nfsd-version*))
;;; 

(defconstant NFS_OK 0)

;; Not owner. The operation was not allowed because the caller is
;; either not a privileged user (root) or not the owner of the target
;; of the operation.
(defconstant NFSERR_PERM 1) 

(defconstant NFSERR_NOENT 2) ;; No such file or directory

;; I/O error. A hard error (for example, a disk error) occurred while
;; processing the requested operation.  For NFSv2, we use this for
;; operations which we do not support.  V3 has NFSERR_NOTSUPP for this
;; purpose.
(defconstant NFSERR_IO 5)

(defconstant NFSERR_NXIO 6) ;; I/O error. No such device or address

;; Permission denied. The caller does not have the correct permission
;; to perform the requested operation. Contrast this with
;; NFSERR_PERM, which restricts itself to owner or privileged user
;; permission failures.
(defconstant NFSERR_ACCES 13) 

(defconstant NFSERR_EXIST 17)
(defconstant NFSERR_NODEV 19) ;; No such device.
(defconstant NFSERR_NOTDIR 20)
(defconstant NFSERR_ISDIR 21)
(defconstant NFSERR_FBIG 27)
(defconstant NFSERR_NOSPC 28)
(defconstant NFSERR_ROFS 30)
(defconstant NFSERR_NAMETOOLONG 63)
(defconstant NFSERR_NOTEMPTY 66)
(defconstant NFSERR_DQUOT 69)
(defconstant NFSERR_STALE 70)
(defconstant NFSERR_WFLUSH 99) ;; v2 only

;; All these are v3 only
(defconstant NFSERR_XDEV 18) ;; Attempt to do a cross-device hard link. 

;; Invalid argument or unsupported argument for an operation. Two
;; examples are attempting a READLINK on an object other than a
;; symbolic link or attempting to SETATTR a time field on a server
;; that does not support this operation
(defconstant NFSERR_INVAL 22)

(defconstant NFSERR_MLINK 31)

; Too many levels of remote in path. The file handle given in the
; arguments referred to a file on a non-local file system on the
; server
(defconstant NFSERR_REMOTE 71)

;; Illegal NFS file handle. The file handle failed internal
;; consistency checks
(defconstant NFSERR_BADHANDLE 10001)

;; Update synchronization mismatch was detected during a SETATTR
;; operation
(defconstant NFSERR_NOT_SYNC  10002)

(defconstant NFSERR_BAD_COOKIE 10003) ;; READDIR or READDIRPLUS cookie is stale

(defconstant NFSERR_NOTSUPP   10004) 
(defconstant NFSERR_TOOSMALL  10005) ;; Buffer or request is too small. 

;; An error occurred on the server which does not map to any of the
;; legal NFS version 3 protocol error values. The client should
;; translate this into an appropriate error. UNIX clients may choose
;; to translate this to EIO
(defconstant NFSERR_SERVERFAULT 10006)

;; An attempt was made to create an object of a type not supported by
;; the server.
(defconstant NFSERR_BADTYPE   10007)

;; The server initiated the request, but was not able to complete it
;; in a timely fashion. The client should wait and then try the
;; request with a new RPC transaction ID. For example, this error
;; should be returned from a server that supports hierarchical storage
;; and receives a request to process a file that has been migrated. In
;; this case, the server should start the immigration process and
;; respond to client with this error.
(defconstant NFSERR_JUKEBOX   10008)


(defconstant *blocksize* 8192)

(defparameter *nfs-debug* nil)
(defparameter *nfs-gc-debug* nil)
(defparameter *nfs-debug-timestamps* nil)

(defun map-errno-to-nfs-error-code (errno)
  (case errno
    (#.*enoent* NFSERR_NOENT) 
    (#.*eio* NFSERR_IO)   
    (#.*eacces* NFSERR_ACCES)
    (#.*enfile* NFSERR_ACCES)
    (#.*enotempty* NFSERR_NOTEMPTY) 
    (#.*eexist* NFSERR_EXIST)
    ;; very general... avoid.  For v3, should should be
    ;; NFSERR_SERVERFAULT
    (t NFSERR_IO))) 

;; Needed for proper error reporting.
(eval-when (compile load eval)
  (setf excl::*strict-probe-file* t))
    
(defun roundup (value multiple)
  (let ((mod (mod value multiple)))
    (+ value (if (> mod 0) (- multiple mod) 0))))

;;; return how many blocks are required to contain 'value' items
;;; given a particular blocksize
(defun howmany (value blocksize)
  (/ (roundup value blocksize) blocksize))

(defmacro bailout (format &rest format-args)
  `(progn
     (logit ,format ,@format-args)
     (console-control :close t :show t)
     (exit 1)))

