(in-package :user)

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi)
  (use-package :gen-nfs))

;;;
(defvar *nfsd-version* "4.5b5")
(defvar *nfsd-long-version* (format nil "~a (NFSv2/NFSv3)" *nfsd-version*))
;;; 

(defconstant *blocksize* 8192)

(defparameter *nfs-debug* nil)
(defparameter *nfs-gc-debug* nil)
(defparameter *nfs-debug-timings* nil) 
(defparameter *nfs-set-mtime-on-write* nil)

(defconstant *nfs-debug-read*        #x00000001)
(defconstant *nfs-debug-write*       #x00000002)
(defconstant *nfs-debug-readdir*     #x00000004) ;; includes readdirplus
(defconstant *nfs-debug-getattr*     #x00000008)
(defconstant *nfs-debug-setattr*     #x00000010)
(defconstant *nfs-debug-lookup*      #x00000020)
(defconstant *nfs-debug-access*      #x00000040)
(defconstant *nfs-debug-create*      #x00000080)
(defconstant *nfs-debug-mkdir*       #x00000100)
(defconstant *nfs-debug-rmdir*       #x00000200)
(defconstant *nfs-debug-remove*      #x00000400)
(defconstant *nfs-debug-rename*      #x00000800)
(defconstant *nfs-debug-fsstat*      #x00001000)
(defconstant *nfs-debug-fsinfo*      #x00002000)
(defconstant *nfs-debug-pathconf*    #x00004000)
(defconstant *nfs-debug-commit*      #x00008000)
(defconstant *nfs-debug-null*        #x00010000)
(defconstant *nfs-debug-statfs*      #x00020000)
(defconstant *nfs-debug-link*        #x00040000)
(defconstant *nfs-debug-symlink*     #x00080000)
(defconstant *nfs-debug-readlink*    #x00100000)
(defconstant *nfs-debug-mknod*       #x00200000)

(defparameter *nfs-debug-filter*     #x0fffffff)

(defmacro nfs-debug-filter-on (type)
  (if (eq type 'readdirplus)
      (setf type 'readdir))
  (let ((constant (intern (format nil "*nfs-debug-~a*" type))))
    `(and *nfs-debug* (/= 0 (logand *nfs-debug-filter* ,constant)))))

(defun map-errno-to-nfs-error-code (errno)
  (case errno
    (#.*enoent* *nfserr-noent*)
    (#.*eio* *nfserr-io*)
    (#.*eacces* *nfserr-acces*)
    (#.*enfile* *nfserr-acces*)
    (#.*enotempty* *nfserr-notempty*) 
    (#.*eexist* *nfserr-exist*)
    ;; very general... avoid.  For v3, should should be
    ;; *nfserr-serverfault*
    (t *nfserr-io*))) 

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
     (logit-stamp ,format ,@format-args)
     (console-control :close t :show t)
     (exit 1)))

(ff:def-foreign-call MoveFileExA ((from (* :char)) 
				  (to (* :char))
				  (flags :int))
  :strings-convert t
  :returning :boolean
  :error-value :os-specific)

(defconstant MOVEFILE_REPLACE_EXISTING 1)

(defun my-rename (from to)
  (multiple-value-bind (success winerr)
      (MoveFileExA from to MOVEFILE_REPLACE_EXISTING)
    (if* success
       then t
       else (excl.osi:perror (excl.osi::win_err_to_errno winerr)
			     "rename failed"))))
	   
