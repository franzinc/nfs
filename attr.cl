(in-package :user)

(eval-when (compile) (declaim (optimize (speed 3))))

;; file types
(defconstant *NFNON* 0)
(defconstant *NFREG* 1)
(defconstant *NFDIR* 2)
(defconstant *NFBLK* 3)
(defconstant *NFCHR* 4)
(defconstant *NFLNK* 5)
;; v3
(defconstant *NFSOCK* 6)
(defconstant *NFFIFO* 7)

(defstruct nfs-attr
  type
  mode
  nlinks
  uid
  gid
  size
  blocksize ;; v2 only
  used ;; v3.  used disk space in bytes (may be less than filesize in case of sparse files)
  rdev ;; we don't support this.. so always unused.
  blocks ;; v2 only
  fsid
  fileid
  atime ;; stored as universal time
  mtime ;; stored as universal time
  ctime ;; stored as universal time
  )

;; should always be larger than *openfilereaptime*.  FIXME: Why?
(defparameter *attr-cache-reap-time* 5) 

;; keys are file handles
(defparameter *nfs-attr-cache* (make-hash-table :test #'eq))
(defparameter *attr-cache-lock* (mp:make-process-lock))

(defmacro stat-mode-to-type (mode)
  `(ecase (logand ,mode #o0170000)
     (#o0040000
      *NFDIR*)
     (#o0100000
      *NFREG*)
     (#o0120000
      *NFLNK*)))

(defun nfs-stat (fh)
  (declare (optimize (speed 3)))
  ;;(logit "Collecting fresh attrs for ~a" (fh-pathname fh))
  (multiple-value-bind (mode nlink uid gid size atime mtime ctime)
      (unicode-stat (fh-pathname fh))
    (let ((type (stat-mode-to-type mode)))
      (if* (eq type *NFDIR*)
	 then (setf size 512))
      (make-nfs-attr
       :type type
       :mode mode
       :nlinks (if (eq nlink 0) 1 nlink)
       :uid uid 
       :gid gid
       :size size
       :blocksize 512
       :used size
       :blocks (howmany size 512)
       :fsid (nfs-export-id (fh-export fh))
       :fileid (fh-id fh)
       :atime atime
       :mtime mtime
       :ctime ctime))))

(defstruct nfs-attr-cache 
  attr
  expiration
  )

(defun lookup-attr (fh)
  (mp:with-process-lock (*attr-cache-lock*)
    (let ((attr-cache (gethash fh *nfs-attr-cache*)))
      (if* attr-cache
	 then ;; We have a cached entry.  Check its expiration
	      (let ((now (excl::cl-internal-real-time)))
		(if* (>= now (nfs-attr-cache-expiration attr-cache))
		   then ;; It expired.  Refresh the attributes and return.
			(let ((attr (nfs-stat fh)))
			  (setf (nfs-attr-cache-attr attr-cache) attr)
			  (setf (nfs-attr-cache-expiration attr-cache) (+ now *attr-cache-reap-time*))
			  ;; Good to go
			  attr)
		   else ;; Not expired.  Use the cached attributes
			;;(logit "Using cached attrs")
			(nfs-attr-cache-attr attr-cache)))
	 else ;; No cached entry.  Make one.
	      (let ((attr (nfs-stat fh)))
		(setf (gethash fh *nfs-attr-cache*) 
		  (make-nfs-attr-cache 
		   :attr attr
		   :expiration (+ (excl::cl-internal-real-time) *attr-cache-reap-time*)))
		
		;; Return new attrs
		attr)))))
  
;; list of size, mtime, ctime
(defun get-pre-op-attrs (fh)
  (let ((attrs (lookup-attr fh)))
    (list (nfs-attr-size attrs) 
	  (nfs-attr-mtime attrs) 
	  (nfs-attr-ctime attrs))))

(defun pre-op-attrs-ctime (pre-op-attrs)
  (third pre-op-attrs))


(defun dump-attr-cache ()
  (mp:with-process-lock (*attr-cache-lock*)
    (maphash #'(lambda (key value)
		 (format t "~S -> ~S~%" key value))
	     *nfs-attr-cache*)))

(defun attr-cache-reaper ()
  (loop
    (sleep (max *attr-cache-reap-time* 1))
    (reap-attr-cache)))

;;; XXX --  might want to make sure that directories
;;; have their a/m/c-times updates before uncaching.. in case
;;; operations were done in a cached way.  need to think about
;;; this more.
(defun reap-attr-cache ()
  (mp:with-process-lock (*attr-cache-lock*)
    (let ((now (excl::cl-internal-real-time)))

      (maphash
       #'(lambda (key attr-cache)
	   (when (>= now (nfs-attr-cache-expiration attr-cache))
	     ;; Expired entry.  Remove it.
	     (remhash key *nfs-attr-cache*)))
       *nfs-attr-cache*))))

;; XXX -- callers to this function should make sure they've
;; written out any cached attr updates before calling.
(defun uncache-attr (fh)
  (mp:with-process-lock (*attr-cache-lock*)
    (remhash fh *nfs-attr-cache*)))


;; used when reading from a file or directory
(defun update-attr-atime (fh &optional (when (get-universal-time)))
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-atime attr) when)
    attr))

;; updates ctime as well.
;; used by directory modifying functions which don't care about size.
(defun update-atime-and-mtime (fh &optional (when (get-universal-time)))
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-atime attr) when)
    (setf (nfs-attr-mtime attr) when)
    (setf (nfs-attr-ctime attr) when)
    attr))

(ff:def-foreign-call (sys-futime "_futime") ((fd :int) (utimbuf (* :void))))

;; used by file modification functions. (i.e nfsd-write(3))
(defun update-attr-times-and-size (stream fh set-mtime)
  (if (not (open-stream-p stream))
      (error "Something passed a closed stream to update-attr-times-and-size"))
  (if set-mtime
      (sys-futime (excl.osi::stream-to-fd stream) 0))
  (let ((attr (update-atime-and-mtime fh))
	(pos (file-position stream)))
    (when (> pos (nfs-attr-size attr))
      (setf (nfs-attr-size attr) pos)
      (setf (nfs-attr-used attr) pos)
      (setf (nfs-attr-blocks attr) (howmany pos 512)))))

(defun set-cached-file-size (fh size)
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-size attr) size)
    (setf (nfs-attr-used attr) size)
    (setf (nfs-attr-blocks attr) (howmany size 512))
    (setf (nfs-attr-ctime attr) (get-universal-time))
    size))

(defun set-cached-file-atime (fh atime)
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-atime attr) atime)
    (setf (nfs-attr-ctime attr) (get-universal-time))
    atime))

(defun set-cached-file-mtime (fh mtime)
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-mtime attr) mtime)
    (setf (nfs-attr-ctime attr) (get-universal-time))
    mtime))

(defun incf-cached-nlinks (fh)
  (incf (nfs-attr-nlinks (lookup-attr fh))))
