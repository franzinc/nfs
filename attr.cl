(in-package :user)

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
  atime
  mtime
  ctime)

;; should always be larger than *openfilereaptime*.
(defparameter *attr-cache-reap-time* 5) 

;; keys are file handles
;; bug14795/bug14692 -- Change this back to an eq hash table when fixed.
(defparameter *nfs-attr-cache* (make-hash-table :test #'equal))
(defparameter *attr-cache-lock* (mp:make-process-lock))

(defun stat-mode-to-type (mode)
  (ecase (logand mode #o0170000)
    (#o0040000
     *NFDIR*)
    (#o0100000
     *NFREG*)))

(defun nfs-attr (fh)
  (let ((s (stat (fh-pathname fh)))
	(blocksize 512))
    (make-nfs-attr
     :type (stat-mode-to-type (stat-mode s))
     :mode (stat-mode s)
     :nlinks 1 ;; simulated.  Perhaps at least '2' should be reported for directories.
     :uid (stat-uid s)
     :gid (stat-gid s)
     :size (stat-size s)
     :blocksize blocksize
     :used (stat-size s)
     :blocks (howmany (stat-size s) blocksize)
     :fsid (nfs-export-id (fh-export fh))
     :fileid (fh-id fh)
     :atime (stat-atime s)
     :mtime (stat-mtime s)
     :ctime (stat-ctime s))))

(defstruct nfs-attr-cache 
  attr
  atime)

(defun lookup-attr (fh)
  (mp:with-process-lock (*attr-cache-lock*)
    (let ((attr-cache (gethash fh *nfs-attr-cache*)))
      (if* attr-cache
	 then
	      ;; update access time
	      (setf (nfs-attr-cache-atime attr-cache) (get-universal-time)) 
	 else ;; new cache entry
	      (setf attr-cache 
		(make-nfs-attr-cache 
		 :attr (nfs-attr fh)
		 :atime (get-universal-time)))
	      (setf (gethash fh *nfs-attr-cache*) attr-cache))
      ;; return results
      (nfs-attr-cache-attr attr-cache))))

;; list of size, mtime, ctime
(defun get-pre-op-attrs (fh)
  (let ((attrs (lookup-attr fh)))
    (list (nfs-attr-size attrs) 
	  (nfs-attr-mtime attrs) 
	  (nfs-attr-ctime attrs))))

(defun dump-attr-cache ()
  (mp:with-process-lock (*attr-cache-lock*)
    (maphash #'(lambda (key value)
		 (format t "~S -> ~S~%" key value))
	     *nfs-attr-cache*)))

(defun attr-cache-reaper ()
  (loop
    (sleep *attr-cache-reap-time*)
    (reap-attr-cache)))

;;; XXX --  might want to make sure that directories
;;; have their a/m/c-times updates before uncaching.. in case
;;; operations were done in a cached way.  need to think about
;;; this more.
(defun reap-attr-cache ()
  (mp:with-process-lock (*attr-cache-lock*)
    (maphash
     #'(lambda (key attr-cache)
	 (if (> (- (get-universal-time) 
		   (nfs-attr-cache-atime attr-cache))
		*attr-cache-reap-time*)
	     (remhash key *nfs-attr-cache*)))
     *nfs-attr-cache*)))

;; XXX -- callers to this function should make sure they've
;; written out any cached attr updates before calling.
(defun uncache-attr (fh)
  (mp:with-process-lock (*attr-cache-lock*)
    (remhash fh *nfs-attr-cache*)))


;; used when reading from a file or directory
(defun update-attr-atime (fh)
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-atime attr) (get-universal-time))
    attr))

;; used by things that update inode information.
(defun update-attr-ctime (fh)
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-ctime attr) (get-universal-time))
    attr))

;; updates ctime as well.
;; used by directory modifying functions which don't care about size.
(defun update-atime-and-mtime (fh) 
  (let ((attr (update-attr-atime fh)))
    (setf (nfs-attr-mtime attr) (get-universal-time))
    (update-attr-ctime fh)))
    
;; used by file modification functions.
(defun update-attr-times-and-size (stream fh)
  (if (not (open-stream-p stream))
      (error "Something passed a closed stream to update-attr-times-and-size"))
  (let ((attr (update-atime-and-mtime fh))
	(pos (file-position stream)))
    (if (> pos (nfs-attr-size attr))
	(setf (nfs-attr-size attr) pos))))

(defun set-cached-file-size (fh size)
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-size attr) size)
    (update-attr-ctime fh)))

(defun set-cached-file-atime (fh atime)
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-atime attr) atime)
    (update-attr-ctime fh)))

(defun set-cached-file-mtime (fh mtime)
  (let ((attr (lookup-attr fh)))
    (setf (nfs-attr-mtime attr) mtime)
    (update-attr-ctime fh)))


