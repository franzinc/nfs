;;; nfs
;;; $Id: nfs.cl,v 1.21 2001/08/10 18:07:55 dancy Exp $

(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

;;; these settings may be overridden by the config file
(defparameter *nfsdebug* nil)
(defparameter *nfslocaluid* 443)
(defparameter *nfslocalgid* 50)
(defparameter *nfslocalumask* #o022)

(defparameter *openfilereaptime* 2) ;; seconds
(defparameter *statcachereaptime* 5)
(defparameter *nfs-dircachereaptime* 30)

(defconstant *nfsprog* 100003)
(defconstant *nfsvers* 2)
(defconstant *nfsport* 2049)
(defconstant *maxdata* 8192)
(defconstant *maxpathlen* 1024)
(defconstant *maxnamlen* 255)
(defconstant *cookiesize* 4)
(defparameter *nfsd-tcp-socket* nil)
(defparameter *nfsd-udp-socket* nil)
(defconstant *blocksize* 512)

(defparameter *socketbuffersize* (* 128 1024))

(defparameter *nfsdxdr* nil)

#|
           NFS_OK = 0,
           NFSERR_PERM=1,
           NFSERR_NOENT=2,
           NFSERR_IO=5,
           NFSERR_NXIO=6,
           NFSERR_ACCES=13,
           NFSERR_EXIST=17,
           NFSERR_NODEV=19,
           NFSERR_NOTDIR=20,
           NFSERR_ISDIR=21,
           NFSERR_FBIG=27,
           NFSERR_NOSPC=28,
           NFSERR_ROFS=30,
           NFSERR_NAMETOOLONG=63,
           NFSERR_NOTEMPTY=66,
           NFSERR_DQUOT=69,
           NFSERR_STALE=70,
	   NFSERR_WFLUSH=99
	   |#


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

(defun map-errno-to-nfs-error-code (errno)
  (cond
   ((= errno 13) NFSERR_ACCES)
   (t NFSERR_IO)))
    
    
(defun make-nfsdsockets ()
  (unless *nfsd-tcp-socket*
    (setf *nfsd-tcp-socket*
      (socket:make-socket :type :stream
                          :connect :passive
                          :local-port *nfsport*
                          :reuse-address t))
    #+(version>= 6 1)
    (socket:set-socket-options *nfsd-tcp-socket* :receive-buffer-size *socketbuffersize* :send-buffer-size *socketbuffersize*))
  (unless *nfsd-udp-socket*
    (setf *nfsd-udp-socket*
      (socket:make-socket :type :datagram
			  :local-port *nfsport*
			  :reuse-address t))
    #+(version>= 6 1)
    (socket:set-socket-options *nfsd-udp-socket* :receive-buffer-size *socketbuffersize* :send-buffer-size *socketbuffersize*)))

  

(defun close-nfsdsockets ()
  (when *nfsd-tcp-socket*
    (close *nfsd-tcp-socket*)
    (setf *nfsd-tcp-socket* nil))
  (when *nfsd-udp-socket*
    (close *nfsd-udp-socket*)
    (setf *nfsd-udp-socket* nil)))

(defun ensure-nfsdxdr ()
  (unless *nfsdxdr*
    (setf *nfsdxdr* (create-xdr :direction :build))))

(defun nfsd ()
  (make-nfsdsockets)
  (ensure-nfsdxdr)
  (portmap-add-program *nfsprog* *nfsvers* *nfsport* IPPROTO_TCP)
  (portmap-add-program *nfsprog* *nfsvers* *nfsport* IPPROTO_UDP)
  (mp:process-run-function "open file reaper" #'nfsd-open-file-reaper)
  (mp:process-run-function "stat cache reaper" #'statcache-reaper)
  (mp:process-run-function "dir cache reaper" #'dircache-reaper)
  (let ((server (make-rpc-server :tcpsock *nfsd-tcp-socket* :udpsock *nfsd-udp-socket*)))
    (loop
      (multiple-value-bind (xdr peer)
          (rpc-get-message server)
        (nfsd-message-handler xdr peer)))))

(defun nfsd-message-handler (xdr peer)
  (let* ((msg (create-rpc-msg xdr))
	 (cbody (rpc-msg-cbody msg)))
    ;;(pprint-cbody cbody)
    (unless (= (rpc-msg-mtype msg) 0)
      (error "Unexpected data!"))
    
    ;; sanity checks first
    (if* (not (= (call-body-prog cbody) *nfsprog*))
       then
	    (format t "Sending program unavailable response for prog=~D~%" (call-body-prog cbody))
	    (rpc-send-prog-unavail peer (rpc-msg-xid msg) (nfsd-null-verf))
	    (return-from nfsd-message-handler))
    (if* (not (= (call-body-vers cbody) *nfsvers*))
       then
	    (write-line "Sending program version mismatch response")
	    (rpc-send-prog-mismatch peer (rpc-msg-xid msg) (nfsd-null-verf) *nfsvers* *nfsvers*)
	    (return-from nfsd-message-handler))
    (case (call-body-proc cbody)
      (0 (nfsd-null peer (rpc-msg-xid msg)))
      (1 (nfsd-getattr peer (rpc-msg-xid msg) (call-body-params cbody)))
      (2 (nfsd-setattr peer (rpc-msg-xid msg) cbody))
      (4 (nfsd-lookup peer (rpc-msg-xid msg) (call-body-params cbody)))
      (6 (nfsd-read peer (rpc-msg-xid msg) (call-body-params cbody)))
      (8 (nfsd-write peer (rpc-msg-xid msg) cbody))
      (9 (nfsd-create peer (rpc-msg-xid msg) cbody))
      (10 (nfsd-remove peer (rpc-msg-xid msg) cbody))
      (11 (nfsd-rename peer (rpc-msg-xid msg) cbody))
      (14 (nfsd-mkdir peer (rpc-msg-xid msg) cbody))
      (15 (nfsd-rmdir peer (rpc-msg-xid msg) cbody))
      (16 (nfsd-readdir peer (rpc-msg-xid msg) (call-body-params cbody)))
      (17 (nfsd-statfs peer (rpc-msg-xid msg) (call-body-params cbody)))
      (t 
       (rpc-send-proc-unavail peer (rpc-msg-xid msg) (nfsd-null-verf))
       (format t "nfsd: unhandled procedure ~D~%" (call-body-proc cbody))))))
    

(defparameter *nfsdnullverf* nil)

(defun nfsd-null-verf ()
  (unless *nfsdnullverf*
    (let ((xdr (create-xdr :direction :build)))
      (xdr-auth-null xdr)
      (setf *nfsdnullverf* xdr)))
  *nfsdnullverf*)


(defun nfsd-null (peer xid)
  (if *nfsdebug*  (format t "nfsd-null~%~%"))
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (nfsd-null-verf) xdr)))

(defun nfsd-getattr (peer xid params)
  (let ((p (with-xdr-xdr (params)
	     (xdr-fhandle-to-pathname params))))
    (if *nfsdebug* (format t "nfsd-getattr(~A)~%" p))
    (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
      (if (null p)
	  (xdr-int *nfsdxdr* NFSERR_STALE)
	(progn
	  (xdr-int *nfsdxdr* NFS_OK)
	  (update-fattr-from-pathname p *nfsdxdr*))))))

(defparameter *nfs-statcache* (make-hash-table :test #'equalp))
(defparameter *statcache-lock* (mp:make-process-lock))

;; sbcons..  (sb . lastaccesstime)
(defun lookup-statcache (p)
  (unless (pathnamep p)
    (error "lookup-statcache must be called with a pathname"))
  (mp:with-process-lock (*statcache-lock*)
    (let ((sbcons (gethash p *nfs-statcache*)))
      (if sbcons
	  (progn
	    (setf (cdr sbcons) (get-universal-time)) ;; update access time
	    (car sbcons))
	(progn ;; new cache entry
	  (setf sbcons (cons (ff:allocate-fobject 'stat) (get-universal-time)))
	  (stat (namestring p) (car sbcons))
	  (setf (gethash p *nfs-statcache*) sbcons)
	  (car sbcons))))))

(defun dump-statcache ()
  (mp:with-process-lock (*statcache-lock*)
    (maphash #'(lambda (key value)
		 (format t "~S -> ~S~%" key value))
	     *nfs-statcache*)))

(defun statcache-reaper ()
  (loop
    (sleep *statcachereaptime*)
    (reap-statcache)))

(defun reap-statcache ()
  (mp:with-process-lock (*statcache-lock*)
    (maphash
     #'(lambda (key value)
	 (if (> (- (get-universal-time) (cdr value)) *statcachereaptime*)
	     (remhash key *nfs-statcache*)))
     *nfs-statcache*)))
    
    

;;; how much to substract from a universal time to make it into a 
;;; unix time.  Or, how much to add to a unix time to make it a universal
;;; time
(defconstant *universal-time-to-unix-time* 2208988800)

(defun update-stat-atime (p)
  (let ((sb (lookup-statcache p)))
    (setf (ff:fslot-value-typed 'stat :foreign sb 'st_atime) (- (get-universal-time) *universal-time-to-unix-time*))
    sb))

(defun update-atime-and-mtime (p)
  (let ((sb (update-stat-atime p)))
    (setf (ff:fslot-value-typed 'stat :foreign sb 'st_mtime) (- (get-universal-time) *universal-time-to-unix-time*))
    sb))


(defun update-stat-times-and-size (f p)
  (let ((sb (update-atime-and-mtime p))
	(pos (file-position f)))
    (if (> pos (ff:fslot-value-typed 'stat :foreign sb 'st_size))
	(setf (ff:fslot-value-typed 'stat :foreign sb 'st_size) pos))))

#|
          enum ftype {
              NFNON = 0,
              NFREG = 1,
              NFDIR = 2,
              NFBLK = 3,
              NFCHR = 4,
              NFLNK = 5
          };
|#

(defconstant *NFREG* 1)
(defconstant *NFDIR* 2)

(defun make-fattr-from-pathname (p)
  (declare
   (type pathname p))
  (let* ((id (pathname-to-fhandle-id p))
	 ;;(sb (ff:allocate-fobject 'stat)))
	 (sb (lookup-statcache p)))
    ;;(stat (namestring p) sb)
    (make-fattr-xdr 
     (if (= 0 (logand (ff:fslot-value-typed 'stat :foreign sb 'st_mode) #o40000))
	 *NFREG* 
       *NFDIR*) ;; type
     (logand (ff:fslot-value-typed 'stat :foreign sb 'st_mode) (lognot *nfslocalumask*))
     (ff:fslot-value-typed 'stat :foreign sb 'st_nlink)
     *nfslocaluid* 
     *nfslocalgid* 
     (ff:fslot-value-typed 'stat :foreign sb 'st_size)
     *blocksize* ;; blocksize
     (ff:fslot-value-typed 'stat :foreign sb 'st_rdev) 
     (howmany (ff:fslot-value-typed 'stat :foreign sb 'st_size) *blocksize*) ;;blocks
     (ff:fslot-value-typed 'stat :foreign sb 'st_rdev) ;; fsid
     id ;; fileid 
     (list (ff:fslot-value-typed 'stat :foreign sb 'st_atime) 0) ;; atime
     (list (ff:fslot-value-typed 'stat :foreign sb 'st_mtime) 0) ;; mtime
     (list (ff:fslot-value-typed 'stat :foreign sb 'st_mtime) 0)))) ; ctime

(defun update-fattr-from-pathname (p xdr)
  (declare
   (type pathname p))
  (let* ((id (pathname-to-fhandle-id p))
	 (sb (lookup-statcache p)))
    (update-fattr 
     xdr
     (if (= 0 (logand (ff:fslot-value-typed 'stat :foreign sb 'st_mode) #o40000))
	 *NFREG* 
       *NFDIR*) ;; type
     (logand (ff:fslot-value-typed 'stat :foreign sb 'st_mode) (lognot *nfslocalumask*)) ;; mode
     (ff:fslot-value-typed 'stat :foreign sb 'st_nlink) ;;nlink
     *nfslocaluid* ;; uid
     *nfslocalgid* ;; gid
     (ff:fslot-value-typed 'stat :foreign sb 'st_size) ;;size 
     ;; changed to 8192 to work around linux's weird readdir stuff
     8192 ;; *blocksize* ;; blocksize  
     (ff:fslot-value-typed 'stat :foreign sb 'st_rdev)  ;; rdev
     (howmany (ff:fslot-value-typed 'stat :foreign sb 'st_size) *blocksize*) ;;blocks
     (ff:fslot-value-typed 'stat :foreign sb 'st_rdev) ;; fsid
     id ;; fileid 
     (list (ff:fslot-value-typed 'stat :foreign sb 'st_atime) 0) ;; atime
     (list (ff:fslot-value-typed 'stat :foreign sb 'st_mtime) 0) ;; mtime
     (list (ff:fslot-value-typed 'stat :foreign sb 'st_mtime) 0) ; ctime
     )
    ))



(defun make-attrstat (stat attributes)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr stat)
    (if (= stat 0) ;; NFS_OK
        (xdr-xdr xdr attributes))
    xdr))


(defstruct fattr
  type;
  mode;
  nlink;
  uid;
  gid;
  size;
  blocksize;
  rdev;
  blocks;
  fsid;
  fileid;
  atime;
  mtime;
  ctime;
  )

(defun make-fattr-xdr (type mode nlink uid gid size blocksize rdev blocks fsid fileid atime mtime ctime)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr type)
    (xdr-int xdr mode)
    (xdr-int xdr nlink)
    (xdr-int xdr uid)
    (xdr-int xdr gid)
    (xdr-int xdr size)
    (xdr-int xdr blocksize)
    (xdr-int xdr rdev)
    (xdr-int xdr blocks)
    (xdr-int xdr fsid)
    (xdr-int xdr fileid)
    (xdr-timeval xdr atime)
    (xdr-timeval xdr mtime)
    (xdr-timeval xdr ctime)
    xdr))

;; 68 bytes
(defun update-fattr (xdr type mode nlink uid gid size blocksize rdev blocks fsid fileid atime mtime ctime)
  (xdr-int xdr type)
  (xdr-int xdr mode)
  (xdr-int xdr nlink)
  (xdr-int xdr uid)
  (xdr-int xdr gid)
  (xdr-int xdr size)
  (xdr-int xdr blocksize)
  (xdr-int xdr rdev)
  (xdr-int xdr blocks)
  (xdr-int xdr fsid)
  (xdr-int xdr fileid)
  (xdr-timeval xdr atime)
  (xdr-timeval xdr mtime)
  (xdr-timeval xdr ctime))



(defun make-statfsres (stat info)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr stat)
    (if (= stat 0) ;; NFS_OK
        (xdr-xdr xdr info))
    xdr))

(defun make-fsinfo-xdr (p)
  (let ((xdr (create-xdr :direction :build))
	freeblocks
	totalblocks)
    (multiple-value-bind (freebytes totalbytes)
        (diskfree p)
      (setf freeblocks (howmany freebytes *blocksize*))
      (setf totalblocks (howmany totalbytes *blocksize*))
      (xdr-unsigned-int xdr *maxdata*)
      (xdr-unsigned-int xdr *blocksize*)
      (xdr-unsigned-int xdr totalblocks)
      (xdr-unsigned-int xdr freeblocks)
      (xdr-unsigned-int xdr freeblocks))
    xdr))
    

(defun nfsd-statfs (peer xid params)
  (let ((p (with-xdr-xdr (params) (xdr-fhandle-to-pathname params))))
    (if *nfsdebug*
	(format t "nfsd-statfs(~A)~%" p))
    (if (null p)
	(let ((xdr (create-xdr :direction :build)))
	  (xdr-int xdr NFSERR_STALE)
	  (send-successful-reply peer xid (nfsd-null-verf) xdr))
      (send-successful-reply peer xid (nfsd-null-verf) 
			     (make-statfsres 0 (make-fsinfo-xdr p))))))

(defun canonicalize-dir (dir)
  (setf dir (namestring dir))
  (if (not (= (position #\\ dir :from-end t) (1- (length dir))))
      (setf dir (concatenate 'string dir "\\")))
  dir)

;;; readdirargs:  fhandle dir, cookie, count

;; perhaps directory listings should be cached?
(defun nfsd-readdir (peer xid params)
  (with-xdr-xdr (params)
    (let ((p (xdr-fhandle-to-pathname params))
	  (cookie (xdr-unsigned-int params))
	  (count (xdr-unsigned-int params)))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null p)
	    (xdr-int *nfsdxdr* NFSERR_STALE)	  
	  (progn
	    (if *nfsdebug* (format t "nfsd-readdir(~A, count ~D, cookie ~S)~%" p count cookie))
	    (add-direntries *nfsdxdr* p count cookie)))))))


;; entries look like (dirlist . atime)
(defparameter *nfs-dircache* (make-hash-table :test #'equalp))
(defparameter *nfs-dircachelock* (mp:make-process-lock))

(defun dump-nfsdircache ()
  (maphash #'(lambda (key value) (format t "~S -> ~S~%" key value)) *nfs-dircache*))

(defun dircache-reaper ()
  (loop
    (sleep *nfs-dircachereaptime*)
    (reap-dircache)))

(defun reap-dircache ()
  (mp:with-process-lock (*nfs-dircachelock*)
    (maphash
     #'(lambda (key value)
	 (if (> (- (get-universal-time) (cdr value)) *nfs-dircachereaptime*)
	     (remhash key *nfs-dircache*)))
     *nfs-dircache*)))

#|
(defun augmented-directory (dir)
  (append (list
	   (pathname (concatenate 'string dir "."))
	   (pathname (concatenate 'string dir "..")))
	  (directory dir)))
|#

(defun augmented-directory (dir)
  (directory dir))
   

(defun nfs-lookup-dir (dir)
  (mp:with-process-lock (*nfs-dircachelock*)
    (setf dir (canonicalize-dir dir))
    (let ((dc (gethash dir *nfs-dircache*)))
      (if (null dc)
	  (progn
	    (setf dc (cons (augmented-directory dir) nil))
	    (setf (gethash dir *nfs-dircache*) dc)))
      (setf (cdr dc) (get-universal-time))
      (car dc))))

;;; doesn't add duplicates
(defun nfs-add-file-to-dircache (file dir)
  (mp:with-process-lock (*nfs-dircachelock*)
    (setf file (pathname file))
    (let ((dirlist (nfs-lookup-dir dir)))
      (if (not (member file dirlist :test #'equalp))
	  (let ((pos (position nil dirlist)))
	    (if (null pos)
		(rplacd (last dirlist) (list file))
	      (setf (nth pos dirlist) file)))))))

(defun nfs-remove-file-from-dircache (file dir)
  (mp:with-process-lock (*nfs-dircachelock*)
    (setf file (pathname file))
    (let* ((dirlist (nfs-lookup-dir dir))
	   (cons (member file dirlist :test #'equalp)))
      (if cons
	  (rplaca cons nil)))))

    
(defun add-direntries (xdr dir max startindex)
  (block nil
    (let ((dirlist (handler-case (nfs-lookup-dir dir)
		     (file-error (c)
		       (case (excl::file-error-errno c)
			 (5 
			  (format t "add-direntries: Handling I/O error while reading directory ~A~%" dir)
			  (xdr-int xdr NFSERR_IO)
			  (return :err))
			 (t
			  (error c))))))
	  (totalbytesadded 8) ;; starts at 8 because in the minimal case, we need to be able to add the final 0 discriminant.. and the eof indicator
	  bytesadded)
      
    (xdr-int xdr NFS_OK)

    (let ((index startindex)
	  (endindex (length dirlist))
	  p)
      (declare 
       (type fixnum index)
       (type fixnum endindex)
       (type pathname p))

      ;;(format t "startindex: ~D  dirlistlen: ~D~%" startindex endindex)
      
      (loop
	(if (>= index endindex) ;; end of directory
	    (progn
	      (xdr-int xdr 0) ;; no more entries
	      (xdr-int xdr 1) ;; EOF
	      (if *nfsdebug* (format t "Reached end of entries~%~%"))
	      (return)))

	(setf p (nth index dirlist))

	(if p
	    (progn
	      (setf bytesadded (make-direntry-xdr xdr p (1+ index)))
	      (incf totalbytesadded bytesadded)
	      ;;(if (eq *nfsdebug* :verbose) (format t "totalbytesadded: ~D~%" totalbytesadded))
	      (if (> totalbytesadded max)
		  (progn
		    (if (eq *nfsdebug* :verbose) 
			(format t " [not added due to size overflow]~%"))
		    (if *nfsdebug* (format t "add-direntries: stopping at entry for ~A due to size reasons.~%" p))
		    (xdr-backspace xdr bytesadded)
		    (xdr-int xdr 0) ;; no more entries
		    (xdr-int xdr 0) ;; not EOF
		    (return)))))
	(if (eq *nfsdebug* :verbose) (format t " [added]~%"))

	(incf index))))))

#|
struct entry {
                   unsigned fileid;
                   filename name;
                   nfscookie cookie;
                   entry *nextentry;
           };
|#

;; returns number of bytes added to xdr
(defun make-direntry-xdr (xdr p cookie)
  (declare 
   (type pathname p))
  (let ((fileid (pathname-to-fhandle-id p))
	(filename (basename p)))
    (when (eq *nfsdebug* :verbose) (format t "~D: ~A fileid=~A next=~A" (1- cookie) filename fileid cookie))
    (xdr-compute-bytes-added (xdr)
			     (xdr-int xdr 1) ;; indicate that data follows
			     (xdr-unsigned-int xdr fileid)
			     (xdr-string xdr filename)
			     (xdr-int xdr cookie))))

;;; returns a pathname (or nil if the pathname was illegal)
(defun add-filename-to-dirname (dir filename)
  (setf dir (namestring dir))
  (if (not (= (position #\\ dir :from-end t) (1- (length dir))))
      (setf dir (concatenate 'string dir "\\")))
  (ignore-errors (pathname (concatenate 'string dir filename))))

;;; returns:
;;; status
;;; fhandle
;;; attributes (fattr)
(defun nfsd-lookup (peer xid params)
  (with-xdr-xdr (params)
    (let* ((dir (xdr-fhandle-to-pathname params))
	   (filename (xdr-string params))
	   newpath)
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null dir)
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (progn
	    (setf newpath (add-filename-to-dirname dir filename))
	    (if *nfsdebug* (format t "nfsd-lookup(~A)~%" newpath))
	    (if (or (null newpath) (not (probe-file newpath)))
		(xdr-int *nfsdxdr* NFSERR_NOENT)
	      (progn
		(xdr-int *nfsdxdr* NFS_OK)
		(pathname-to-fhandle-with-xdr *nfsdxdr* newpath)
		(update-fattr-from-pathname newpath *nfsdxdr*)))))))))
	      

#|
struct readargs {
                   fhandle file;
                   unsigned offset;
                   unsigned count;
                   unsigned totalcount;
		   };
		   |#

(defparameter *nfs-openfilelist* nil)

(defstruct openfile
  pathname
  direction
  stream
  lastaccess)

(defparameter *nfs-openfilelist-lock* (mp:make-process-lock))

(defun get-open-file (pathname direction)
  (handler-case 
      (mp:with-process-lock (*nfs-openfilelist-lock*)
	(let* ((newof (make-openfile :pathname pathname
				     :direction direction))
	       (oldof (locate-open-file newof)))
	  (if oldof
	      (progn
		(setf (openfile-lastaccess oldof) (get-universal-time))
		(openfile-stream oldof))
	    (progn
	      (setf (openfile-stream newof)
		(if (eq direction :input)
		    (open pathname :direction :input) ;; need error checking
		  (open pathname :direction :output
			:if-exists :overwrite)))
	      (setf (openfile-lastaccess newof) (get-universal-time))
	      (push newof *nfs-openfilelist*)
	      ;;(format t "open file list has ~d entries now~%" (length *nfs-openfilelist*))
	      (openfile-stream newof)))))
    (file-error (c)
      (values nil (excl::file-error-errno c)))))


(defun locate-open-file (of)
  (find of *nfs-openfilelist*
	:test #'(lambda (a b)
		  (and (equalp (openfile-pathname a) (openfile-pathname b))
		       (eq (openfile-direction a) (openfile-direction b))))))

(defun locate-open-file-any (p)
  (find p *nfs-openfilelist*
	:test #'(lambda (a b) (equalp a (openfile-pathname b)))))

(defun close-open-file (p)
  (setf p (pathname p))
  ;;(format t "close-open-file ~S~%" p)
  (mp:with-process-lock (*nfs-openfilelist-lock*)
    (let (of)
      (while (setf of (locate-open-file-any p))
	     (setf (openfile-lastaccess of) 0)
	     (format t "close-open-file: Calling reap-open-files to effect a close~%")
	     (reap-open-files)))))
	     

(defun reap-open-files ()
  (mp:with-process-lock (*nfs-openfilelist-lock*)
    (let ((res nil)
	  (now (get-universal-time)))
      (dolist (of *nfs-openfilelist*)
	(if (< (- now (openfile-lastaccess of)) *openfilereaptime*)
	    (push of res)
	  (progn
	    ;;(format t "reap-open-files: Closing ~S~%" (openfile-pathname of))
	    (close (openfile-stream of))))
	(setf *nfs-openfilelist* res)))))

(defun nfsd-open-file-reaper ()
  (loop
    (sleep *openfilereaptime*)
    (reap-open-files)))

(defun nfsd-read (peer xid params)
  (with-xdr-xdr (params)
    (let* ((p (xdr-fhandle-to-pathname params))
	 (offset (xdr-unsigned-int params))
	 (count (xdr-unsigned-int params))
	 ;;(totalcount (xdr-unsigned-int readargs))  ;; unused
	 )
    (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
      (if (eq *nfsdebug* :verbose) (format t "nfsd-read(~A, offset=~D, count=~D)~%"  p offset count))
      (if (null p)
	  (progn
	    (xdr-int *nfsdxdr* NFSERR_STALE))
	(multiple-value-bind (f errno)
	    (get-open-file p :input)
	  (if (null f)
	      (progn
		(xdr-int *nfsdxdr* (map-errno-to-nfs-error-code errno))
		)
	    (progn
	      (file-position f offset)
	      (xdr-with-seek (*nfsdxdr* 72) (xdr-opaque-variable-from-stream *nfsdxdr* f count))
	      (xdr-int *nfsdxdr* NFS_OK) 
	      (update-stat-atime p)
	      (update-fattr-from-pathname p *nfsdxdr*)
	      ))))))))

;;; args:  fhandle dir, filename, sattr
;;; returns: status
;;   fhandle, attributes (if okay status)
(defun nfsd-create (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((dir (xdr-fhandle-to-pathname params))
	  (filename (xdr-string params))
	  (sattr (xdr-sattr-to-struct-sattr params)))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null dir)
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (let ((newpath (add-filename-to-dirname dir filename)))
	    (if *nfsdebug* (format t "nfsd-create( ~A with attributes ~A~%" newpath sattr))
	    (if (or (null newpath) (not (nfs-okay-to-write (call-body-cred cbody))))
		(progn
		  (format t "permission denied~%") 
		  (xdr-int *nfsdxdr* NFSERR_ACCES))
	      ;; okay to write
	      (if (probe-file newpath)
		  (xdr-int *nfsdxdr* NFSERR_EXIST)
		(let ((f (handler-case (open newpath :direction :output)
			   (file-error (c)
			     (cond 
			      ((= (excl::file-error-errno c) 22)
			       (xdr-int *nfsdxdr* NFSERR_ACCES)
			       :err)
			      (t (error c)))))))
		  (if (not (eq f :err)) 
		      (progn
			(close f)
			(update-atime-and-mtime dir)
			(nfs-add-file-to-dircache newpath dir)
			(xdr-int *nfsdxdr* NFS_OK)
			(pathname-to-fhandle-with-xdr *nfsdxdr* newpath)
			(update-fattr-from-pathname newpath *nfsdxdr*))))))))))))

  
#|
          struct sattr {
              unsigned int mode;
              unsigned int uid;
              unsigned int gid;
              unsigned int size;
              timeval      atime;
              timeval      mtime;
	      };
	      |#

(defstruct sattr
  mode
  uid
  gid
  size
  atime
  mtime)

(defun xdr-sattr-to-struct-sattr (xdr)
  (make-sattr
   :mode (xdr-unsigned-int xdr)
   :uid (xdr-unsigned-int xdr)
   :gid (xdr-unsigned-int xdr)
   :size (xdr-unsigned-int xdr)
   :atime (xdr-timeval xdr)
   :mtime (xdr-timeval xdr)))

;; args:
;; fhandle dir
;; filename name

;; returns status
(defun nfsd-remove (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((dir (xdr-fhandle-to-pathname params))
	  (filename (xdr-string params))
	  newpath)
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null dir)
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (progn
	    (setf newpath (add-filename-to-dirname dir filename))
	    (if *nfsdebug* (format t "nfsd-remove(~A)~%" newpath))
	    (if (null newpath)
		(xdr-int *nfsdxdr* NFSERR_NOENT)
	      (if (nfs-okay-to-write (call-body-cred cbody))
		  (progn
		    (close-open-file newpath)
		    (handler-case (excl::filesys-delete-file (namestring newpath))
		      (file-error (c)
			(let ((errno (excl::file-error-errno c)))
			  (if (numberp errno)
			      (cond
			       ((= 2 errno)
				(format t "delete: no such file or directory~%")
				(xdr-int *nfsdxdr* NFSERR_NOENT))
			       ((= 13 errno)
				(format t "delete: permission denied~%")
				(xdr-int *nfsdxdr* NFSERR_ACCES))
			       (t 
				(error c)))
			    (error c))))
		      (:no-error (c)
			(declare (ignore c))
			;;(format t "delete succeeded.~%")
			(update-atime-and-mtime dir)
			(nfs-remove-file-from-dircache newpath dir)
			(xdr-int *nfsdxdr* NFS_OK))))
		(progn
		  (format t "delete: auth denied~%")
		  (xdr-int *nfsdxdr* NFSERR_ACCES))))))))))

    
;;; returns:
;;; status
;;; attributes (if no error)
(defun nfsd-write (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name xdr)
    (let ((p (xdr-fhandle-to-pathname xdr))
	  (beginoffset (xdr-unsigned-int xdr)) ;; not used
	  (offset (xdr-unsigned-int xdr))
	  (totalcount (xdr-unsigned-int xdr)) ;; not used
	  (data (xdr-opaque-variable xdr)))
      (declare (ignore beginoffset totalcount))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null p)
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (progn 
	    (if (eq *nfsdebug* :verbose) (format t "nfsd-write(~A,offset=~A) ~D bytes~%" p offset (third data)))
	    (if (nfs-okay-to-write (call-body-cred cbody))
		(let ((f (get-open-file p :output)))
		  (file-position f offset)
		  (write-sequence 
		   (xdr-vec (first data))
		   f
		   :start (second data)
		   :end (+ (third data) (second data)))
		  (update-stat-times-and-size f p)
		  (xdr-int *nfsdxdr* NFS_OK)
		  (update-fattr-from-pathname p *nfsdxdr*))
	      (progn
		(format t "nfsd-write: permission denied~%")
		(xdr-int *nfsdxdr* NFSERR_ACCES)))))))))



;;; this is often used to truncate files (if the size parameters is
;;; not 0xffffffff)
(defun nfsd-setattr (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name xdr)
  (let* ((p (xdr-fhandle-to-pathname xdr))
	 (sattr (xdr-sattr-to-struct-sattr xdr)))
    (if *nfsdebug*
	(format t "nfsd-setattr(~A ~A)~%" p  sattr))
    (if (nfs-okay-to-write (call-body-cred cbody))
	(progn
	  (if (not (= (sattr-size sattr) #xffffffff))
	      (truncate-file (namestring p) (sattr-size sattr))) ;; need error checking
	  (send-successful-reply 
	   peer xid 
	   (nfsd-null-verf) 
	   (make-attrstat NFS_OK (make-fattr-from-pathname p))))
      (send-successful-reply peer xid (nfsd-null-verf)
			     (make-attrstat NFSERR_ACCES nil))))))
    

;;; from:  fhandle dir, filename name
;;; to:    fhandle dir, filename name
(defun nfsd-rename (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((fromdir (xdr-fhandle-to-pathname params))
	  (fromfilename (xdr-string params))
	  (todir (xdr-fhandle-to-pathname params))
	  (tofilename (xdr-string params)))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (or (null fromdir) (null todir))
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (let ((from (add-filename-to-dirname fromdir fromfilename))
		(to (add-filename-to-dirname todir tofilename)))
	    (if *nfsdebug* (format t "nfsd-rename(~A -> ~A~%)" from to))
	    (cond 
	     ((null from)
	      (xdr-int *nfsdxdr* NFSERR_NOENT))
	     ((or (null to) (not (nfs-okay-to-write (call-body-cred cbody))))
	      (xdr-int *nfsdxdr* NFSERR_ACCES))
	     (t 
	      (rename-file from (merge-pathnames to (make-pathname :type :unspecific))) ;; need error checking
	      (swap-fhandles from to)
	      (nfs-remove-file-from-dircache from fromdir)
	      (nfs-add-file-to-dircache to todir)
	      (xdr-int *nfsdxdr* NFS_OK)))))))))

;;; args:  fhandle dir, filename, sattr
;;; returns: status
;;   fhandle, attributes (if okay status)
(defun nfsd-mkdir (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((dir (xdr-fhandle-to-pathname params))
	  (filename (xdr-string params))
	  (sattr (xdr-sattr-to-struct-sattr params)))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null dir)
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (let ((newpath (add-filename-to-dirname dir filename)))
	    (if *nfsdebug* (format t "nfsd-mkdir( ~A with attributes ~A~%" newpath sattr))
	    (if (or (null newpath) (not (nfs-okay-to-write (call-body-cred cbody))))
		(progn
		  (format t "permission denied~%")
		  (xdr-int *nfsdxdr* NFSERR_ACCES))
	      ;; okay to write
	      (if (probe-file newpath)
		  (xdr-int *nfsdxdr* NFSERR_EXIST)
		;; doesn't exist.. do the work
		(progn
		  ;; need error checking here
		  (make-directory newpath)
		  ;;(format t "mkdir: newpath is ~S~%" newpath)
		  (nfs-add-file-to-dircache newpath dir)
		  (update-atime-and-mtime dir)
		  (xdr-int *nfsdxdr* NFS_OK)
		  (pathname-to-fhandle-with-xdr *nfsdxdr* newpath)
		  (update-fattr-from-pathname newpath *nfsdxdr*))))))))))


(defun nfsd-rmdir (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((dir (xdr-fhandle-to-pathname params))
	  (filename (xdr-string params)))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null dir)
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (let ((newpath (add-filename-to-dirname dir filename)))
	    (if *nfsdebug* (format t "nfds-rmdir(~A)~%" newpath))
	    (if (null newpath)
		(xdr-int *nfsdxdr* NFSERR_NOENT)
	      (if (nfs-okay-to-write (call-body-cred cbody))
		  (if (probe-file newpath) 
		      (progn
			(handler-case (excl::rmdir newpath)
			  (file-error (c)
			    (cond
			     ((= 13 (excl::file-error-errno c))
			      (xdr-int *nfsdxdr* NFSERR_ACCES))
			     ((= 41 (excl::file-error-errno c))
			      (xdr-int *nfsdxdr* NFSERR_NOTEMPTY))
			     (t
			      (error c))))
			  (:no-error (c)
			    (declare (ignore c))
			    (if *nfsdebug* (format t "rmdir succeeded~%"))
			    (update-atime-and-mtime dir)
			    (nfs-remove-file-from-dircache newpath dir)
			    (xdr-int *nfsdxdr* NFS_OK))))
		    (xdr-int *nfsdxdr* NFSERR_NOENT))
		(xdr-int *nfsdxdr* NFSERR_ACCES)))))))))

;; should also check the file permissions..
(defun nfs-okay-to-write (cred)
  (if (= 1 (opaque-auth-flavor cred))
      (let ((au (xdr-opaque-auth-struct-to-auth-unix-struct cred)))
	(= (auth-unix-uid au) *nfslocaluid*))
    nil))
	  
