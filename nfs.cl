;;; nfs
;;; $Id: nfs.cl,v 1.5 2001/05/23 16:51:40 layer Exp $

(in-package :user)

;;; these settings may be overridden by the config file
(defparameter *nfsdebug* nil)
(defparameter *nfslocaluid* 443)
(defparameter *nfslocalgid* 50)
(defparameter *nfslocalumask* #o022)

(defparameter *nfsconfigfile* "nfs.cfg")

(defconstant *nfsprog* 100003)
(defconstant *nfsvers* 2)
(defconstant *nfsport* 2049)
(defconstant *maxdata* 8192)
(defconstant *maxpathlen* 1024)
(defconstant *maxnamlen* 255)
(defconstant *cookiesize* 4)
(defparameter *nfsd-tcp-socket* nil)
(defparameter *nfsd-udp-socket* nil)

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

    
    
(defun make-nfsdsockets ()
  (unless *nfsd-tcp-socket*
    (setf *nfsd-tcp-socket*
      (socket:make-socket :type :stream
                          :connect :passive
                          :local-port *nfsport*
                          :reuse-address t)))
    (unless *nfsd-udp-socket*
    (setf *nfsd-udp-socket*
      (socket:make-socket :type :datagram
                          :local-port *nfsport*
                          :reuse-address t))))

  

(defun close-nfsdsockets ()
  (when *nfsd-tcp-socket*
    (close *nfsd-tcp-socket*)
    (setf *nfsd-tcp-socket* nil))
  (when *nfsd-udp-socket*
    (close *nfsd-udp-socket*)
    (setf *nfsd-udp-socket* nil)))


(defun nfsd ()
  (read-nfs-cfg)
  (make-nfsdsockets)
  (portmap-add-program *nfsprog* *nfsvers* *nfsport* IPPROTO_TCP)
  (portmap-add-program *nfsprog* *nfsvers* *nfsport* IPPROTO_UDP)
  (mp:process-run-function "open file reaper" #'nfsd-open-file-reaper)
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
    (if (and (= (call-body-prog cbody) *nfsprog*)
	     (= (call-body-vers cbody) *nfsvers*))
	(case (call-body-proc cbody)
	  (0 (nfsd-null peer (rpc-msg-xid msg)))
	  (1 (nfsd-getattr peer (rpc-msg-xid msg) cbody))
	  (2 (nfsd-setattr peer (rpc-msg-xid msg) cbody))
	  (4 (nfsd-lookup peer (rpc-msg-xid msg) cbody))
	  (6 (nfsd-read peer (rpc-msg-xid msg) cbody))
	  (8 (nfsd-write peer (rpc-msg-xid msg) cbody))
	  (9 (nfsd-create peer (rpc-msg-xid msg) cbody))
	  (10 (nfsd-remove peer (rpc-msg-xid msg) cbody))
	  (11 (nfsd-rename peer (rpc-msg-xid msg) cbody))
	  (14 (nfsd-mkdir peer (rpc-msg-xid msg) cbody))
	  (15 (nfsd-rmdir peer (rpc-msg-xid msg) cbody))
	  (16 (nfsd-readdir peer (rpc-msg-xid msg) cbody))
	  (17 (nfsd-statfs peer (rpc-msg-xid msg) cbody))
	  (t 
	   (rpc-send-proc-unavail peer (rpc-msg-xid msg) (nfsd-null-verf))
	   (format t "nfsd: unhandled procedure ~D~%" (call-body-proc cbody))))
      ;; we don't know about this program that it's asking for
      (progn
	(format t "Sending program unavailable response.~%")
	(rpc-send-prog-unavail peer (rpc-msg-xid msg) (nfsd-null-verf))))))

(defun nfsd-null-verf ()
  (let ((xdr (create-xdr :direction :build)))
    (xdr-auth-null xdr)
    xdr))

(defun nfsd-null (peer xid)
  (if *nfsdebug*
      (format t "nfsd-null called.~%"))
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (nfsd-null-verf) xdr)))

(defun nfsd-getattr (peer xid cbody)
  (let* ((fhandle (call-body-params cbody))
         (p (fhandle-to-pathname fhandle)))
    (if *nfsdebug*
	(format t "nfsd-getattr(~A)~%" p))
    (send-successful-reply peer xid (nfsd-null-verf) 
                           (make-attrstat 0 (make-fattr-from-pathname p)))))
                           

(defun make-fattr-from-pathname (p)
  (let* ((id (second (multiple-value-list (pathname-to-fhandle p))))
         (isdir (directoryp p))    
         (sb (ff:allocate-fobject 'stat)))
    (stat (namestring p) sb)
    (make-fattr-xdr 
     (if isdir 2 1) ;; type
     (logand (ff:fslot-value sb 'st_mode) (lognot *nfslocalumask*))
     (ff:fslot-value sb 'st_nlink)
     *nfslocaluid* ;;(ff:fslot-value sb 'st_uid)
     *nfslocalgid* ;;(ff:fslot-value sb 'st_gid)
     (ff:fslot-value sb 'st_size)
     1024 ;; blocksize
     (ff:fslot-value sb 'st_rdev) 
     (howmany (ff:fslot-value sb 'st_size) 1024) ;;blocks
     (ff:fslot-value sb 'st_rdev) ;; fsid
     id ;; fileid 
     (list (ff:fslot-value sb 'st_atime) 0) ;; atime
     (list (ff:fslot-value sb 'st_mtime) 0) ;; mtime
     (list (ff:fslot-value sb 'st_mtime) 0)))) ; ctime

(defun make-attrstat (stat attributes)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr stat)
    (if (= stat 0) ;; NFS_OK
        (xdr-xdr xdr attributes))
    xdr))

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


(defun make-statfsres (stat info)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr stat)
    (if (= stat 0) ;; NFS_OK
        (xdr-xdr xdr info))
    xdr))

(defun make-fsinfo-xdr (p)
  (let ((xdr (create-xdr :direction :build)))
    (multiple-value-bind (blocksize freeblocks totalblocks)
        (diskfree p)
      (xdr-unsigned-int xdr (* 16 1024))
      (xdr-unsigned-int xdr blocksize)
      (xdr-unsigned-int xdr totalblocks)
      (xdr-unsigned-int xdr freeblocks)
      (xdr-unsigned-int xdr freeblocks))
    xdr))
    

(defun nfsd-statfs (peer xid cbody)
  (let* ((fhandle (call-body-params cbody))
         (p (fhandle-to-pathname fhandle)))
    (if *nfsdebug*
	(format t "nfsd-statfs(~A)~%" p))
    (send-successful-reply peer xid (nfsd-null-verf) 
     (make-statfsres 0 (make-fsinfo-xdr p)))))

(defstruct readdirargs
  fhandle
  cookie
  count)

(defun xdr-to-readdirargs (xdr)
  (let ((fhandle (xdr-opaque-fixed xdr :len *fhsize*))
        (cookie (xdr-opaque-fixed xdr :len *cookiesize*))
        (count (xdr-unsigned-int xdr)))
    (make-readdirargs
     :fhandle (create-xdr :vec fhandle)
     :cookie (create-xdr :vec cookie)
     :count count)))


(defun canonicalize-dir (dir)
  (setf dir (namestring dir))
  (if (not (= (position #\\ dir :from-end t) (1- (length dir))))
      (setf dir (concatenate 'string dir "\\")))
  dir)

(defun nfsd-readdir (peer xid cbody)
  (let* ((rda (xdr-to-readdirargs (call-body-params cbody)))
         (p (fhandle-to-pathname (readdirargs-fhandle rda)))
         (dir (directory (canonicalize-dir p)) ))
    (if *nfsdebug*
	(format t "nfsd-readdir(~A, count ~D, cookie ~S)~%" p (readdirargs-count rda) (readdirargs-cookie rda)))
    (if p
        (multiple-value-bind (xdr eof)
            (add-direntries dir
                            (readdirargs-count rda) 
                            (xdr-int (readdirargs-cookie rda)))
          (send-successful-reply peer xid 
                                 (nfsd-null-verf) 
                                 (make-readdirres-xdr NFS_OK (make-readdirok-xdr xdr eof))))
      (send-successful-reply peer xid (nfsd-null-verf) 
                            (make-readdirres-xdr NFSERR_STALE nil))))) 

          

#|
struct entry {
                   unsigned fileid;
                   filename name;
                   nfscookie cookie;
                   entry *nextentry;
           };
|#

(defun make-direntry-xdr (p cookie)
  (let ((fileid (second (multiple-value-list (pathname-to-fhandle p))))
        (filename (basename p))
        (xdr (create-xdr :direction :build)))
    (when *nfsdebug*
      (format t "~A ~A ~A~%" filename fileid cookie))
    (xdr-int xdr 1) ;; indicate that data follows
    (xdr-unsigned-int xdr fileid)
    (xdr-string xdr filename)
    (xdr-int xdr cookie)
    xdr))

(defun add-direntries (dirlist max startindex)
  (let ((xdr (create-xdr :direction :build))
        (nextindex (1+ startindex))
        entry)
    (if (and (> startindex 0 )(>= startindex (length dirlist)))
	(progn
	  (format t "add-direntries: Doing bogus startindex hack.~%")
	  (setf startindex 0)
	  (setf nextindex 1)
	  ))
    (setf dirlist (subseq dirlist startindex))
    (dolist (p dirlist)
      ;;(format t "max is ~D~%" max)
      (setf entry (make-direntry-xdr p nextindex))
      (if (> (xdr-size entry) max)
          (progn
	    (format t "add-direntries: stopping at entry for ~A due to size reasons.~%"
		    p)
            (xdr-int xdr 0) ;; no more entries
            (return-from add-direntries (values xdr nil))))
      (xdr-xdr xdr entry)
      (decf max (xdr-size entry))
      (incf nextindex))
    (xdr-int xdr 0) ;; no more entries
    (format t "Reached end of entries~%")
    (values xdr t)))

(defun make-readdirres-xdr (stat readdirok)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr stat)
    (if (= stat 0)
        (xdr-xdr xdr readdirok))
    xdr))

(defun make-readdirok-xdr (entries eof)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-xdr xdr entries)
    (xdr-int xdr (if eof 1 0))
    xdr))

#|
          struct diropargs {
              fhandle  dir;
              filename name;
          };
|#

(defstruct diropargs
  fhandle
  filename)

(defun xdr-to-diropargs (xdr)
  (make-diropargs
   :fhandle (create-xdr :vec (xdr-opaque-fixed xdr :len *fhsize*))
   :filename (xdr-string xdr)))

(defun diropargs-struct-to-pathname (doa)
  (add-filename-to-dirname
   (fhandle-to-pathname (diropargs-fhandle doa))
   (diropargs-filename doa)))

(defun diropargs-xdr-to-pathname (xdr)
  (diropargs-struct-to-pathname (xdr-to-diropargs xdr)))

(defun add-filename-to-dirname (dir filename)
  (setf dir (namestring dir))
  (if (not (= (position #\\ dir :from-end t) (1- (length dir))))
      (setf dir (concatenate 'string dir "\\")))
  (concatenate 'string dir filename))

(defun nfsd-lookup (peer xid cbody)
  (let* ((doa (xdr-to-diropargs (call-body-params cbody)))
         (dir (let ((dir (fhandle-to-pathname (diropargs-fhandle doa))))
		(when (null dir)
		  (send-successful-reply
		   peer xid (nfsd-null-verf)
		   (make-diropres-xdr NFSERR_STALE nil))
		  (return-from nfsd-lookup))
		dir))
         (filename (diropargs-filename doa))
         (newpath (add-filename-to-dirname dir filename))
         (newfhandle
	  (third (multiple-value-list (pathname-to-fhandle newpath)))))
    (if *nfsdebug*
	(format t "nfds-lookup ~A in ~A~%" filename dir))
    (if (probe-file newpath)
	(send-successful-reply 
	 peer xid 
	 (nfsd-null-verf) 
	 (make-diropres-xdr 
	  0 (make-diropok newfhandle (make-fattr-from-pathname newpath))))
      (send-successful-reply
       peer xid (nfsd-null-verf)
       (make-diropres-xdr NFSERR_NOENT nil)))))



(defun make-diropres-xdr (stat diropok)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr stat)
    (if (= stat 0)
        (xdr-xdr xdr diropok))
    xdr))

(defun make-diropok (fhandle attributes)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-xdr xdr fhandle)
    (xdr-xdr xdr attributes)
    xdr))

#|
struct readargs {
                   fhandle file;
                   unsigned offset;
                   unsigned count;
                   unsigned totalcount;
		   };
		   |#

(defparameter *nfs-readbuf* 
    (make-array *maxdata* :element-type '(unsigned-byte 8)))

(defparameter *nfs-openfilelist* nil)

(defstruct openfile
  pathname
  direction
  stream
  lastaccess)

(defparameter *nfs-openfilelist-lock* (mp:make-process-lock))

(defun get-open-file (pathname direction)
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
	  (openfile-stream newof))))))


(defun locate-open-file (of)
  (find of *nfs-openfilelist*
	:test #'(lambda (a b)
		  (and (equalp (openfile-pathname a) (openfile-pathname b))
		       (eq (openfile-direction a) (openfile-direction b))))))

(defparameter *reaptime* 2) ;; seconds

(defun reap-open-files ()
  (mp:with-process-lock (*nfs-openfilelist-lock*)
    (let ((res nil)
	  (now (get-universal-time)))
      (dolist (of *nfs-openfilelist*)
	(if (< (- now (openfile-lastaccess of)) *reaptime*)
	    (push of res)
	  (close (openfile-stream of)))
	(setf *nfs-openfilelist* res)))))

(defun nfsd-open-file-reaper ()
  (loop
    (sleep *reaptime*)
    (reap-open-files)))

(defun nfsd-read (peer xid cbody)
  (let* ((readargs (call-body-params cbody))
	 (fhandle (xdr-opaque-fixed readargs :len *fhsize*))
	 (p (fhandle-vec-to-pathname fhandle))
	 (offset (xdr-unsigned-int readargs))
	 (count (xdr-unsigned-int readargs))
	 ;;(totalcount (xdr-unsigned-int readargs))
	 (res (create-xdr :direction :build))
	 bytesread
	 ) ;; unused
    ;;(format t "nfsd-read(~A, offset=~D, count=~D)~%"  p offset count)
    (let ((f (get-open-file p :input)))
      (file-position f offset)
      (setf bytesread (read-sequence *nfs-readbuf* f :end count)))
    (xdr-int res NFS_OK) 
    (xdr-xdr res (make-fattr-from-pathname p))
    (xdr-opaque-variable res (subseq *nfs-readbuf* 0 bytesread))
    (send-successful-reply 
     peer xid 
     (nfsd-null-verf) 
     res)))

(defun createargs-xdr-to-pathname-and-sattr-struct (xdr)
  (values (diropargs-xdr-to-pathname xdr) 
	  (xdr-sattr-to-struct-sattr xdr)))

(defun nfsd-create (peer xid cbody)
  (multiple-value-bind (newpath sattr)
      (createargs-xdr-to-pathname-and-sattr-struct (call-body-params cbody))
    (if *nfsdebug*
	(format t "nfsd-create( ~A with attributes ~A~%" newpath sattr))
    (if (nfs-okay-to-write (call-body-cred cbody))
	(if (probe-file newpath)
	    (send-successful-reply peer xid (nfsd-null-verf) (make-diropres-xdr NFSERR_EXIST nil))
	  (progn
	    ;; need error checking here
	    (with-open-file (f newpath :direction :output)
	      )
	    (send-successful-reply 
	     peer xid 
	     (nfsd-null-verf) 
	     (make-diropres-xdr 
	      NFS_OK 
	      (make-diropok 
	       (third (multiple-value-list (pathname-to-fhandle newpath)))
	       (make-fattr-from-pathname newpath))))))
      (send-successful-reply peer xid (nfsd-null-verf) 
			     (make-diropres-xdr NFSERR_ACCES nil))))) 
  
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

(defun nfsd-remove (peer xid cbody)
  (let* ((doa (xdr-to-diropargs (call-body-params cbody)))
	 (fhandle (diropargs-fhandle doa))
	 (dir (fhandle-to-pathname fhandle))
	 (filename (diropargs-filename doa))
	 (newpath (add-filename-to-dirname dir filename))
	 (res (create-xdr :direction :build)))
    (if *nfsdebug*
	(format t "nfsd-remove(~A)~%" newpath))
    (if (nfs-okay-to-write (call-body-cred cbody))
	(progn
	  (handler-case (delete-file newpath)
	    (file-error (c)
	      (if* (= 13 (excl::file-error-errno c))
		 then ;; permission denied
		      (xdr-int res NFSERR_ACCES)
		      (send-successful-reply peer xid (nfsd-null-verf) res)
		      (return-from nfsd-remove)
		 else ;; unknown (as of now)
		      (xdr-int res NFSERR_IO)
		      (send-successful-reply peer xid (nfsd-null-verf) res)
		      (return-from nfsd-remove))))
	  (xdr-int res NFS_OK)
	  (send-successful-reply peer xid (nfsd-null-verf) res))
      (progn
	(xdr-int res NFSERR_ACCES)
	(send-successful-reply peer xid (nfsd-null-verf) res)))))
    
    
(defun nfsd-write (peer xid cbody)
  (let* ((xdr (call-body-params cbody))
	 (fhandle (xdr-opaque-fixed xdr :len *fhsize*))
	 (p (fhandle-vec-to-pathname fhandle))
	 (beginoffset (xdr-unsigned-int xdr)) ;; not used
	 (offset (xdr-unsigned-int xdr))
	 (totalcount (xdr-unsigned-int xdr)) ;; not used
	 (data (xdr-opaque-variable xdr)))
    (declare (ignore beginoffset totalcount))
    ;;(format t "nfsd-write(~A offset=~D)~%" p offset)
    (if (nfs-okay-to-write (call-body-cred cbody))
	(let ((f (get-open-file p :output)))
	  (file-position f offset)
	  (write-sequence data f)
	  (send-successful-reply 
	   peer xid 
	   (nfsd-null-verf) 
	   (make-attrstat 0 (make-fattr-from-pathname p))))
      (progn
	(format t "nfsd-write: permission denied~%")
	(send-successful-reply 
	 peer xid 
	 (nfsd-null-verf) 
	 (make-attrstat NFSERR_ACCES nil))))))


;;; this is often used to truncate files (if the size parameters is
;;; not 0xffffffff)
(defun nfsd-setattr (peer xid cbody)
  (let* ((xdr (call-body-params cbody))
	 (fhandle (xdr-opaque-fixed xdr :len *fhsize*))
	 (p (fhandle-vec-to-pathname fhandle))
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
			     (make-attrstat NFSERR_ACCES nil)))))
    

(defun nfsd-rename (peer xid cbody)
  (let* ((xdr (call-body-params cbody))
	 (from (diropargs-xdr-to-pathname xdr))
	 (to (diropargs-xdr-to-pathname xdr))
	 (res (create-xdr :direction :build)))
    (if *nfsdebug*
	(format t "nfsd-rename(~A -> ~A)~%" from to))
    (rename-file from to) ;; need error checking
    (xdr-int res NFS_OK)
    (send-successful-reply peer xid (nfsd-null-verf) res)))

(defun nfsd-mkdir (peer xid cbody)
  (multiple-value-bind (newpath sattr)
      (createargs-xdr-to-pathname-and-sattr-struct (call-body-params cbody))
    (if *nfsdebug*
	(format t "nfsd-mkdir( ~A with attributes ~A~%" newpath sattr))
    (if (nfs-okay-to-write (call-body-cred cbody))
	(progn 
	  (if (probe-file newpath)
	      (send-successful-reply peer xid (nfsd-null-verf) (make-diropres-xdr NFSERR_EXIST nil))
	    (progn
	      ;; need error checking here
	      (make-directory newpath)
	      (send-successful-reply 
	       peer xid 
	       (nfsd-null-verf) 
	       (make-diropres-xdr 
		NFS_OK 
		(make-diropok 
		 (third (multiple-value-list (pathname-to-fhandle newpath)))
		 (make-fattr-from-pathname newpath)))))))
      (send-successful-reply peer xid (nfsd-null-verf)
			     (make-diropres-xdr NFSERR_ACCES nil)))))

(defun nfsd-rmdir (peer xid cbody)
  (let* ((newpath (diropargs-xdr-to-pathname (call-body-params cbody)))
	 (res (create-xdr :direction :build))
	 (resval NFS_OK))
    (if *nfsdebug*
	(format t "nfds-rmdir(~A)~%" newpath))
    (if (nfs-okay-to-write (call-body-cred cbody))
	(if (probe-file newpath) 
	    (handler-case 
		(delete-directory newpath)
	      (file-error () (setf resval NFSERR_IO)))
	  (setf resval NFSERR_NOENT))
      (setf resval NFSERR_ACCES))
    (xdr-int res resval)
    (send-successful-reply peer xid (nfsd-null-verf) res)))

    

;; should also check the file permissions..
(defun nfs-okay-to-write (cred)
  (if (= 1 (opaque-auth-flavor cred))
      (let ((au (xdr-auth-unix (opaque-auth-body cred))))
	(= (auth-unix-uid au) *nfslocaluid*))
    nil))
	  
(defun read-nfs-cfg ()
  (with-open-file (s *nfsconfigfile*)
    (dolist (pair (read s))
      (set (first pair) (second pair)))))
