;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the Franz
;; preamble to the LGPL found in
;; http://opensource.franz.com/preamble.html.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License can be
;; found at http://opensource.franz.com/license.html.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; $Id: nfs.cl,v 1.45 2003/03/19 16:54:16 dancy Exp $

;; nfs

(in-package :user)

(defvar *nfsd-version* "1.1.3")

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

;;; these settings may be overridden by the config file
(defparameter *nfsdebug* nil)
(defparameter *nfslocaluid* 443)
(defparameter *nfslocalgid* 50)
(defparameter *nfslocalumask* #o022)

(defparameter *openfilereaptime* 2) ;; seconds
(defparameter *statcachereaptime* 5) ;; should always be larger than *openfilereaptime*
(defparameter *nfs-dircachereaptime* 5)

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
  (case errno
    (2 NFSERR_NOENT)  ;; ENOENT
    (5 NFSERR_IO)     ;; EIO
    (13 NFSERR_ACCES) ;; EACCES
    (22 NFSERR_ACCES) ;; ENFILE
    (41 NFSERR_NOTEMPTY) ;; ENOTEMPTY
    (t NFSERR_IO)))
    
    
(defun make-nfsdsockets ()
  (unless *nfsd-tcp-socket*
    (setf *nfsd-tcp-socket*
      (socket:make-socket :type :stream
                          :connect :passive
                          :local-port *nfsport*
                          :reuse-address t))
    #+(version>= 6 1)
    (socket:set-socket-options *nfsd-tcp-socket*
			       :receive-buffer-size *socketbuffersize*
			       :send-buffer-size *socketbuffersize*))
  (unless *nfsd-udp-socket*
    (setf *nfsd-udp-socket*
      (socket:make-socket :type :datagram
			  :local-port *nfsport*
			  :reuse-address t))
    #+(version>= 6 1)
    (socket:set-socket-options *nfsd-udp-socket*
			       :receive-buffer-size *socketbuffersize*
			       :send-buffer-size *socketbuffersize*)))

  

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
  (let ((server (make-rpc-server :tcpsock *nfsd-tcp-socket*
				 :udpsock *nfsd-udp-socket*)))
    (loop
      (multiple-value-bind (xdr peer)
          (rpc-get-message server)
        (nfsd-message-handler xdr peer)))))

(defun nfsd-message-handler (xdr peer)
  (let* ((msg (create-rpc-msg xdr))
	 (cbody (rpc-msg-cbody msg))
	 (xid (rpc-msg-xid msg)))
    ;;(pprint-cbody cbody)
    (unless (= (rpc-msg-mtype msg) 0)
      (error "Unexpected data!"))
    
    ;; sanity checks first
    (if* (not (= (call-body-prog cbody) *nfsprog*))
       then
	    (if *nfsdebug*
		(format t "Sending program unavailable response for prog=~D~%"
		    (call-body-prog cbody)))
	    (rpc-send-prog-unavail peer xid (nfsd-null-verf))
	    (return-from nfsd-message-handler))
    (if* (not (= (call-body-vers cbody) *nfsvers*))
       then
	    (if *nfsdebug*
		(write-line "Sending program version mismatch response"))
	    (rpc-send-prog-mismatch peer xid
				    (nfsd-null-verf) *nfsvers* *nfsvers*)
	    (return-from nfsd-message-handler))
    (if* (not (access-allowed-p (rpc-peer-addr peer)))
       then
	    (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) 
					      :create nil)
	      (xdr-int *nfsdxdr* NFSERR_ACCES)
	      (return-from nfsd-message-handler)))
    (case (call-body-proc cbody)
      (0 (nfsd-null peer xid))
      (1 (nfsd-getattr peer xid (call-body-params cbody)))
      (2 (nfsd-setattr peer xid cbody))
      (4 (nfsd-lookup peer xid (call-body-params cbody)))
      (6 (nfsd-read peer xid (call-body-params cbody)))
      (8 (nfsd-write peer xid cbody))
      (9 (nfsd-create peer xid cbody))
      (10 (nfsd-remove peer xid cbody))
      (11 (nfsd-rename peer xid cbody))
      (13 (nfsd-symlink peer xid cbody))
      (14 (nfsd-mkdir peer xid cbody))
      (15 (nfsd-rmdir peer xid cbody))
      (16 (nfsd-readdir peer xid (call-body-params cbody)))
      (17 (nfsd-statfs peer xid (call-body-params cbody)))
      (t 
       (rpc-send-proc-unavail peer xid (nfsd-null-verf))
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
      (if* (null p)
	 then
	      (xdr-int *nfsdxdr* NFSERR_STALE)
	 else
	      (xdr-int *nfsdxdr* NFS_OK)
	      (update-fattr-from-pathname p *nfsdxdr*)))))

(defparameter *nfs-statcache* (make-hash-table :test #'equalp))
(defparameter *statcache-lock* (mp:make-process-lock))

;; sbcons..  (sb . lastaccesstime)
(defun lookup-statcache (p)
  (unless (pathnamep p)
    (error "lookup-statcache must be called with a pathname"))
  (mp:with-process-lock (*statcache-lock*)
    (let ((sbcons (gethash p *nfs-statcache*)))
      (if* sbcons
	 then
	      (setf (cdr sbcons) (get-universal-time)) ;; update access time
	      (car sbcons)
	 else ;; new cache entry
	      (setf sbcons 
		(cons (ff:allocate-fobject 'stat) (get-universal-time)))
	      (stat (namestring p) (car sbcons))
	      (setf (gethash p *nfs-statcache*) sbcons)
	      (car sbcons)))))

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

(defun remove-statcache (p)
  (mp:with-process-lock (*statcache-lock*)
    (remhash p *nfs-statcache*)))

;;; how much to substract from a universal time to make it into a 
;;; unix time.  Or, how much to add to a unix time to make it a universal
;;; time
(defconstant *universal-time-to-unix-time* 2208988800)

(defmacro sbslot (slotname) 
  `(ff:fslot-value-typed 'stat :foreign sb ,slotname))

;; used when reading from a file or directory
(defun update-stat-atime (p)
  (let ((sb (lookup-statcache p)))
    (setf (sbslot 'st_atime)
      (- (get-universal-time) *universal-time-to-unix-time*))
    sb))

;; used by things that update inode information.
(defun update-stat-ctime (p)
  (let ((sb (lookup-statcache p)))
    (setf (sbslot 'st_ctime)
      (- (get-universal-time) *universal-time-to-unix-time*))
    sb))

;; updates ctime as well.
;; used by directory modifying functions which don't care about size.
(defun update-atime-and-mtime (p) 
  (let ((sb (update-stat-atime p))
	(now (get-universal-time)))
    (setf (sbslot 'st_mtime)
      (- now *universal-time-to-unix-time*))
    (update-stat-ctime p)
    sb))

;; used by file modification functions.
(defun update-stat-times-and-size (f p)
  (let ((sb (update-atime-and-mtime p))
	(pos (file-position f)))
    (if (> pos (sbslot 'st_size))
	(setf (sbslot 'st_size) pos))))

(defun set-cached-file-size (p size)
  (let ((sb (lookup-statcache p)))
    (setf (sbslot 'st_size) size)
    (update-stat-ctime p)))

(defun set-cached-file-atime (p atime)
  (let ((sb (lookup-statcache p)))
    (setf (sbslot 'st_atime) atime)
    (update-stat-ctime p)))

(defun set-cached-file-mtime (p mtime)
  (let ((sb (lookup-statcache p)))
    (setf (sbslot 'st_mtime) mtime)
    (update-stat-ctime p)))

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

(defun update-fattr-from-pathname (p xdr)
  (declare
   (type pathname p))
  (let* ((id (pathname-to-fhandle-id p))
	 (sb (lookup-statcache p)))
    (update-fattr 
     xdr
     (if (= 0 (logand (sbslot 'st_mode) #o40000))
	 *NFREG* 
       *NFDIR*) ;; type
     (logand (sbslot 'st_mode) (lognot *nfslocalumask*)) ;; mode
     (sbslot 'st_nlink) ;;nlink
     *nfslocaluid* ;; uid
     *nfslocalgid* ;; gid
     (sbslot 'st_size) ;;size 
     ;; changed to 8192 to work around linux's weird readdir stuff
     8192 ;; *blocksize* ;; blocksize  
     (sbslot 'st_rdev)  ;; rdev
     (howmany (sbslot 'st_size) *blocksize*) ;;blocks
     (sbslot 'st_rdev) ;; fsid
     id ;; fileid 
     (list (sbslot 'st_atime) 0) ;; atime
     (list (sbslot 'st_mtime) 0) ;; mtime
     (list (sbslot 'st_mtime) 0) ; ctime
     )
    ))

;; 68 bytes
(defun update-fattr (xdr type mode nlink uid gid size blocksize rdev blocks
		     fsid fileid atime mtime ctime)
  ;;(format t "update-fattr: mtime is ~D~%" mtime)
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

(defun make-fsinfo-xdr (p xdr)
  (let (freeblocks
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
      (let ((xdr (create-xdr :direction :build))
	    (errcode NFS_OK))
	(handler-case (make-fsinfo-xdr p xdr)
	  (t (c)
	    (format t "make-fsinfo-xdr got error ~A~%" c)
	    (setf errcode NFSERR_IO)))
	(send-successful-reply peer xid (nfsd-null-verf) 
			       (make-statfsres errcode xdr))))))

(defun canonicalize-dir (dir)
  (setf dir (namestring dir))
  (if (not (= (position #\\ dir :from-end t) (1- (length dir))))
      (setf dir (concatenate 'string dir "\\")))
  dir)

;; only works when 'dir' doesn't have a trailing backslash.
;; this probably could use some security checks.
(defun parent-dir (dir)
  (setf dir (path-namestring dir))
  (if (string= ":\\" (subseq dir 1))
      (pathname dir)
    (pathname (subseq dir 0 (1- (length dir))))))

;;; readdirargs:  fhandle dir, cookie, count
(defun nfsd-readdir (peer xid params)
  (with-xdr-xdr (params)
    (let ((p (xdr-fhandle-to-pathname params))
	  (cookie (xdr-unsigned-int params))
	  (count (xdr-unsigned-int params)))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null p)
	    (xdr-int *nfsdxdr* NFSERR_STALE)	  
	  (progn
	    (if *nfsdebug*
		(format t "nfsd-readdir(~A, count ~D, cookie ~S)~%"
			p count cookie))
	    (add-direntries *nfsdxdr* p count cookie)
	    (update-stat-atime p)))))))


;; entries look like (dirlist . atime)
(defparameter *nfs-dircache* (make-hash-table :test #'equalp))
(defparameter *nfs-dircachelock* (mp:make-process-lock))

(defun dump-nfsdircache ()
  (maphash #'(lambda (key value)
	       (format t "~S -> ~S~%" key value))
	   *nfs-dircache*))

(defun dircache-reaper ()
  (if (not (= 0 *nfs-dircachereaptime*))
      (loop
	(sleep *nfs-dircachereaptime*)
	(reap-dircache))))

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
    (let ((dc (if* (= 0 *nfs-dircachereaptime*) 
		 then
		      (reap-dircache) ;; so it doesn't grow forever
		      nil 
		 else
		      (gethash dir *nfs-dircache*))))
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
		(if (null dirlist)
		    (set-nfs-dircache file dir)
		  (rplacd (last dirlist) (list file)))
	      (setf (nth pos dirlist) file)))))))

(defun set-nfs-dircache (file dir)
  (let ((dc (cons (list file) (get-universal-time))))
    (setf (gethash dir *nfs-dircache*) dc)))

(defun nfs-remove-file-from-dircache (file dir)
  (mp:with-process-lock (*nfs-dircachelock*)
    (setf file (pathname file))
    (let* ((dirlist (nfs-lookup-dir dir))
	   (cons (member file dirlist :test #'equalp)))
      (if cons
	  (rplaca cons nil)))))

    
(defun add-direntries (xdr dir max startindex)
  (block nil
    (let ((dirlist
	   (handler-case (nfs-lookup-dir dir)
	     (file-error (c)
	       (case (excl::syscall-error-errno c)
		 (5 
		  (format t "~
add-direntries: Handling I/O error while reading directory ~A~%" 
			  dir)
		  (xdr-int xdr NFSERR_IO)
		  (return :err))
		 (t
		  (error c))))))
	  ;; starts at 8 because in the minimal case, we need to be able to
	  ;; add the final 0 discriminant.. and the eof indicator
	  (totalbytesadded 8)
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
    (when (eq *nfsdebug* :verbose)
      (format t "~D: ~A fileid=~A next=~A" (1- cookie) filename fileid cookie))
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
      (if *nfsdebug* (format t "nfsd-lookup ~S in ~S~%" filename dir))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (null dir)
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (progn
	    (if (string= filename "..")
		(setf newpath (parent-dir dir))
	      (setf newpath (add-filename-to-dirname dir filename)))
	    (if* (or (null newpath) (not (probe-file newpath)))
	       then
		    (if (eq *nfsdebug* :verbose) (format t "file not found~%"))
		    (xdr-int *nfsdxdr* NFSERR_NOENT)
	       else
		    (xdr-int *nfsdxdr* NFS_OK)
		    (pathname-to-fhandle-with-xdr *nfsdxdr* newpath)
		    (update-fattr-from-pathname newpath *nfsdxdr*))))))))
	      

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
	      (openfile-stream newof)))))
    (file-error (c)
      (format t "get-open-file: condition: ~a~%" c) 
      (values nil (excl::syscall-error-errno c)))))


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
  (if (eq *nfsdebug* :verbose) 
      (format t "close-open-file ~S~%" p))
  (mp:with-process-lock (*nfs-openfilelist-lock*)
    (let (of)
      (while (setf of (locate-open-file-any p))
	     (setf (openfile-lastaccess of) 0)
	     (if *nfsdebug*
		 (format t "~
close-open-file: Calling reap-open-files to effect a close~%"))
	     (reap-open-files)))))
	     

(defun reap-open-files ()
  (mp:with-process-lock (*nfs-openfilelist-lock*)
    (let ((res nil)
	  (now (get-universal-time)))
      (dolist (of *nfs-openfilelist*)
	(if* (< (- now (openfile-lastaccess of)) *openfilereaptime*)
	   then (push of res)
	   else
		(let* ((pathname (openfile-pathname of))
		       (sb (lookup-statcache pathname)))
		  (if *nfsdebug*
		      (format t "reap-open-files Closing ~S~%" 
			      (openfile-pathname of)))
		  (close (openfile-stream of))
		  (if (eq (openfile-direction of) :output)
		      (set-file-time pathname
				     (sbslot 'st_atime)
				     (sbslot 'st_mtime))))))
      (setf *nfs-openfilelist* res))))

(defun nfsd-open-file-reaper ()
  (loop
    (sleep *openfilereaptime*)
    (reap-open-files)))

(defmacro with-nfs-err-handler ((xdr) &body body)
  (let ((xdrsym (gensym))
	(savepossym (gensym)))
    `(let* ((,xdrsym ,xdr)
	    (,savepossym (xdr-pos ,xdrsym)))
       (handler-case (progn ,@body)
	 (file-error (c)
	   (setf (xdr-pos ,xdrsym) ,savepossym)
	   (format t "Handling file error ~A~%" c)
	   (xdr-int ,xdrsym (map-errno-to-nfs-error-code 
			     (excl::syscall-error-errno c))))
	 (t (c)
	   (setf (xdr-pos ,xdrsym) ,savepossym)
	   (format t "Handling unexpected error ~A~%" c)
	   (xdr-int ,xdrsym NFSERR_IO))))))

(defun nfsd-read (peer xid params)
  (with-xdr-xdr (params)
    (let* ((p (xdr-fhandle-to-pathname params))
	   (offset (xdr-unsigned-int params))
	   (count (xdr-unsigned-int params))
	   ;;(totalcount (xdr-unsigned-int readargs))  ;; unused
	   )
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if (eq *nfsdebug* :verbose)
	    (format t "nfsd-read(~A, offset=~D, count=~D)~%"  p offset count))
	(if (null p)
	    (xdr-int *nfsdxdr* NFSERR_STALE)
	  (with-nfs-err-handler (*nfsdxdr*)
	    (multiple-value-bind (f errno)
		(get-open-file p :input)
	      (if* (null f)
		 then
		      (xdr-int *nfsdxdr* 
			       (map-errno-to-nfs-error-code errno))
		 else
		      (file-position f offset)
		      (xdr-with-seek
		       (*nfsdxdr* 72)
		       (xdr-opaque-variable-from-stream *nfsdxdr* f count))
		      (xdr-int *nfsdxdr* NFS_OK) 
		      (update-stat-atime p)
		      (update-fattr-from-pathname p *nfsdxdr*)))))))))

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
	    (if *nfsdebug*
		(format t "nfsd-create( ~A with attributes ~A~%"
			newpath sattr))
	    (if (or (null newpath)
		    (not (nfs-okay-to-write (call-body-cred cbody))))
		(progn
		  (format t "permission denied~%") 
		  (xdr-int *nfsdxdr* NFSERR_ACCES))
	      ;; okay to write
	      (if (probe-file newpath)
		  (xdr-int *nfsdxdr* NFSERR_EXIST)
		(let ((f (handler-case (open newpath :direction :output)
			   (file-error (c)
			     (cond 
			      ((= (excl::syscall-error-errno c) 22) ;; ENFILE
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
			(update-fattr-from-pathname
			 newpath *nfsdxdr*))))))))))))

  
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
		    (handler-case (excl::filesys-delete-file
				   (namestring newpath))
		      (file-error (c)
			(let ((errno (excl::syscall-error-errno c)))
			  (if (numberp errno)
			      (cond
			       ((= 2 errno)
				(format t
					"delete: no such file or directory~%")
				(xdr-int *nfsdxdr* NFSERR_NOENT))
			       ((= 13 errno)
				(format t "delete: permission denied~%")
				(xdr-int *nfsdxdr* NFSERR_ACCES))
			       (t 
				(error c)))
			    (error c))))
		      (:no-error (c)
			(declare (ignore c))
			(update-atime-and-mtime dir)
			(nfs-remove-file-from-dircache newpath dir)
			(remove-fhandle-by-pathname newpath)
			(remove-statcache newpath)
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
	(if* (null p)
	   then
		(xdr-int *nfsdxdr* NFSERR_STALE)
	   else
		(if (eq *nfsdebug* :verbose)
		    (format t "nfsd-write(~A,offset=~A) ~D bytes~%"
			    p offset (third data)))
		(if* (not (nfs-okay-to-write (call-body-cred cbody)))
		   then
			(if *nfsdebug*
			    (format t "nfsd-write: permission denied~%"))
			(xdr-int *nfsdxdr* NFSERR_ACCES)
		   else
			(multiple-value-bind (f #+ignore errno)
			    (get-open-file p :output)
			  (if* (null f)
			     then (format t "nfsd-write: IO error~%")
				  (xdr-int *nfsdxdr* NFSERR_IO)
			     else (file-position f offset)
				  (write-sequence 
				   (xdr-vec (first data))
				   f
				   :start (second data)
				   :end (+ (third data) (second data)))
				  (if* *nfsdebug*
				     then
					  (format t "filesize after write: ~D~%"
						  (file-length f))
					  (format t "file position after write: ~D~%"
						  (file-position f)))
				  (update-stat-times-and-size f p)
				  (xdr-int *nfsdxdr* NFS_OK)
				  (update-fattr-from-pathname p *nfsdxdr*)))))))))


(defun nfsd-setattr (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name xdr)
    (let* ((p (xdr-fhandle-to-pathname xdr))
	   (sattr (xdr-sattr-to-struct-sattr xdr))
	   (err 0))
      (if *nfsdebug*
	  (format t "nfsd-setattr(~A ~A)~%" p  sattr))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if* (not (nfs-okay-to-write (call-body-cred cbody)))
	   then
		(format t "permission denied~%") 
		(xdr-int *nfsdxdr* NFSERR_ACCES)
	   else
		(close-open-file p)
		(if* (not (= (sattr-size sattr) #xffffffff))
		   then
			(setf err 
			  (truncate-file (namestring p) (sattr-size sattr)))
			(if* (= err 0)
			   then
				(if *nfsdebug*
				    (format t "filesize after truncation is ~S~%"
					    (file-length p)))
				(set-cached-file-size p (sattr-size sattr))
				(update-atime-and-mtime p)))

	      ;;; atime and mtime mods should always come together.  We'll
	      ;;; presume that here to simplify the code.
		(if* (and (= err 0)
			  (not (= (first (sattr-atime sattr)) #xffffffff)))
		   then
			(set-file-time p 
				       (first (sattr-atime sattr))
				       (first (sattr-mtime sattr)))
			(set-cached-file-atime p (first (sattr-atime sattr)))
			(set-cached-file-mtime p (first (sattr-mtime sattr))))
	      ;;; uid/gid/mode updates are ignored for now
		(if* (= err 0)
		   then
			(xdr-int *nfsdxdr* NFS_OK)
			(update-fattr-from-pathname p *nfsdxdr*)
		   else
			(xdr-int *nfsdxdr* NFSERR_IO)))))))

;;; from:  fhandle dir, filename name
;;; to:    fhandle dir, filename name
(defun nfsd-rename (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((fromdir (xdr-fhandle-to-pathname params))
	  (fromfilename (xdr-string params))
	  (todir (xdr-fhandle-to-pathname params))
	  (tofilename (xdr-string params)))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if* (or (null fromdir) (null todir))
	   then
		(xdr-int *nfsdxdr* NFSERR_STALE)
	   else
		(let ((from (add-filename-to-dirname fromdir fromfilename))
		      (to (add-filename-to-dirname todir tofilename)))
		  (if *nfsdebug* 
		      (format t "nfsd-rename(~A -> ~A)~%" from to))
		  (cond 
		   ((null from) 
		    (xdr-int *nfsdxdr* NFSERR_NOENT))
		   ((or (null to) 
			(not (nfs-okay-to-write (call-body-cred cbody))))
		    (xdr-int *nfsdxdr* NFSERR_ACCES))
		   (t 
		    (close-open-file from)
		    (close-open-file to)
		    (if* (eq :err (rename-helper from to))
		       then
			    (xdr-int *nfsdxdr* NFSERR_IO)
		       else
			    (swap-fhandles from to)
			    (remove-fhandle-by-pathname from)
			    (remove-statcache from)
			    (remove-statcache to)
			    (update-atime-and-mtime fromdir)
			    (update-atime-and-mtime todir)
			    (nfs-remove-file-from-dircache from fromdir)
			    (nfs-add-file-to-dircache to todir)
			    (xdr-int *nfsdxdr* NFS_OK))))))))))

(defun rename-helper (from to)
  (handler-case
      (rename-file
       from (merge-pathnames to (make-pathname :type :unspecific)))
    (t (c)
      (declare (ignore c))
      :err)))

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
	    (if *nfsdebug*
		(format t "nfsd-mkdir( ~A with attributes ~A~%" newpath sattr))
	    (if (or (null newpath)
		    (not (nfs-okay-to-write (call-body-cred cbody))))
		(progn
		  (format t "permission denied~%")
		  (xdr-int *nfsdxdr* NFSERR_ACCES))
	      ;; okay to write
	      (if* (probe-file newpath)
		 then 
		      (xdr-int *nfsdxdr* NFSERR_EXIST)
	       elseif (null (ignore-errors (make-directory newpath))) ;; this could use some work
		 then
		      (xdr-int *nfsdxdr* NFSERR_ACCES)
		 else
		      ;; doesn't exist.. do the work
		      (nfs-add-file-to-dircache newpath dir)
		      (update-atime-and-mtime dir)
		      (xdr-int *nfsdxdr* NFS_OK)
		      (pathname-to-fhandle-with-xdr *nfsdxdr* newpath)
		      (update-fattr-from-pathname newpath *nfsdxdr*)))))))))


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
			     ((= 13 (excl::syscall-error-errno c))
			      (xdr-int *nfsdxdr* NFSERR_ACCES))
			     ((= 41 (excl::syscall-error-errno c))
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


#|
struct symlinkargs {
                   diropargs from;  (fhandle dir, filename name)
                   path to;
                   sattr attributes;
		   };
|#		  

;; A lot of work just to ultimately say that the operation
;; doesn't work.
(defun nfsd-symlink (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((dir (xdr-fhandle-to-pathname params))
	  (filename (xdr-string params))
	  (to (xdr-string params))
	  (sattr (xdr-sattr-to-struct-sattr params)))
      (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) :create nil)
	(if* (null dir)
	   then 
		(xdr-int *nfsdxdr* NFSERR_STALE)
	   else 
		(let ((newpath (add-filename-to-dirname dir filename)))
		  (if *nfsdebug*
		      (format t "nfsd-symlink( ~A to ~A with attributes ~A~%"
			      newpath to sattr))
		  (if* (or (null newpath)
			   (not (nfs-okay-to-write (call-body-cred cbody))))
		     then
			  (format t "permission denied~%") 
			  (xdr-int *nfsdxdr* NFSERR_ACCES)
		     else
			  (xdr-int *nfsdxdr* NFSERR_IO))))))))


;; should also check the file permissions..
(defun nfs-okay-to-write (cred)
  (if (= 1 (opaque-auth-flavor cred))
      (let ((au (xdr-opaque-auth-struct-to-auth-unix-struct cred)))
	(= (auth-unix-uid au) *nfslocaluid*))
    nil))
	  

