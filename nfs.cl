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
;; $Id: nfs.cl,v 1.60 2005/01/31 18:42:24 layer Exp $

;; nfs

(in-package :user)

(defvar *nfsd-version* "2.2")

(eval-when (compile)
  (declaim (optimize (speed 3))))

(defconstant *nfsprog* 100003)
(defconstant *nfsvers* 2)
(defconstant *nfsport* 2049)
(defconstant *maxdata* 8192)
(defconstant *maxpathlen* 1024)
(defconstant *maxnamlen* 255)
(defconstant *cookiesize* 4)
(defparameter *nfsd-tcp-socket* nil)
(defparameter *nfsd-udp-socket* nil)
(defparameter *socketbuffersize* (* 128 1024))

(defun make-nfsdsockets ()
  (unless *nfsd-tcp-socket*
    (setf *nfsd-tcp-socket*
      (socket:make-socket :type :hiper
                          :connect :passive
                          :local-port *nfsport*
                          :reuse-address t))
    (socket:set-socket-options *nfsd-tcp-socket*
			       :receive-buffer-size *socketbuffersize*
			       :send-buffer-size *socketbuffersize*))
  (unless *nfsd-udp-socket*
    (setf *nfsd-udp-socket*
      (socket:make-socket :type :datagram
			  :local-port *nfsport*
			  :reuse-address t))
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

(defmacro with-nfsdsockets (() &body body)
  `(progn
     (make-nfsdsockets)
     ,@body
     (close-nfsdsockets)))


(defun nfsd ()
  (format t "Allegro NFS v~A started.~%" *nfsd-version*)
  (if* *nfs-gc-debug*
     then (format t "~&Turning on memory management debugging.~%")
	  (setf (sys:gsgc-switch :print) t)
	  (setf (sys:gsgc-switch :stats) t)
	  (setq excl:*global-gc-behavior* nil)
     else (setf (sys:gsgc-switch :print) nil))	  

  (with-nfsdsockets ()
    (with-portmapper-mapping (*nfsprog* *nfsvers* *nfsport* IPPROTO_TCP)
      (with-portmapper-mapping (*nfsprog* *nfsvers* *nfsport* IPPROTO_UDP)
	(mp:process-run-function "open file reaper" #'nfsd-open-file-reaper)
	(mp:process-run-function "attr cache reaper" #'attr-cache-reaper)
	(mp:process-run-function "dir cache reaper" #'dircache-reaper)
	(let* ((buffer (make-array #.(* 64 1024) :element-type '(unsigned-byte 8)))
	       (server (make-rpc-server :tcpsock *nfsd-tcp-socket*
					:udpsock *nfsd-udp-socket*
					:buffer buffer)))
	  (declare (dynamic-extent buffer server))
	  (loop
	    (multiple-value-bind (xdr peer)
		(rpc-get-message server)
	      (nfsd-message-handler xdr peer))))))))

(defun nfsd-message-handler (xdr peer)
  (let* ((msg (create-rpc-msg xdr))
	 (cbody (rpc-msg-cbody msg))
	 (xid (rpc-msg-xid msg)))
    ;;(pprint-cbody cbody)
    (when (/= (rpc-msg-mtype msg) 0)
      (error "Unexpected data!"))
    
    ;; sanity checks first
    (when (not (= (call-body-prog cbody) *nfsprog*))
      (if *nfs-debug*
	  (format t "Sending program unavailable response for prog=~D~%"
		  (call-body-prog cbody)))
      (rpc-send-prog-unavail peer xid (nfsd-null-verf))
      (return-from nfsd-message-handler))
    
    (when (not (= (call-body-vers cbody) *nfsvers*))
      (if *nfs-debug*
	  (write-line "Sending program version mismatch response"))
      (rpc-send-prog-mismatch peer xid
			      (nfsd-null-verf) *nfsvers* *nfsvers*)
      (return-from nfsd-message-handler))

    (case (call-body-proc cbody)
      (0 (nfsd-null peer xid))
      (1 (nfsd-getattr peer xid cbody))
      (2 (nfsd-setattr peer xid cbody))
      (4 (nfsd-lookup peer xid cbody))
      (6 (nfsd-read peer xid cbody))
      (8 (nfsd-write peer xid cbody))
      (9 (nfsd-create peer xid cbody))
      (10 (nfsd-remove peer xid cbody))
      (11 (nfsd-rename peer xid cbody))
      (13 (nfsd-symlink peer xid cbody))
      (14 (nfsd-mkdir peer xid cbody))
      (15 (nfsd-rmdir peer xid cbody))
      (16 (nfsd-readdir peer xid cbody))
      (17 (nfsd-statfs peer xid cbody))
      ;; secret functions used by the configuration program
      (100 (nfsd-get-config-file-path peer xid cbody))
      (101 (nfsd-reload-configuration peer xid cbody))

      
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


(defmacro with-backtrace-on-non-file-error (() &body body)
  `(handler-bind
       ((error
	 (lambda (c)
	   (ignore-errors 
	    (if (not (typep c 'file-error))
		(with-open-file (f "c:\\temp\\nfs-errlog.txt"
				 :direction :output
				 :if-does-not-exist :create
				 :if-exists :append)
		  (let ((*print-readably* nil)
			(*print-miser-width* 40)
			(*print-pretty* t)
			(tpl:*zoom-print-circle* t)
			(tpl:*zoom-print-level* nil)
			(tpl:*zoom-print-length* nil)
			(*terminal-io* f)
			(*standard-output* f))
		    (tpl:do-command "zoom" :from-read-eval-print-loop nil))))))))
     ,@body))

(defmacro with-nfs-err-handler ((xdr) &body body)
  (let ((savepossym (gensym)))
    `(let ((,savepossym (xdr-pos ,xdr)))
       (handler-case
	   (with-backtrace-on-non-file-error ()
	     ,@body)
	 (file-error (c)
	   (setf (xdr-pos ,xdr) ,savepossym)
	   (if *nfs-debug*
	       (format t "Handling file error: ~A~%" c))
	   (xdr-int ,xdr (map-errno-to-nfs-error-code 
			       (excl::syscall-error-errno c))))
	 (error (c)
	   (setf (xdr-pos ,xdr) ,savepossym)
	   (format t "Handling unexpected error: ~A~%" c)
	   (xdr-int ,xdr NFSERR_IO))))))
    

(defmacro with-non-stale-fh ((xdr fh) &body body)
  (if (listp fh)
      `(if (some #'null (list ,@fh))
	   (xdr-int ,xdr NFSERR_STALE)
	 (progn ,@body))
    `(if ,fh
	 (progn ,@body)
       (xdr-int ,xdr NFSERR_STALE))))
    

(defmacro with-allowed-host-access ((xdr fh addr) &body body)
  `(if (export-host-access-allowed-p (fh-export ,fh) ,addr)
       (progn ,@body)
     (xdr-int ,xdr NFSERR_ACCES)))

(defmacro define-nfs-proc (name arglist &body body)
  (let ((funcname (intern (format nil "~A-~A" 'nfsd name)))
	argdefs debugs fhsyms host-access-check-fh)
    (dolist (pair arglist)
      (ecase (second pair)
	(fhandle 
	 (push (first pair) fhsyms)
	 (push `(,(first pair) (xdr-fhandle params)) argdefs)
	 (push `(format t "~A, " (if ,(first pair)
				    (fh-pathname ,(first pair))
				  "stale-handle"))
	       debugs))
	(string
	 (push `(,(first pair) (xdr-string params)) argdefs)
	 (push `(format t "~A, " ,(first pair)) debugs))
	(unsigned
	 (push `(,(first pair) (xdr-unsigned-int params)) argdefs)
	 (push `(format t "~D, " ,(first pair)) debugs))
	(sattr
	 (push `(,(first pair) (xdr-sattr-to-struct-sattr params)) argdefs)
	 (push `(format t "~A, " ,(first pair)) debugs))
	(data
	 (push `(,(first pair) (xdr-opaque-variable params)) argdefs)
	 (push `(format t "<data>, ") debugs))))
    (setf argdefs (reverse argdefs))
    (push '(format t ")~%") debugs)
    (push '(finish-output) debugs)
    (setf debugs (reverse debugs))
    (setf host-access-check-fh (first fhsyms))
    (if (null host-access-check-fh)
	(error 
	 "define-nfs-proc:  There must be at least one fhandle variable"))
    `(defun ,funcname (peer xid cbody)
       (with-xdr-xdr ((call-body-params cbody) :name params)
	 (let (,@argdefs)
	   (when *nfs-debug*
	     (format t "~A(" (quote ,name))
	     ,@debugs)
	   (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf) 
					     :create nil)
	     (with-non-stale-fh (*nfsdxdr* ,fhsyms)
	       (with-allowed-host-access (*nfsdxdr* ,host-access-check-fh 
					  (rpc-peer-addr peer))
		 (with-nfs-err-handler (*nfsdxdr*)
		   ,@body)))))))))

       

(defmacro with-permission ((fh type) &body body)
  (let ((func (ecase type
		(:read 'nfs-okay-to-read)
		(:write 'nfs-okay-to-write))))
    `(if* (not (,func ,fh (call-body-cred cbody)))
	then
	     (if *nfs-debug* (format t "permission denied~%") )
	     (xdr-int *nfsdxdr* NFSERR_ACCES)
	else
	     ,@body)))


(defun nfsd-null (peer xid)
  (if *nfs-debug*  (format t "nfsd-null~%~%"))
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (nfsd-null-verf) xdr)))


(define-nfs-proc getattr ((fh fhandle))
  (with-permission (fh :read)
    (xdr-int *nfsdxdr* NFS_OK)
    (update-fattr-from-fh fh *nfsdxdr*)))

(defun update-fattr-from-fh (fh xdr)
  (let ((attr (lookup-attr fh))
	(exp (fh-export fh)))
    (update-fattr 
     xdr
     (nfs-attr-type attr)
     (logior 
      (logand (nfs-attr-mode attr) (lognot (nfs-export-umask exp)))
      (nfs-export-set-mode-bits exp)) ;; mode
     (nfs-attr-nlinks attr) ;;nlinks
     (nfs-export-uid exp) ;; uid
     (nfs-export-gid exp) ;; gid
     (nfs-attr-size attr) ;;size 
     (nfs-attr-blocksize attr) ;; blocksize
     (nfs-attr-rdev attr)  ;; rdev
     (nfs-attr-blocks attr) ;; blocks
     (nfs-attr-fsid attr) ;; fsid
     (nfs-attr-fileid attr) ;; fileid 
     (list (universal-to-unix-time (nfs-attr-atime attr)) 0) ;; atime
     (list (universal-to-unix-time (nfs-attr-mtime attr)) 0) ;; mtime
     (list (universal-to-unix-time (nfs-attr-ctime attr)) 0)))) ;; ctime
  
  
  
;; 68 bytes
;; Expects atime, mtime, and ctime to be timevals in unix
;; time format.
(defun update-fattr (xdr type mode nlink uid gid size blocksize rdev blocks
		     fsid fileid atime mtime ctime)
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

(define-nfs-proc statfs ((fh fhandle))
  (with-permission (fh :read)
    (multiple-value-bind (non-priv-free priv-free total)
	(ignore-errors (get-filesystem-free-space (fh-pathname fh)))
      (if* (null non-priv-free)
	 then
	      (xdr-int *nfsdxdr* NFSERR_IO)
	 else
	      ;; Convert to "blocks"
	      (setf non-priv-free (howmany non-priv-free *blocksize*))
	      (setf priv-free (howmany priv-free *blocksize*))
	      (setf total (howmany total *blocksize*))
	    
	      (xdr-int *nfsdxdr* NFS_OK) 
	      (xdr-unsigned-int *nfsdxdr* *maxdata*)
	      (xdr-unsigned-int *nfsdxdr* *blocksize*)
	      (xdr-unsigned-int *nfsdxdr* total)
	      (xdr-unsigned-int *nfsdxdr* priv-free) 
	      (xdr-unsigned-int *nfsdxdr* non-priv-free)))))

;;; readdirargs:  fhandle dir, cookie, count
(define-nfs-proc readdir ((fh fhandle) (cookie unsigned) (count unsigned))
  (with-permission (fh :read)
    (if (add-direntries *nfsdxdr* fh count cookie)
	(update-attr-atime fh))))

;; Returns 't' if okay, nil otherwise.

;; totalbytesadded starts at 8 because in the minimal case, we need to
;; be able to add the final 0 discriminant.. and the eof indicator

(defun add-direntries (xdr dirfh max startindex)
  (let* ((dir (fh-pathname dirfh))
	 (dirlist (nfs-lookup-dir dir))
	 (index startindex)
	 (totalbytesadded 8)
	 (complete t)
	 bytesadded
	 endindex
	 p)
    (when (eq dirlist :err)
      (xdr-int xdr NFSERR_IO)
      (return-from add-direntries))
    
    (xdr-int xdr NFS_OK)

    (setf endindex (length dirlist))
    ;;(format t "startindex: ~D  dirlistlen: ~D~%" startindex endindex)

    (while (< index endindex)
      (setf p (nth index dirlist))
      
      (when p ;; some entries can be nil (due to removal by client)
	(setf bytesadded 
	  (make-direntry-xdr xdr dirfh p (1+ index)))
	(incf totalbytesadded bytesadded)
	(when (> totalbytesadded max)
	  (if (eq *nfs-debug* :verbose) 
	      (format t " [not added due to size overflow]~%"))
	  (if *nfs-debug* 
	      (format t "add-direntries: stopping at entry for ~A (no space)~%" 
		      p))
	  (xdr-backspace xdr bytesadded)
	  (setf complete nil)
	  (return)) ;; break from loop
	
	(if (eq *nfs-debug* :verbose) (format t " [added]~%")))
      
      (incf index))
    
    (xdr-int xdr 0) ;; no more entries
    (xdr-int xdr (if complete 1 0))
    (if (and *nfs-debug* complete)
	(format t "Reached end of entries~%~%"))
    t))


#|
struct entry {
                   unsigned fileid;
                   filename name;
                   nfscookie cookie;
                   entry *nextentry;
           };
|#

;; returns number of bytes added to xdr
(defun make-direntry-xdr (xdr dirfh filename cookie)
  (let* ((fh (lookup-fh-in-dir dirfh filename 
			       :create t
			       :allow-dotnames t))
	 (fileid (fh-id fh)))
    (when (eq *nfs-debug* :verbose)
      (format t "~D: ~A fileid=~A next=~A" (1- cookie) filename fileid cookie))
    (with-xdr-compute-bytes-added (xdr)
      (xdr-int xdr 1) ;; indicate that data follows
      (xdr-unsigned-int xdr fileid)
      (xdr-string xdr filename)
      (xdr-int xdr cookie))))


;; Errors out if file doesn't exist or something.. otherwise,
;; returns file handle.
(defun nfs-probe-file (dirfh filename &key allow-dotnames)
  (with-potential-fhandle (fh dirfh filename :allow-dotnames allow-dotnames)
    (lookup-attr fh)
    fh))

;;; returns:
;;; status
;;; fhandle
;;; attributes (fattr)

(define-nfs-proc lookup ((dirfh fhandle) (filename string))
  (with-permission (dirfh :read)
    (let ((fh (nfs-probe-file dirfh filename :allow-dotnames t)))
      (xdr-int *nfsdxdr* NFS_OK)
      (xdr-fhandle *nfsdxdr* fh)
      (update-fattr-from-fh fh *nfsdxdr*))))


;; readargs: fhandle, offset, count, totalcount
(define-nfs-proc read ((fh fhandle) (offset unsigned) (count unsigned)
				    (totalcount unsigned))
  (with-permission (fh :read)
    (let ((f (get-open-file fh :input)))
      (file-position f offset)
      (xdr-with-seek
       (*nfsdxdr* 72)
       (xdr-opaque-variable-from-stream *nfsdxdr* f count))
      (xdr-int *nfsdxdr* NFS_OK) 
      (update-attr-atime fh)
      (update-fattr-from-fh fh *nfsdxdr*))))

;; args: fhandle dir, filename, sattr 
;; returns: status, fhandle, attributes (if okay status)

;; Doesn't use sattr.
(define-nfs-proc create ((dirfh fhandle) (filename string) (sattr sattr))
  (with-permission (dirfh :write)
    (let* ((fh (lookup-fh-in-dir dirfh filename :create t))
	   (newpath (fh-pathname fh)))
      ;; XXX -- need patch from Kevin for error handling
      ;; to work right here.
      ;; create the file.
      (close (open newpath :direction :output))
      (update-atime-and-mtime dirfh)
      (nfs-add-file-to-dir filename (fh-pathname dirfh))
      (xdr-int *nfsdxdr* NFS_OK)
      (xdr-fhandle *nfsdxdr* fh)
      (update-fattr-from-fh fh *nfsdxdr*))))

  
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
(define-nfs-proc remove ((dirfh fhandle) (filename string))
  ;; nfs-probe-file will throw an error if file doesn't exist
  (let ((fh (nfs-probe-file dirfh filename)))
    (with-permission (dirfh :write)
      (close-open-file fh)
      (delete-file (fh-pathname fh))
      (update-atime-and-mtime dirfh)
      (nfs-remove-file-from-dir filename (fh-pathname dirfh))
      (remove-fhandle fh filename)
      (uncache-attr fh)
      (xdr-int *nfsdxdr* NFS_OK))))

;; args: fhandle, beginoffset, offset, totalcount, data
;;; returns:
;;; status
;;; attributes (if no error)
(define-nfs-proc write ((fh fhandle) (beginoffset unsigned)
				     (offset unsigned)
				     (totalcount unsigned)
				     (data data))
  (with-permission (fh :write)
    (let ((f (get-open-file fh :output)))
      (file-position f offset)
      (write-sequence (xdr-vec (first data))
		      f
		      :start (second data)
		      :end (+ (third data) (second data)))
      (update-attr-times-and-size f fh)
      (xdr-int *nfsdxdr* NFS_OK)
      (update-fattr-from-fh fh *nfsdxdr*))))

;; args: fhandle, sattr
;; returns status and attributes.
(define-nfs-proc setattr ((fh fhandle) (sattr sattr))
  (with-permission (fh :write)
    (let ((p (fh-pathname fh)))
      (close-open-file fh)
      ;; only works on regular files.
      (let ((current-attr (lookup-attr fh)))
	(when (= (nfs-attr-type current-attr) *NFREG*)
	  
	  (let ((newsize (sattr-size sattr))
		(atime (first (sattr-atime sattr)))
		(mtime (first (sattr-mtime sattr))))
	    
	    (setf atime (if (= atime -1) nil (unix-to-universal-time atime)))
	    (setf mtime (if (= mtime -1) nil (unix-to-universal-time mtime)))
	    
	    (when (/= newsize #xffffffff)
	      (os-truncate p newsize)
	      (set-cached-file-size fh newsize)
	      (update-atime-and-mtime fh))
	    
	    (when (or atime mtime)
	      (utime p atime mtime))
	  
	    (if atime
		(setf (nfs-attr-atime current-attr) atime))
	    (if mtime
		(setf (nfs-attr-mtime current-attr) mtime)))))
      
      ;; report success even if we didn't actually do it
      (xdr-int *nfsdxdr* NFS_OK)
      (update-fattr-from-fh fh *nfsdxdr*))))


;;; from:  fhandle dir, filename name
;;; to:    fhandle dir, filename name
;; Renames across exports are not allowed.
;; for NFSv3, we'd return NFSERR_XDEV.  
;; for NFSv2, we return NFSERR_ACCES (ala Linux)

(defmacro with-same-export ((fh1 fh2) &body body)
  `(if (eq (fh-export ,fh1) (fh-export ,fh2))
       (progn ,@body)
     (xdr-int *nfsdxdr* NFSERR_ACCES)))

(define-nfs-proc rename ((fromdirfh fhandle) 
			 (fromfilename string)
			 (todirfh fhandle)
			 (tofilename string))
  (with-same-export (fromdirfh todirfh)
    (with-permission (fromdirfh :write)
      ;; not really necessary since they have to be the same export...
      ;; and the same exports always have the same permissions..
      ;; but leaving it this way to make it clearer.
      (with-permission (fromdirfh :write)
	(with-permission (todirfh :write)
	  ;; nfs-probe-file will throw an error if the file doesn't
	  ;; exist.
	  (let* ((fromfh (nfs-probe-file fromdirfh fromfilename))
		 (from (fh-pathname fromfh))
		 (tofh (lookup-fh-in-dir todirfh tofilename :create t))
		 (to (fh-pathname tofh)))
	    (close-open-file fromfh)
	    (close-open-file tofh)
	    (rename from to)
	    (remove-fhandle tofh tofilename)
	    (rename-fhandle fromfh fromfilename todirfh tofilename)
	    (uncache-attr fromfh)
	    (uncache-attr tofh)
	    (update-atime-and-mtime fromdirfh)
	    (update-atime-and-mtime todirfh)
	    (nfs-remove-file-from-dir fromfilename (fh-pathname fromdirfh))
	    (nfs-add-file-to-dir tofilename (fh-pathname todirfh))
	    (xdr-int *nfsdxdr* NFS_OK)))))))

;;; args:  fhandle dir, filename, sattr
;;; returns: status
;;   fhandle, attributes (if okay status)
(define-nfs-proc mkdir ((dirfh fhandle) (filename string) (sattr sattr))
  (with-permission (dirfh :write)
    (let ((fh (lookup-fh-in-dir dirfh filename :create t)))
      (make-directory (fh-pathname fh))
      (nfs-add-file-to-dir filename (fh-pathname dirfh))
      (update-atime-and-mtime dirfh)
      (xdr-int *nfsdxdr* NFS_OK)
      (xdr-fhandle *nfsdxdr* fh)
      (update-fattr-from-fh fh *nfsdxdr*))))

(define-nfs-proc rmdir ((dirfh fhandle) (filename string))
  (with-permission (dirfh :write)
    (let ((fh (lookup-fh-in-dir dirfh filename :create t)))
      (rmdir (fh-pathname fh)) ;; throws error if non-empty
      (update-atime-and-mtime dirfh)
      (nfs-remove-file-from-dir filename (fh-pathname dirfh))
      (uncache-attr fh)
      (remove-fhandle fh filename)
      (xdr-int *nfsdxdr* NFS_OK))))
  


;; args:  fromdir handle, fromfile,  path, attributes
(define-nfs-proc symlink ((dirfh fhandle) 
			  (filename string)
			  (symlink string)
			  (sattr sattr))
  (with-permission (dirfh :write)
    ;; We don't do symlink
    (xdr-int *nfsdxdr* NFSERR_IO)))


(defun nfs-okay-to-write (fh cred)
  (when (= 1 (opaque-auth-flavor cred))
    (let* ((au (xdr-opaque-auth-struct-to-auth-unix-struct cred))
	   (uid (auth-unix-uid au)))
      (export-user-write-access-allowed-p (fh-export fh) uid))))

(defun nfs-okay-to-read (fh cred)
  (when (= 1 (opaque-auth-flavor cred))
    (let* ((au (xdr-opaque-auth-struct-to-auth-unix-struct cred))
	   (uid (auth-unix-uid au)))
      (export-user-read-access-allowed-p (fh-export fh) uid))))

;;; configuration program interface

(defun nfsd-get-config-file-path (peer xid cbody)
  (declare (ignore cbody))
  (when (= (socket:dotted-to-ipaddr "127.0.0.1") (rpc-peer-addr peer))
    (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf)
				      :create nil)
      (xdr-string *nfsdxdr* 
		  (if *configfile*
		      (namestring *configfile*)
		    "not set yet")))))

(defun nfsd-reload-configuration (peer xid cbody)
  (declare (ignore cbody))
  (when (= (socket:dotted-to-ipaddr "127.0.0.1") (rpc-peer-addr peer))
    (with-successful-reply (*nfsdxdr* peer xid (nfsd-null-verf)
				      :create nil)
      (format t "Reloading configuration file...~%")
      (read-nfs-cfg *configfile*)
      (xdr-unsigned-int *nfsdxdr* 1))))
      
      
      
