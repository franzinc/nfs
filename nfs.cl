;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2014 Franz Inc, Oakland, CA.  All rights reserved.
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

(in-package :user)

(eval-when (compile) (declaim (optimize (speed 3))))

(eval-when (compile load eval)
  (use-package :gen-nfs)
  (use-package :xdr)
  (require :pe)
  (require :autozoom))

(defparameter *socketbuffersize* (* 128 1024))
(defparameter *nfsd-start-time* nil)

;; Called by startem
(defun nfsd (start-gate)
  (setf *nfsd-start-time* (get-universal-time))

  (sunrpc:with-rpc-sockets ("NFS" usock tsock :port #.*nfs-port*)
    (sunrpc:with-portmapper-mappings ("NFS" #.*nfs-program*
					    '(2 3)
					    #.*nfs-port*
					    #.*nfs-port*)
      (mp:process-run-function "open file reaper" #'nfsd-open-file-reaper)
      (mp:process-run-function "attr cache reaper" #'attr-cache-reaper)
      (let ((server (sunrpc:make-rpc-server :udpsock usock :tcpsock tsock)))
	
	(dolist (sock (list usock tsock))
	  (socket:set-socket-options sock
				     :receive-buffer-size *socketbuffersize*
				     :send-buffer-size *socketbuffersize*))
	(announce "started")
	(if* *nfs-gc-debug*
	   then (logit-stamp "~&Turning on memory management debugging.~%")
		(setf (sys:gsgc-switch :print) t)
		(setf (sys:gsgc-switch :stats) t)
		;; This is the default setting.
		(setf *global-gc-behavior* :auto-and-warn)
	   else (setf (sys:gsgc-switch :print) nil))	  

	;; Notify caller that we've started
	(mp:open-gate start-gate)
	
	(loop
	  (sunrpc:rpc-receive-and-handle-message server
						 #'nfsd-message-handler))))))
	    

(defun nfsd-message-handler (xdr peer)
  (declare (optimize (speed 3)))
  (let ((msg (sunrpc:xdr-rpc-msg xdr)))
    (sunrpc:with-valid-call (msg peer cbody)
      (let ((xid (sunrpc:rpc-msg-xid msg))
	    (vers (sunrpc:call-body-vers cbody))
	    (proc (sunrpc:call-body-proc cbody)))
	
	;; sanity checks first
	(when (not (eq (sunrpc:call-body-prog cbody) #.*nfs-program*))
	  (if *nfs-debug*
	      (logit-stamp "~
NFS: ~a: Sending program unavailable response for prog=~D~%"
		     (sunrpc:peer-dotted peer)
		     (sunrpc:call-body-prog cbody)))
	  (sunrpc:send-prog-unavail-reply peer xid sunrpc:*nullverf*)
	  (return-from nfsd-message-handler))
	
	(case vers
	  (2
	   (case proc
	     (0 (nfsd-null peer xid xdr cbody))
	     (1 (nfsd-getattr peer xid xdr cbody))
	     (2 (nfsd-setattr peer xid xdr cbody))
	     ;; 3 = ROOT
	     (4 (nfsd-lookup peer xid xdr cbody))
	     (5 (nfsd-readlink peer xid xdr cbody))
	     (6 (nfsd-read peer xid xdr cbody))
	     ;; 7 = WRITECACHE
	     (8 (nfsd-write peer xid xdr cbody))
	     (9 (nfsd-create peer xid xdr cbody))
	     (10 (nfsd-remove peer xid xdr cbody))
	     (11 (nfsd-rename peer xid xdr cbody))
	     (12 (nfsd-link peer xid xdr cbody))
	     (13 (nfsd-symlink peer xid xdr cbody))
	     (14 (nfsd-mkdir peer xid xdr cbody))
	     (15 (nfsd-rmdir peer xid xdr cbody))
	     (16 (nfsd-readdir peer xid xdr cbody))
	     (17 (nfsd-statfs peer xid xdr cbody))
	     ;; secret functions used by the configuration program
	     (100 (nfsd-get-config-file-path peer xid xdr cbody))
	     (101 (nfsd-reload-configuration peer xid xdr cbody))
	     ;; secret function used by the console program
	     (102 (nfsd-get-console-port peer xid xdr cbody))
	     (t
	      (sunrpc:send-proc-unavail-reply peer xid sunrpc:*nullverf*)
	      (logit-stamp "NFSv2: ~a: unhandled procedure ~D~%" 
		     (sunrpc:peer-dotted peer)  proc))))
	  (3
	   (case proc
	     (0 (nfsd-null peer xid xdr cbody))
	     (1 (nfsd-getattr peer xid xdr cbody))
	     (2 (nfsd-setattr3 peer xid xdr cbody))
	     (3 (nfsd-lookup peer xid xdr cbody))
	     (4 (nfsd-access peer xid xdr cbody))
	     (5 (nfsd-readlink peer xid xdr cbody))
	     (6 (nfsd-read3 peer xid xdr cbody))
	     (7 (nfsd-write3 peer xid xdr cbody))
	     (8 (nfsd-create3 peer xid xdr cbody))
	     (9 (nfsd-mkdir peer xid xdr cbody))
	     (10 (nfsd-symlink3 peer xid xdr cbody))
	     (11 (nfsd-mknod peer xid xdr cbody))
	     (12 (nfsd-remove peer xid xdr cbody))
	     (13 (nfsd-rmdir peer xid xdr cbody))
	     (14 (nfsd-rename peer xid xdr cbody))
	     (15 (nfsd-link peer xid xdr cbody))
	     (16 (nfsd-readdir3 peer xid xdr cbody))
;;;;TODO: how to handle stream errors here?  like
;;;;      "Connection reset by peer" (errno 10054)
	     (17 (nfsd-readdirplus peer xid xdr cbody))
	     (18 (nfsd-fsstat peer xid xdr cbody))
	     (19 (nfsd-fsinfo peer xid xdr cbody))
	     (20 (nfsd-pathconf peer xid xdr cbody))
	     (21 (nfsd-commit peer xid xdr cbody))
	     (t
	      (sunrpc:send-proc-unavail-reply peer xid sunrpc:*nullverf*)
	      (logit-stamp "NFSv3: ~a: unhandled procedure ~D~%" 
		     (sunrpc:peer-dotted peer)
		     proc))))
	  (t
	   (if *nfs-debug*
	       (logit-stamp "NFS: ~a: Sending program version mismatch response~%"
		      (sunrpc:peer-dotted peer)))
	   (sunrpc:send-prog-mismatch-reply peer xid sunrpc:*nullverf* 2 3)
	   (return-from nfsd-message-handler)))))))

(defun log-backtrace-to-nfs-errlog (c)
  (with-open-file (f "sys:nfs-errlog.txt"
		   :direction :output
		   :if-does-not-exist :create
		   :if-exists :append)
    (format f "~a: Allegro NFS version ~a (commit ~a)~%" 
	    (excl.osi:ctime)
	    *nfsd-version*
	    *nfsd-commit-id*)
    (format f "Unexpected error: ~a~%" c)
    (top-level.debug:zoom f :count t)))

(defun get-error-code-from-condition (c)
  "Handles syscall-error and errno-stream-error"
  (typecase c
    (syscall-error
     (excl::syscall-error-errno c))
    (errno-stream-error 
     (slot-value c 'excl::code))))
     
(defun syscall-error-handler (c xdr savedpos vers debug-this-procedure)
  "Handles syscall-error and errno-stream-error"
  (let ((errno (get-error-code-from-condition c)))
    (setf (xdr:xdr-pos xdr) savedpos)
    
    ;; Make some errors look less alarming. 
    (when debug-this-procedure
      (case errno
	(#.*enoent* 
	 (logit " => [Not found]~%"))
	(#.*enotempty*
	 (logit " => [Not empty]~%"))
	(#.*eexist*
	 (logit " => [Already exists]~%"))
	(#.*enospc*
	 (logit " => [No space]~%"))
	(t
	 (logit "Handling file error: ~A~%" c))))
    
    
    (xdr-int xdr (map-errno-to-nfs-error-code errno))
    (if (= vers 3)
	(nfs-xdr-wcc-data xdr nil nil))))
  
(defmacro with-nfs-err-handler ((xdr vers) &body body)
  (let ((savepossym (gensym))
	(c (gensym))
	(out (gensym)))
    `(let ((,savepossym (xdr:xdr-pos ,xdr)))
       (tagbody
	 (handler-bind 
	     ((illegal-filename-error 
	       (lambda (,c)
		 (setf (xdr:xdr-pos ,xdr) ,savepossym)
		 (when debug-this-procedure (logit "Illegal filename~%"))
		 (ecase (mode ,c)
		   (:lookup 
		    ;; Filename is illegal so it will definitely not exist
		    (xdr-int ,xdr #.*nfserr-noent*))
		   (:create
		    (xdr-int ,xdr #.*nfserr-acces*))) ;; rfc1813 says to use this
		 (if (= ,vers 3)
		     (nfs-xdr-wcc-data ,xdr nil nil))
		 (go ,out)))
	      (syscall-error
	       (lambda (,c)
		 (syscall-error-handler ,c ,xdr ,savepossym ,vers debug-this-procedure)
		 (go ,out)))
	      (errno-stream-error
	       (lambda (,c)
		 (syscall-error-handler ,c ,xdr ,savepossym ,vers debug-this-procedure)
		 (go ,out)))
	      (error 
	       (lambda (,c)
		 (ignore-errors (log-backtrace-to-nfs-errlog ,c))
		 (setf (xdr:xdr-pos ,xdr) ,savepossym)
		 (logit-stamp "Handling unexpected error: ~a~%" ,c)
		 (xdr-int ,xdr #.*nfserr-io*)
		 (if (= ,vers 3)
		     (nfs-xdr-wcc-data ,xdr nil nil))
		 (go ,out))))
	   (progn ,@body))
	 ,out))))

(defmacro with-valid-fh ((xdr vers fhs) &body body)
  (let ((block (gensym))
	(fh (gensym)))
    `(block ,block
       (dolist (,fh (list ,@fhs))
	 (case ,fh
	   (:inval
	    (when debug-this-procedure (logit " Invalid file handle~%"))
	    (ecase ,vers
	      (2 
	       (xdr-int ,xdr #.*nfserr-stale*))
	      (3 
	       (xdr-int ,xdr #.*nfs3err-badhandle*)
	       (nfs-xdr-wcc-data ,xdr nil nil)))
	    (return-from ,block))
	   (:stale
	    (when debug-this-procedure (logit " Stale file handle~%"))
	    (xdr-int ,xdr #.*nfserr-stale*)
	    (if (= ,vers 3)
		(nfs-xdr-wcc-data ,xdr nil nil))
	    (return-from ,block))))
       ,@body)))


(defmacro with-allowed-host-access ((vers xdr fh addr) &body body)
  `(if* (export-host-access-allowed-p (fh-export ,fh) ,addr)
      then
	   ,@body
      else
	   (when debug-this-procedure (logit " Host access denied~%"))
	   (xdr-int ,xdr #.*nfserr-acces*)
	   (if (= ,vers 3)
	       (nfs-xdr-wcc-data ,xdr nil nil))))

;; Returns a timestamp as a float.  Note that this value may very well
;; be negative!  This is only intended to be used to compare relative
;; times.
(defun get-high-res-time ()
  (multiple-value-bind (secs usecs)
      (excl::acl-internal-real-time)
    (+ secs (* usecs #.(expt 10.0 -6)))))

(eval-when (compile load eval)
  (if (/= 1000 internal-time-units-per-second)
      (error "internal-time-units-per-second is not 1000.  define-nfs-proc macro will need to be adjusted since it assumes millisecond resolution")))

(defmacro define-nfs-proc (name arglist &body body)
  (let ((funcname (intern (format nil "~A-~A" 'nfsd name)))
	(first t)
	argdefs debugs fhsyms host-access-check-fh debugtype)
    
    (setf debugtype
      (let* ((x (symbol-name name))
	     (y (1- (length x))))
	(if (char= (schar x y) #\3)
	    (setf x (subseq x 0 y)))
	(intern x)))
    
    (macrolet ((add-debug (expr)
		 `(progn
		    (if (not first)
			(push '(logit ", ") debugs))
		    (push ,expr debugs))))
      (dolist (pair arglist)
	(ecase (second pair)
	  (fhandle 
	   (push (first pair) fhsyms)
	   (push `(,(first pair) (xdr-fhandle params vers)) argdefs)
	   (add-debug `(logit "~A" (if (fh-p ,(first pair))
				       (fh-pathname ,(first pair))
				     ,(first pair)))))
	  (string
	   (push `(,(first pair) (xdr-string params)) argdefs)
	   (add-debug `(logit "~A" ,(first pair))))
	  (filename
	   (push `(,(first pair) (xdr-string-utf8 params)) argdefs)
	   (add-debug `(logit "~A" ,(first pair))))
	  (unsigned
	   (push `(,(first pair) (xdr-unsigned-int params)) argdefs)
	   (add-debug `(logit "~D" ,(first pair))))
	  (access-mask
	   (push `(,(first pair) (xdr-unsigned-int params)) argdefs)
	   (add-debug `(logit "~a" (access-mask-to-string ,(first pair)))))
	  (uint64
	   (push `(,(first pair) (xdr-unsigned-hyper params)) argdefs)
	   (add-debug `(logit "~D" ,(first pair))))
	  (sattr
	   (push `(,(first pair) (xdr-tweaked-sattr params vers)) 
		 argdefs)
	   (add-debug `(logit "~A" (tweaked-sattr-to-string ,(first pair)))))
	  (sattrguard3 
	   (push `(,(first pair) 
		   (if (xdr-bool params)
		       (prog1 (unix-to-universal-time (xdr-unsigned-int params))
			 (xdr-unsigned-int params))))
		 argdefs)
	   (add-debug `(logit "~A" ,(first pair))))
	  (createhow3 
	   (push `(,(first pair) 
		   (ecase (xdr-unsigned-int params)
		     (0 ;; unchecked
		      (list :unchecked (xdr-tweaked-sattr params 3)))
		     (1 ;; guarded
		      (list :guarded (xdr-tweaked-sattr params 3)))
		     (2 ;; exclusive
		      (list :exclusive (xdr-unsigned-hyper params)))))
		 argdefs)
	   (add-debug `(logit "~a, ~a" (first ,(first pair))
			      (if* (eq (first ,(first pair)) :exclusive)
				 then (second ,(first pair))
				 else (tweaked-sattr-to-string 
				       (second ,(first pair)))))))
	  (data
	   (push `(,(first pair) (xdr-opaque-variable params)) argdefs)
	   (add-debug `(logit "<data>"))))
	(setf first nil)))
    (setf argdefs (reverse argdefs))
    (push '(logit ")") debugs)
    (setf debugs (reverse debugs))
    (setf host-access-check-fh (first fhsyms))
    (if (null host-access-check-fh)
	(error 
	 "define-nfs-proc:  There must be at least one fhandle variable"))
    `(defun ,funcname (peer xid params cbody)
       (let* ((vers (sunrpc:call-body-vers cbody))
	      procedure-start-time
	      debug-this-procedure
	      ,@argdefs)
	 (when (nfs-debug-filter-on ,debugtype)
	   (setf debug-this-procedure t)
	   (logit-stamp "NFSv~d: ~a: ~a(" 
			vers
			(sunrpc:peer-dotted peer)
			(quote ,name))
	   ,@debugs
	   (if *nfs-debug-timings*
	       (setf procedure-start-time (get-high-res-time))))
	 
	 (sunrpc:with-successful-reply (*nfsdxdr* peer xid sunrpc:*nullverf*)
	   
	   (with-nfs-err-handler (*nfsdxdr* vers)
	     (with-valid-fh (*nfsdxdr* vers ,fhsyms)
	       (with-allowed-host-access (vers *nfsdxdr* 
					       ,host-access-check-fh 
					       (sunrpc:rpc-peer-addr peer))
		 #+nfs-debug
		 (handler-bind
		     ((error
		       (lambda (c)
			 (when *nfs-debug*
			   (with-standard-io-syntax
			     (let ((*print-readably* nil)
				   (*print-miser-width* 40)
				   (*print-pretty* t)
				   (*print-structure* nil)
				   (*print-array* nil)
				   (tpl:*zoom-print-circle* t)
				   (tpl:*zoom-print-level* nil)
				   (tpl:*zoom-print-length* nil))
			       (ignore-errors
				(logit "Error: ~a.~%" c))
			       (ignore-errors ;prevent recursion
				(let* ((s *log-stream*)
				       (*terminal-io* s)
				       (*standard-output* s))
				  (tpl:do-command "zoom"
				    :brief t
				    :from-read-eval-print-loop nil
				    :count t :all t)))))))))
		   ,@body)
		 #-nfs-debug ,@body
		
		 (when debug-this-procedure
		   (when *nfs-debug-timings*
		     (let* ((end-time (get-high-res-time))
			    (elapsed-time (- end-time procedure-start-time)))
		       (logit " ==> ~,3f secs" elapsed-time)))
		   ;; Terminate general debug output
		   (logit "~%"))))))))))
  

(defmacro with-permission ((fh type &key op) &body body)
  (let ((func (ecase type
		(:read 'nfs-okay-to-read)
		(:write 'nfs-okay-to-write)))
	(failres3 (if* (eq op :link)
		     then `(progn
			     (nfs-xdr-post-op-attr *nfsdxdr* nil)
			     (nfs-xdr-wcc-data *nfsdxdr* nil nil))
		     else `(nfs-xdr-wcc-data *nfsdxdr* nil nil))))
    
    `(if* (not (,func ,fh (sunrpc:call-body-cred cbody)))
	then (if debug-this-procedure (logit " permission denied~%") )
	     (xdr-int *nfsdxdr* #.*nfserr-acces*)
	     (if (= vers 3)
		 ,failres3)
	else ,@body)))

;; fh must be a non-stale file handle
(defmacro with-dirfh ((fh &key op) &body body)
  (let ((attr (gensym "attr"))
	(failres3 (if* (eq op :link)
		     then `(progn
			     (nfs-xdr-post-op-attr *nfsdxdr* nil)
			     (nfs-xdr-wcc-data *nfsdxdr* nil nil))
		     else `(nfs-xdr-wcc-data *nfsdxdr* nil nil))))
    `(let ((,attr (lookup-attr ,fh)))
       (if* (= (nfs-attr-type ,attr) #.*nfdir*)
	  then ,@body
	  else (when debug-this-procedure (logit " Not a directory~%"))
	       (xdr-int *nfsdxdr* #.*nfserr-notdir*)
	       (if (= vers 3)
		   ,failres3)))))

;; fh must be a non-stale file handle
(defmacro with-non-dir-fh ((fh &key op) &body body)
  (let ((attr (gensym))
	(failres3 (if* (eq op :link)
		     then `(progn
			     (nfs-xdr-post-op-attr *nfsdxdr* nil)
			     (nfs-xdr-wcc-data *nfsdxdr* nil nil))
		     else `(nfs-xdr-wcc-data *nfsdxdr* nil nil))))

    `(let ((,attr (lookup-attr ,fh)))
       (if* (/= (nfs-attr-type ,attr) #.*nfdir*)
	  then  ,@body
	  else (when debug-this-procedure (logit " Not allowed to be a directory~%"))
	       (xdr-int *nfsdxdr* #.*nfserr-perm*)
	       (if (= vers 3)
		   ,failres3)))))


(defmacro with-same-export ((fh1 fh2 function) &body body)
  (let ((failres3 
	 (ecase function
	   (:rename
	    `(progn
	       (nfs-xdr-wcc-data *nfsdxdr* nil nil) ;; from
	       (nfs-xdr-wcc-data *nfsdxdr* nil nil))) ;; to
	   (:link
	    `(progn
	       (nfs-xdr-post-op-attr *nfsdxdr* nil)
	       (nfs-xdr-wcc-data *nfsdxdr* nil nil))))))
    
    `(if* (eq (fh-export ,fh1) (fh-export ,fh2))
	then ,@body
	else(when debug-this-procedure (logit " Not same export~%"))
	     (ecase vers
	       (2
		(xdr-int *nfsdxdr* #.*nfserr-acces*))
	       (3
		(xdr-int *nfsdxdr* #.*nfserr-xdev*)
		,failres3)))))
		 


(defmacro nfs-unsupported ()
  `(progn
     (when debug-this-procedure (logit " Unsupported~%"))
     (ecase vers
       (2 
	(xdr-int *nfsdxdr* #.*nfserr-io*))
       (3
	(xdr-int *nfsdxdr* #.*nfs3err-notsupp*)
	(nfs-xdr-wcc-data *nfsdxdr* nil nil)))))

;; This is good for nfstime and nfstime2 (where the second slot
;; is nanoseconds, not microseconds) because we don't use the 
;; second slot.
(defmacro nfstime-to-universal-time (nfstime)
  ;; We ignore the microseconds slot since universal time only has
  ;; 1-second resolution.
  `(unix-to-universal-time (nfstime-seconds ,nfstime)))

(defun tweak-sattr (s vers)
  (ecase vers
    (2 
     (macrolet ((tweak (slot)
		  `(if (= (,slot s) #xffffffff)
		       (setf (,slot s) nil)))
		(tweak-time (slot)
		  `(let ((x (,slot s)))
		     (if* (= (nfstime-seconds x) #xffffffff)
			then (setf (,slot s) nil)
			else (setf (,slot s) 
			       (nfstime-to-universal-time x))))))
       (tweak sattr-mode)
       (tweak sattr-uid)
       (tweak sattr-gid)
       (tweak sattr-size)
       (tweak-time sattr-atime)
       (tweak-time sattr-mtime)
       s))
    (3
     (macrolet ((getslot (slot)
		  (let ((reader 
			 (intern (format nil "sattr3-~a" slot)
				 :gen-nfs))
			(set-it
			 (intern (format nil "set-~a3-set-it" slot)
				 :gen-nfs))
			(reader2
			 (intern (format nil "set-~a3-~a" slot slot)
				 :gen-nfs)))
		    `(let ((x (,reader s)))
		       (if (,set-it x)
			   (,reader2 x)))))
		(get-time-slot (slot)
		  (let ((reader
			 (intern (format nil "sattr3-~a" slot)))
			(set-it
			 (intern (format nil "set-~a-set-it" slot)))
			(reader2
			 (intern (format nil "set-~a-~a" slot slot))))
		    `(let ((x (,reader s)))
		       (case (,set-it x)
			 (#.*set-to-client-time*
			  (nfstime-to-universal-time (,reader2 x)))
			 (#.*set-to-server-time*
			  (get-universal-time))
			 (t nil))))))

       (make-sattr :mode (getslot mode)
		   :uid (getslot uid)
		   :gid (getslot gid)
		   :size (getslot size)
		   :atime (get-time-slot atime)
		   :mtime (get-time-slot mtime))))))

(defun xdr-tweaked-sattr (xdr vers)
  (ecase vers
    (2 (tweak-sattr (xdr-sattr xdr) 2))
    (3 (tweak-sattr (xdr-sattr3 xdr) 3)))) 

(defun tweaked-sattr-to-string (s)
  (format nil "[Mode: ~o, uid: ~a, gid: ~a, size: ~a, atime: ~a, mtime: ~a]"
	  (sattr-mode s) (sattr-uid s) (sattr-gid s) (sattr-size s)
	  (sattr-atime s) (sattr-mtime s)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCEDURES 

(defun nfsd-null (peer xid params cbody)
  (declare (ignore params))
  (if (nfs-debug-filter-on null)
      (logit-stamp "NFSv~d: ~a: NULL~%" 
	     (sunrpc:call-body-vers cbody)
	     (sunrpc:peer-dotted peer)))

  (sunrpc:with-successful-reply (xdr peer xid sunrpc:*nullverf*)
    ))

(define-nfs-proc getattr ((fh fhandle))
  (xdr-int *nfsdxdr* #.*nfs-ok*)
  (nfs-xdr-fattr *nfsdxdr* fh vers))

#|
          struct fattr {
              ftype        type;  (int)
              unsigned int mode;
              unsigned int nlink;
              unsigned int uid;
              unsigned int gid;
              unsigned int size;
              unsigned int blocksize;
              unsigned int rdev;
              unsigned int blocks;
              unsigned int fsid;
              unsigned int fileid;
              timeval      atime;  (uint32 seconds, uint32 microseconds)
              timeval      mtime;
              timeval      ctime;
          };
|#

#|
      struct fattr3 {
         ftype3     type;  (int)
         mode3      mode;  (unsigned int)
         uint32     nlink; (unsigned int)
         uid3       uid;   (unsigned int)
         gid3       gid;   (unsigned int)
         size3      size;  (uint64)
         size3      used;  (uint64)
         specdata3  rdev;  (uint32 specdata1, uint32 specdata2)
         uint64     fsid;  (uint64)
         fileid3    fileid; (uint64)
         nfstime3   atime;  (uint32 seconds, uint32 nanoseconds)
         nfstime3   mtime; (uint32 seconds, uint32 nanoseconds)
         nfstime3   ctime; (uint32 seconds, uint32 nanoseconds)
      };
|#

(defun nfs-xdr-fattr (xdr fh vers)
  (let ((attr (lookup-attr fh))
	(exp  (fh-export   fh)))
    (xdr-int xdr (nfs-attr-type attr)) ;; type
    (xdr-unsigned-int xdr (logior (logand (nfs-attr-mode attr) 
					  (lognot (nfs-export-umask exp)))
				  (nfs-export-set-mode-bits exp))) ;; mode
    (xdr-unsigned-int xdr (nfs-attr-nlinks attr)) ;;nlinks
    (xdr-unsigned-int xdr (nfs-export-uid exp)) ;; uid
    (xdr-unsigned-int xdr (nfs-export-gid exp)) ;; gid
    (ecase vers
      (2 (xdr-unsigned-int xdr (nfs-attr-size attr)) ;; size
	 (xdr-unsigned-int xdr (nfs-attr-blocksize attr)) ;; blocksize
	 (xdr-unsigned-int xdr 0) ;; rdev
	 (xdr-unsigned-int xdr (nfs-attr-blocks attr)) ;;blocks
	 (xdr-unsigned-int xdr (nfs-attr-fsid attr)) ;;fsid
	 (xdr-unsigned-int xdr (nfs-attr-fileid attr))) ;;fileid
      (3 (xdr-unsigned-hyper xdr (nfs-attr-size attr)) ;; size
         (xdr-unsigned-hyper xdr (nfs-attr-used attr)) ;; used
	 (xdr-unsigned-int xdr 0) (xdr-unsigned-int xdr 0) ;; rdev
	 (xdr-unsigned-hyper xdr (nfs-attr-fsid attr)) ;; fsid
	 (xdr-unsigned-hyper xdr (nfs-attr-fileid attr)))) ;; fileid
    ;; timeval and nfstime3 types are the same in their first part
    ;; (uint32 seconds), but different in their second part
    ;; (v2: uint32 microseconds, v3: uint32 nanoseconds).  However,
    ;; we don't have precision better than one second, so, as far 
    ;; as we're concerned, the types are the same since we always
    ;; put 0 in the second part.
    (xdr-unsigned-int xdr (universal-to-unix-time (nfs-attr-atime attr)))
    (xdr-int xdr 0)
    (xdr-unsigned-int xdr (universal-to-unix-time (nfs-attr-mtime attr)))
    (xdr-int xdr 0)
    (xdr-unsigned-int xdr (universal-to-unix-time (nfs-attr-ctime attr)))
    (xdr-int xdr 0)))
    
(define-nfs-proc statfs ((fh fhandle))
  (with-permission (fh :read)
    (multiple-value-bind (non-priv-free priv-free total)
	(ignore-errors (unicode-get-filesystem-free-space (fh-pathname fh)))
      (if* (null non-priv-free)
	 then
	      (when debug-this-procedure (logit " I/O error~%"))
	      (xdr-int *nfsdxdr* #.*nfserr-io*)
	 else
	      ;; Convert to "blocks"
	      (setf non-priv-free (howmany non-priv-free *blocksize*))
	      (setf priv-free (howmany priv-free *blocksize*))
	      (setf total (howmany total *blocksize*))
	    
	      (xdr-int *nfsdxdr* #.*nfs-ok*) 
	      (xdr-unsigned-int *nfsdxdr* 8192)
	      (xdr-unsigned-int *nfsdxdr* *blocksize*)
	      (xdr-unsigned-int *nfsdxdr* total)
	      (xdr-unsigned-int *nfsdxdr* priv-free) 
	      (xdr-unsigned-int *nfsdxdr* non-priv-free)))))

;; v3 only.
;; Allowable errors:
;; NFS3ERR_IO 
;; NFS3ERR_STALE 
;; NFS3ERR_BADHANDLE 
;; NFS3ERR_SERVERFAULT

#|
       struct FSSTAT3resok {
           post_op_attr obj_attributes;
           size3        tbytes;
           size3        fbytes;
           size3        abytes;
           size3        tfiles;
           size3        ffiles;
           size3        afiles;
           uint32       invarsec;
      };
|#

(define-nfs-proc fsstat ((fh fhandle))
  (multiple-value-bind (non-priv-free priv-free total)
      (ignore-errors (unicode-get-filesystem-free-space (fh-pathname fh)))
    (if* (null non-priv-free)
       then
	    (when debug-this-procedure (logit " I/O error~%"))
	    (xdr-int *nfsdxdr* #.*nfserr-io*)
	    (nfs-xdr-wcc-data *nfsdxdr* nil nil)
       else
	    (xdr-int *nfsdxdr* #.*nfs-ok*)
	    (nfs-xdr-post-op-attr *nfsdxdr* fh)
	    (xdr-unsigned-hyper *nfsdxdr* total)
	    (xdr-unsigned-hyper *nfsdxdr* priv-free)
	    (xdr-unsigned-hyper *nfsdxdr* non-priv-free)
	    (xdr-unsigned-hyper *nfsdxdr* -1) ;; tfiles
	    (xdr-unsigned-hyper *nfsdxdr* -1) ;; ffiles
	    (xdr-unsigned-hyper *nfsdxdr* -1) ;; afiles
	    (xdr-unsigned-int *nfsdxdr* 0)))) ;; invarsec

;; v3 only
;; allowable errors:
;;      NFS3ERR_STALE
;;      NFS3ERR_BADHANDLE
;;      NFS3ERR_SERVERFAULT

#|
      struct FSINFO3resok {
           post_op_attr obj_attributes;
           uint32       rtmax;
           uint32       rtpref;
           uint32       rtmult;
           uint32       wtmax;
           uint32       wtpref;
           uint32       wtmult;
           uint32       dtpref;
           size3        maxfilesize;
           nfstime3     time_delta;
           uint32       properties;
      };
|#

(define-nfs-proc fsinfo ((fh fhandle))
  (xdr-int *nfsdxdr* #.*nfs-ok*)
  (nfs-xdr-post-op-attr *nfsdxdr* fh)
  (xdr-unsigned-int *nfsdxdr* 65536) ;; rtmax
  (xdr-unsigned-int *nfsdxdr* 65536) ;; rtpref
  (xdr-unsigned-int *nfsdxdr* 512) ;; rtmult
  (xdr-unsigned-int *nfsdxdr* 65536) ;; wtmax
  (xdr-unsigned-int *nfsdxdr* 65536) ;; wtpref
  (xdr-unsigned-int *nfsdxdr* 512) ;; wtmult
  (xdr-unsigned-int *nfsdxdr* 65536) ;; dtpref
  (xdr-unsigned-hyper *nfsdxdr* #.(1- (ash 1 63))) ;; maxfilesize
  ;; time_delta.  Indicate that we only keep time to the nearest
  ;; two seconds which is the case for FAT filesystems.  NTFS
  ;; is 10 milliseconds.  
  (xdr-unsigned-int *nfsdxdr* 2) (xdr-unsigned-int *nfsdxdr* 0)
  ;; Can set time on files
  ;; homogeneous (all files have same pathconf information)
  ;; hard links supported
  ;; symbolink links supported 
  (xdr-unsigned-int *nfsdxdr* 
		    #.(logior *fsf3-link*
			      *fsf3-symlink*
			      *fsf3-homogeneous*
			      *fsf3-cansettime*)))


;;; readdirargs:  fhandle dir, cookie, count
(define-nfs-proc readdir ((fh fhandle) (cookie unsigned) (count unsigned))
  (with-dirfh (fh)
    (with-permission (fh :read)
      (add-direntries *nfsdxdr* fh count count cookie 2 nil nil)
      (update-attr-atime fh))))


;; cookieverf is 8 bytes of data.  We use those 8 bytes as a uint64.
;; Allowable errors:
;;      NFS3ERR_IO
;;      NFS3ERR_ACCES
;;      NFS3ERR_NOTDIR
;;      NFS3ERR_BAD_COOKIE
;;      NFS3ERR_TOOSMALL
;;      NFS3ERR_STALE
;;      NFS3ERR_BADHANDLE
;;      NFS3ERR_SERVERFAULT

;; RFC183: "count" is the maximum size of the
;; READDIR3resok structure, including all XDR overhead.
(define-nfs-proc readdir3 ((dirfh fhandle) 
			   (cookie uint64) 
			   (cookieverf uint64)
			   (count unsigned))
  (with-dirfh (dirfh)
    (with-permission (dirfh :read)
      (add-direntries *nfsdxdr* dirfh count count cookie 3 cookieverf nil)
      (update-attr-atime dirfh))))

;;; RFC1813: 
;;;      dircount
;;;         The maximum number of bytes of directory information
;;;         returned. This number should not include the size of
;;;         the attributes and file handle portions of the result.
;;;
;;;      maxcount
;;;         The maximum size of the READDIRPLUS3resok structure, in
;;;         bytes. The size must include all XDR overhead.
(define-nfs-proc readdirplus ((dirfh fhandle)
			      (cookie uint64)
			      (cookieverf uint64)
			      (dircount unsigned)
			      (maxcount unsigned)) 
  (with-dirfh (dirfh)
    (with-permission (dirfh :read)
      (add-direntries *nfsdxdr* dirfh dircount maxcount cookie 3 cookieverf t)
      (update-attr-atime dirfh))))

;; totalbytesadded starts at 8 because in the minimal case, we need to
;; be able to add the final 0 discriminant.. and the eof indicator

;; DIRMAX is the maximum number of bytes of directory information
;; to return (not including attributes and file handle info).
;; MAXCOUNT is the maximum size of the READDIRPLUS3resok structure, including
;; XDR overhead.
(defun add-direntries (xdr dirfh dirmax maxcount startindex vers verf plus)
  (declare (optimize (speed 3)))
  
  ;; rfe15117: Some NFS clients using UDP may request a result size
  ;; that is larger than the max UDP packet size, so trim MAXCOUNT to
  ;; avoid issues.
  (let ((udp-limit (- *max-udp-datagram-size* (sunrpc:get-successful-reply-overhead))))
    (setf maxcount (min maxcount udp-limit)))
  
  (multiple-value-bind (dirvec dc)
      (nfs-lookup-dir dirfh t)
    (declare (simple-vector dirvec))
    (let ((index startindex)
	  (totalbytesadded 8)
	  (dirbytesadded 0)
	  (complete t)
	  (entries 0)
	  bytesadded
	  innerbytesadded
	  endindex
	  p
	  debug)
      (declare (fixnum entries totalbytesadded dirbytesadded index
		       bytesadded innerbytesadded))
      
      (when (nfs-debug-filter-on readdir)
	(setf debug *nfs-debug*)
	
	(if (eq debug :verbose)
	    (logit "~%")))
      
      ;; HP-UX doesn't do the cookie verifier properly so don't
      ;; complain if we get a verifier of 0.
      (when (and (= vers 3) (/= startindex 0) (/= verf 0)
		 (/= verf (dircache-id dc))
		 ;; [rfe15123] Handle reversed readdir cookie verifier
		 (/= (bswap64 verf) (dircache-id dc)))
	(when debug (logit " Bad cookie~%"))
	(xdr-int xdr #.*nfs3err-bad-cookie*)
	(nfs-xdr-post-op-attr xdr dirfh)
	(return-from add-direntries))
	
      (xdr-int xdr #.*nfs-ok*)
	
      ;; readdirresok begins here.  This is what is subject to the
      ;; 'count' (v2) or 'maxcount' (v3) limit.
      (when (= vers 3)
	(incf totalbytesadded 
	      (with-xdr-compute-bytes-added (xdr)
		(nfs-xdr-post-op-attr xdr dirfh)
		(xdr-unsigned-hyper xdr (dircache-id dc)))))
      
      (setf endindex (length dirvec))
	
      (while (< index endindex)
	(setf p (aref dirvec index))
	  
	(when p ;; some entries can be nil (due to removal by client)
	  (multiple-value-setq (bytesadded innerbytesadded)
	    (make-direntry-xdr xdr dirfh p (1+ index) vers plus))
	  
	  (incf totalbytesadded bytesadded)
	  (incf dirbytesadded innerbytesadded)
	  
	  (when (or (> totalbytesadded maxcount)
		    (> dirbytesadded dirmax))
	    (if (eq debug :verbose) 
		(logit " [not added due to size overflow]~%"))
	    (xdr:xdr-backspace xdr bytesadded)
	    (setf complete nil)
	    (return)) ;; break from loop

	  (incf entries)
	  (if (eq debug :verbose) (logit " [added]~%")))
	  
	(incf index))
      ;; end while
	
      (xdr-int xdr 0) ;; no more entries
      (xdr-bool xdr complete)
      (when debug 
	(logit " ~d entries" entries)
	(if complete
	    (logit " EOF"))))))


#|
     struct entry {
          unsigned  fileid;
          filename  name;
          nfscookie cookie;
          entry     *nextentry;
      };

      struct entry3 {
           fileid3      fileid;  (uint64)
           filename3    name;
           cookie3      cookie;  (uint64)
           entry3       *nextentry;
      };

      struct entryplus3 {
           fileid3      fileid;
           filename3    name;
	   cookie3      cookie;
	   /* Same as entry3 up to this point */
           post_op_attr name_attributes;
           post_op_fh3  name_handle;
           entryplus3   *nextentry;
      };

|#

;; returns 
;; 1) The number of bytes added to xdr
;; 2) The number of bytes of directory information added.
;;    This includes the "entry follows" indicator, 
;;    the fileid, the filename, and the cookie.
(defun make-direntry-xdr (xdr dirfh filename cookie vers plus)
  (let* ((fh (lookup-fh-in-dir dirfh filename 
			       :allow-dotnames t))
	 (fileid (fh-file-id fh))
	 inner-bytes-added)
    (when (and (nfs-debug-filter-on readdir) (eq *nfs-debug* :verbose))
      (logit "~D: ~A fileid=~A next=~A" 
	     (1- cookie) filename fileid cookie))
    (values
     ;; First return value is the result of xdr:with-xdr-compute-bytes-added.
     (xdr:with-xdr-compute-bytes-added (xdr)
       (setf inner-bytes-added
	 (xdr:with-xdr-compute-bytes-added (xdr)
	   (xdr-int xdr 1) ;; indicate that data follows
	   (ecase vers
	     (2 (xdr-unsigned-int xdr fileid))
	     (3 (xdr-unsigned-hyper xdr fileid)))
	   (xdr-string-utf8 xdr filename)
	   (ecase vers
	     (2 (xdr-int xdr cookie))
	     (3 (xdr-unsigned-hyper xdr cookie)))))
       (when plus
	 (nfs-xdr-post-op-attr xdr fh)
	 (nfs-xdr-post-op-fh xdr fh)))
     ;; Second return value
     inner-bytes-added)))

;; Like lookup-fh-in-dir but returns nil if no such file or directory.
(defun nfs-probe-file (dirfh filename &key allow-dotnames)
  (handler-bind 
      ((syscall-error 
	(lambda (e)
	  (when (eq (syscall-error-errno e) *enoent*)
	    (return-from nfs-probe-file nil)))))
    (lookup-fh-in-dir dirfh filename :allow-dotnames allow-dotnames)))

;; v3 allowable errors: 
;;       NFS3ERR_IO
;;      NFS3ERR_NOENT
;;      NFS3ERR_ACCES
;;      NFS3ERR_NOTDIR
;;      NFS3ERR_NAMETOOLONG
;;     NFS3ERR_STALE
;;     NFS3ERR_BADHANDLE
;;      NFS3ERR_SERVERFAULT

;;; v2 returns: status fhandle attributes (fattr)
;;  v3 returns: status fhandle, obj post-op attributes, dir post-op attributes

(define-nfs-proc lookup ((dirfh fhandle) (filename filename))
  (with-dirfh (dirfh)
    (with-permission (dirfh :read)
      (let ((fh (lookup-fh-in-dir dirfh filename :allow-dotnames t)))
	(xdr-int *nfsdxdr* #.*nfs-ok*)
	(xdr-fhandle *nfsdxdr* vers fh)
	(ecase vers
	  (2 
	   (nfs-xdr-fattr *nfsdxdr* fh 2))
	  (3 
	   (nfs-xdr-post-op-attr *nfsdxdr* fh)
	   (nfs-xdr-post-op-attr *nfsdxdr* dirfh)))))))


;; v3 only
;;      const ACCESS3_READ    = 0x0001;
;;      const ACCESS3_LOOKUP  = 0x0002;
;;      const ACCESS3_MODIFY  = 0x0004;
;;      const ACCESS3_EXTEND  = 0x0008;
;;      const ACCESS3_DELETE  = 0x0010;
;;      const ACCESS3_EXECUTE = 0x0020;

;; Allowable errors:
;;      NFS3ERR_IO
;;      NFS3ERR_STALE
;;      NFS3ERR_BADHANDLE
;;      NFS3ERR_SERVERFAULT

(defun access-mask-to-string (value)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (fixnum value))
  (let (res)
    (macrolet ((check (sym)
		 (let ((name (subseq (symbol-name sym) 
				     (1+ (length "access3-"))
				     (1- (length (symbol-name sym))))))
		   `(if (not (zerop (logand value ,sym)))
			(push ,name res)))))
      (check *access3-read*)
      (check *access3-lookup*)
      (check *access3-modify*)
      (check *access3-extend*)
      (check *access3-delete*)
      (check *access3-execute*))
    (if* res
       then (list-to-delimited-string (nreverse res) #\|)
       else "NONE")))

(define-nfs-proc access ((fh fhandle) (access access-mask))
  (xdr-int *nfsdxdr* #.*nfs-ok*)
  (nfs-xdr-post-op-attr *nfsdxdr* fh)
  (let ((res 0)
	(cred (sunrpc:call-body-cred cbody)))
    (declare (fixnum res))
    (if (nfs-okay-to-read fh cred)
	(setf res 
	  (logior res #.(logior *access3-read* *access3-lookup* *access3-execute*))))
    (if (nfs-okay-to-write fh cred)
	(setf res 
	  (logior res #.(logior *access3-modify* *access3-extend* *access3-delete*))))
    
    ;; Some clients (Solaris 7 in particular) get upset if we return
    ;; more detailed access information than requested, so use logand
    ;; to only supply the requested bits.
    (setf res (logand res access))
    
    (if debug-this-procedure
	(logit " ==> ~a" (access-mask-to-string res)))
    
    (xdr-unsigned-int *nfsdxdr* res)))

;; readargs: fhandle, offset, count, totalcount
(define-nfs-proc read ((fh fhandle) (offset unsigned) (count unsigned)
				    (totalcount unsigned))
  (with-permission (fh :read)
    (with-nfs-open-file (f fh :input)
      (file-position f offset)
      ;; 72 = sizeof(fattr)+sizeof(*nfs-ok*)
      ;; 72 =       68     +   4
      (xdr:with-xdr-seek (*nfsdxdr* 72) 
	(xdr-opaque-variable-from-stream *nfsdxdr* f count))
      (xdr-int *nfsdxdr* #.*nfs-ok*) 
      (update-attr-atime fh)
      (nfs-xdr-fattr *nfsdxdr* fh 2))))

(define-nfs-proc read3 ((fh fhandle) (offset uint64) (count unsigned))
  (with-permission (fh :read)
    (with-non-dir-fh (fh)
      (with-nfs-open-file (f fh :input)
	(let (got)
	  (file-position f offset)
	  (with-xdr-seek (*nfsdxdr* 100)
	    (setf got (xdr-opaque-variable-from-stream *nfsdxdr* f count)))
	  (if debug-this-procedure (logit " (read ~d bytes)" got))
	  (update-attr-atime fh)
	  (xdr-int *nfsdxdr* #.*nfs-ok*) 
	  (nfs-xdr-post-op-attr *nfsdxdr* fh)
	  (xdr-unsigned-int *nfsdxdr* got)
	  (xdr-bool *nfsdxdr* (= (file-position f) (file-length f))))))))


;; args: fhandle dir, filename, sattr 
;; returns: status, fhandle, attributes (if okay status)

;; Most of the supplied attributes are ignored.

;; rfc1094 is silent on what should happen if the file in question
;; already exists.  However, Linux and Solaris (and probably the rest)
;; consider it a non-error situation and return success.  The Open
;; Group document "Protocols for Interworking: XNFS, Version 3W" does
;; explicitly state that CREATE should treat an already-existing file
;; as a successful situation.  If the file already exists, linux
;; ignores all the supplied attributes except for the 'size'.  I'll do
;; mostly the same here.  I accept atime/mtime updates as well. 

(define-nfs-proc create ((dirfh fhandle) (filename filename) (sattr sattr))
  (with-permission (dirfh :write)
    (let ((newpath (add-filename-to-dirname (fh-pathname dirfh) filename :create)))
      ;; Create the file if necessary 
      (close (unicode-open newpath :direction :output :if-exists :overwrite))
      (update-atime-and-mtime dirfh)
      (nfs-add-file-to-dir filename dirfh)
      
      (let ((fh (lookup-fh-in-dir dirfh filename)))
	(set-file-attributes fh sattr)
	
	(xdr-int *nfsdxdr* #.*nfs-ok*)
	(xdr-fhandle *nfsdxdr* 2 fh)
	(nfs-xdr-fattr *nfsdxdr* fh 2)))))

(define-nfs-proc create3 ((dirfh fhandle) 
			  (filename filename) 
			  (how createhow3))
  (with-permission (dirfh :write)
    (let* ((pre-op-attrs (get-pre-op-attrs dirfh))
	   (newpath (add-filename-to-dirname (fh-pathname dirfh) filename :create))
	   ;; attributes only supplied for unchecked or guarded mode.
	   (sattr (if (sattr-p (second how)) (second how)))
	   fh
	   created)

      (flet ((create-the-file (if-exists verifier)
	       (close (unicode-open newpath :direction :output :if-exists if-exists))
	       (setf created t)
	       (setf fh (lookup-fh-in-dir dirfh filename))
	       (setf (fh-verifier fh) verifier)))
	     
	(ecase (first how)
	  (:unchecked
	   (create-the-file :overwrite nil))
	  (:guarded
	   ;; the error handler will handle the *eexist* case.
	   (create-the-file :error nil))
	  (:exclusive
	   ;; XXX - The verifier information is supposed to be stored in
	   ;; stable storage but we don't do that because of how 
	   ;; gross Windows filesystem semantics are with respect to
	   ;; time resolution.  Instead, we fake it by storing the verifier
	   ;; in the file handle.
	   (let ((verifier (second how)))
	     (setf fh (nfs-probe-file dirfh filename)) ;; nil if file doesn't exist yet
	     (if* (and fh (eql verifier (fh-verifier fh)))
		then ;; Must be a duplicate request.  Report success.
		     (if debug-this-procedure 
			 (logit " Duplicate request~%"))
		else ;; the error handler will handle the *eexist* case
		     (create-the-file :error verifier))))))

      (when created
	(update-atime-and-mtime dirfh)
	(nfs-add-file-to-dir filename dirfh))

      (if sattr
	  (set-file-attributes fh sattr))
      
      (if debug-this-procedure
	  (logit " ==> OK"))

      (xdr-int *nfsdxdr* #.*nfs-ok*)
      (nfs-xdr-post-op-fh *nfsdxdr* fh)
      (nfs-xdr-post-op-attr *nfsdxdr* fh)
      (nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs dirfh))))

;; Due to the arrangement of data structures, hard links which are
;; not in the same directory are not supported.  
(defmacro with-same-dir-fh ((dirfh1 dirfh2) &body body)
  `(if* (eq ,dirfh1 ,dirfh2)
      then ,@body
      else (if debug-this-procedure
	       (logit "=> [hard links in separate directories not supported]"))
	   (ecase vers
	     (2 
	      (xdr-int *nfsdxdr* #.*nfserr-io*))
	     (3
	      (xdr-int *nfsdxdr* #.*nfs3err-notsupp*)
	      (nfs-xdr-post-op-attr *nfsdxdr* nil)
	      (nfs-xdr-wcc-data *nfsdxdr* nil nil)))))

(define-nfs-proc link ((fh fhandle) (destdirfh fhandle) 
				    (destfilename filename))
  ;; Many sanity checks to prevent corruption
  (with-permission (destdirfh :write :op :link)
    (with-same-export (fh destdirfh :link)
      (with-non-dir-fh (fh :op :link)
	(with-dirfh (destdirfh :op :link)
	  (with-same-dir-fh ((fh-parent fh) destdirfh)
	    (let* ((newpath 
		    (add-filename-to-dirname (fh-pathname destdirfh) destfilename :create))
		   (pre-op-attrs (get-pre-op-attrs destdirfh)))
	      
	      ;; Perform the actual hard link operation
	      (unicode-link (fh-pathname fh) newpath)

	      ;; Update file handle information
	      (update-alternate-pathnames fh :add destfilename)
	      (link-fh-in-dir fh destdirfh destfilename)

	      ;; Update other info
	      (update-atime-and-mtime destdirfh)
	      (nfs-add-file-to-dir destfilename destdirfh) ;; update dircache
	      (incf-cached-nlinks fh)
	      ;; need to incf nlinks for the original file handle (which should
	      ;; affect all links).  One easy thing would be to just 
	      ;; de-cache fh attrs.. but that's excessive.

	      (xdr-int *nfsdxdr* #.*nfs-ok*)
	      (when (= vers 3)
		(nfs-xdr-post-op-attr *nfsdxdr* fh)
		(nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs destdirfh)))))))))

;; args:
;; fhandle dir
;; filename name

;; returns status
(define-nfs-proc remove ((dirfh fhandle) (filename filename))
  ;; lookup-fh-in-dir will throw an error if file doesn't exist
  (let ((pre-op-attrs (get-pre-op-attrs dirfh))
	(fh (lookup-fh-in-dir dirfh filename)) 
	(path (add-filename-to-dirname (fh-pathname dirfh) filename :lookup)))
    (with-permission (dirfh :write)
      (if* (eq (close-open-file fh :check-refcount t) :still-open)
	 then ;; Unfortunately there is no NFS error code that indicates
	      ;; that a file is in use.  Use EPERM instead.
	      (xdr-int *nfsdxdr* #.*nfserr-perm*)
	 else  ;; Don't use (fh-pathname fh) since we may be dealing with 
	      ;; a hard link.  Use the filename provided instead.
	      (unicode-unlink path)
	      (update-atime-and-mtime dirfh)
	      ;; Update dircache.
	      (nfs-remove-file-from-dir filename dirfh)
	      (remove-fhandle fh filename)
	      (uncache-attr fh)
	      (xdr-int *nfsdxdr* #.*nfs-ok*))
      (if (= vers 3)
	  (nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs dirfh)))))

;; args: fhandle, beginoffset, offset, totalcount, data
;;; returns:
;;; status
;;; attributes (if no error)
(define-nfs-proc write ((fh fhandle) (beginoffset unsigned)
				     (offset unsigned)
				     (totalcount unsigned)
				     (data data))
  (with-permission (fh :write)
    (with-nfs-open-file (f fh :output)
      (file-position f offset)
      (write-sequence (opaque-vec data)
		      f
		      :start (opaque-offset data)
		      :end (+ (opaque-offset data) (+ (opaque-len data))))
      (finish-output f)
      
      (update-attr-times-and-size f fh *nfs-set-mtime-on-write*)
      (xdr-int *nfsdxdr* #.*nfs-ok*)
      (nfs-xdr-fattr *nfsdxdr* fh 2))))

#|
      enum stable_how {
           UNSTABLE  = 0,
           DATA_SYNC = 1,
           FILE_SYNC = 2
      };
      |#

(define-nfs-proc write3 ((fh fhandle) 
			 (offset uint64)
			 (count unsigned)
			 (stable-how unsigned)
			 (data data))
  (with-permission (fh :write)
    (with-nfs-open-file (f fh :output)
      (let ((pre-op-attrs (get-pre-op-attrs fh))
	    (oo (opaque-offset data))
	    wrote)
	(file-position f offset)
	(setf wrote 
	  (- (write-vector (opaque-vec data)
			   f
			   :start oo
			   :end (+ oo count))
	     oo))
	(finish-output f)
	(update-attr-times-and-size f fh *nfs-set-mtime-on-write*)
	(if (> stable-how 0) 
	    (fsync f))
	(xdr-int *nfsdxdr* #.*nfs-ok*)
	(nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs fh)
	(xdr-unsigned-int *nfsdxdr* wrote)
	(xdr-unsigned-int *nfsdxdr* stable-how) 
	;;  typedef opaque writeverf3[NFS3_WRITEVERFSIZE];
	;; NFS3_WRITEVERFSIZE 8
	(xdr-unsigned-hyper *nfsdxdr* *nfsd-start-time*)))))


;; if offset=0 and count=0, then client is requesting a commit
;; of the entire file.
(define-nfs-proc commit ((fh fhandle) (offset uint64) (count unsigned))
  (with-permission (fh :write)
    (with-nfs-open-file (f fh :output)
      (fsync f)
      (xdr-unsigned-int *nfsdxdr* #.*nfs-ok*)
      (nfs-xdr-wcc-data *nfsdxdr* (get-pre-op-attrs fh) fh)
      (xdr-unsigned-hyper *nfsdxdr* *nfsd-start-time*)))) ;;verf
	  

;; args: fhandle, sattr
;; returns status and attributes.

(define-nfs-proc setattr ((fh fhandle) (sattr sattr))
  (with-permission (fh :write)
    (set-file-attributes fh sattr)
    ;; report success even if we didn't actually do it
    (xdr-int *nfsdxdr* #.*nfs-ok*)
    (nfs-xdr-fattr *nfsdxdr* fh vers)))

;; Used by setattr and create.
;; The only attributes which we support are atime, mtime and size.
(defun set-file-attributes (fh sattr)
  (let ((pathname     (fh-pathname fh))
	(current-attr (lookup-attr fh)))
    ;; only works on regular files.
    (when (= (nfs-attr-type current-attr) #.*nfreg*)

      (let ((newsize (sattr-size sattr))
	    (atime   (sattr-atime sattr))
	    (mtime   (sattr-mtime sattr)))
	    
	(when newsize
	  (unicode-truncate pathname newsize)
	  (set-cached-file-size fh newsize)
	  (update-atime-and-mtime fh))
	    
	(when (or atime mtime)
	  (unicode-utime pathname atime mtime))
	  
	(if atime
	    (setf (nfs-attr-atime current-attr) atime))
	(if mtime
	    (setf (nfs-attr-mtime current-attr) mtime))))))
      
;; build-only.  
;; size, mtime, ctime
(defun nfs-xdr-wcc-attr (xdr attrs)
  (xdr-unsigned-hyper xdr (first attrs))
  (xdr-unsigned-int xdr (universal-to-unix-time (second attrs)))
  (xdr-unsigned-int xdr 0)
  (xdr-unsigned-int xdr (universal-to-unix-time (third attrs)))
  (xdr-unsigned-int xdr 0))
  
;; xdr-bool, when building, returns its second arg
(defun nfs-xdr-pre-op-attr (xdr attrs)
  (declare (optimize (speed 3) (safety 0)))
  (if (xdr-bool xdr attrs)
      (nfs-xdr-wcc-attr xdr attrs)))
    
(defun nfs-xdr-wcc-data (xdr before after)
  (declare (optimize (speed 3) (safety 0)))
  (nfs-xdr-pre-op-attr xdr before)
  (nfs-xdr-post-op-attr xdr after))

(defun nfs-xdr-post-op-attr (xdr fh)
  (declare (optimize (speed 3) (safety 0)))
  (if (xdr-bool xdr fh)
      (nfs-xdr-fattr xdr fh 3)))

(defun nfs-xdr-post-op-fh (xdr fh)
  (declare (optimize (speed 3) (safety 0)))
  (if (xdr-bool xdr fh)
      (xdr-fhandle xdr 3 fh)))


;; args: fhandle, sattr3, sattrguard3
(define-nfs-proc setattr3 ((fh fhandle) (sattr sattr) (guard sattrguard3))
  (with-permission (fh :write)
    (let ((pre-op-attrs (get-pre-op-attrs fh)))
      (if* (and guard (/= guard (pre-op-attrs-ctime pre-op-attrs)))
	 then (when debug-this-procedure (logit " Guard time out of sync~%"))
	      (xdr-int *nfsdxdr* #.*nfs3err-not-sync*)
	      (nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs fh)
	 else (set-file-attributes fh sattr)
	      (xdr-int *nfsdxdr* #.*nfs-ok*)
	      (nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs fh)))))

;;; from:  fhandle dir, filename name
;;; to:    fhandle dir, filename name
;; Renames across exports are not allowed. 
;; If attempted:
;;  for NFSv3, we return *nfserr-xdev*.  
;;  for NFSv2, we return *nfserr-acces* (ala Linux)

(define-nfs-proc rename ((fromdirfh fhandle) 
			 (fromfilename filename)
			 (todirfh fhandle)
			 (tofilename filename))
  (with-same-export (fromdirfh todirfh :rename)
    (with-permission (fromdirfh :write)
      ;; not really necessary since they have to be the same export...
      ;; and the same exports always have the same permissions..
      ;; but leaving it this way to make it clearer.
      (with-permission (fromdirfh :write)
	(with-permission (todirfh :write)
	  ;; lookup-fh-in-dir will throw an error if the file doesn't
	  ;; exist.
	  (let* ((pre-op-attrs-from (get-pre-op-attrs fromdirfh))
		 (pre-op-attrs-to (get-pre-op-attrs todirfh))
		 (fromfh (lookup-fh-in-dir fromdirfh fromfilename))
		 ;; Use name provided by client (in case of hard link)
		 (from (add-filename-to-dirname (fh-pathname fromdirfh)
						fromfilename :lookup))
		 ;; See if the destination already exists.
		 (tofh (nfs-probe-file todirfh tofilename))
		 ;; Use name provided by client (in case of hard link)
		 (to (add-filename-to-dirname (fh-pathname todirfh)
					      tofilename :create)))
	    (if* (or 
		  (eq (close-open-file fromfh :check-refcount t) :still-open)
		  (and tofh (eq (close-open-file tofh :check-refcount t) :still-open)))
	       then (xdr-int *nfsdxdr* #.*nfserr-perm*)
	       else 
		    ;; This will auto-delete any existing destination
		    ;; file.
		    (my-rename from to :unicode t)
		    ;; If there was an existing destination file, remove
		    ;; its file handle information.
		    (when tofh
		      (remove-fhandle tofh tofilename)
		      (uncache-attr tofh))		      
		    (rename-fhandle fromfh fromfilename todirfh tofilename)
		    (uncache-attr fromfh)
		    (update-atime-and-mtime fromdirfh)
		    (update-atime-and-mtime todirfh)
		    ;; Update dir caches, if they're in use.
		    (nfs-remove-file-from-dir fromfilename fromdirfh)
		    (nfs-add-file-to-dir tofilename todirfh)
		    
		    #+ignore
		    (format t "after rename: ~s~%" fromfh)
		    
		    (xdr-int *nfsdxdr* #.*nfs-ok*))
	    (when (= vers 3)
	      (nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs-from fromdirfh)
	      (nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs-to todirfh))))))))

;;; args:  fhandle dir, filename, sattr
;;; returns: status
;;   fhandle, attributes (if okay status)
;; We don't use the supplied attributes.
(define-nfs-proc mkdir ((dirfh fhandle) (filename filename) (sattr sattr))
  (with-permission (dirfh :write)
    (let ((pre-op-attrs (get-pre-op-attrs dirfh))
	  (newpath (add-filename-to-dirname (fh-pathname dirfh) filename :create))
	  fh)
      (unicode-mkdir newpath)
      (setf fh (lookup-fh-in-dir dirfh filename))
      (nfs-add-file-to-dir filename dirfh)
      (update-atime-and-mtime dirfh)
      (if debug-this-procedure
	  (logit " ==> OK"))
      (xdr-int *nfsdxdr* #.*nfs-ok*)
      (ecase vers
	(2
	 (xdr-fhandle *nfsdxdr* 2 fh)	 
	 (nfs-xdr-fattr *nfsdxdr* fh 2))
	(3
	 (nfs-xdr-post-op-fh *nfsdxdr* fh)
	 (nfs-xdr-post-op-attr *nfsdxdr* fh)
	 (nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs dirfh))))))


(define-nfs-proc rmdir ((dirfh fhandle) (filename filename))
  (with-permission (dirfh :write)
    (let* ((pre-op-attrs (get-pre-op-attrs dirfh))
	   (fh (lookup-fh-in-dir dirfh filename))
	   (path (fh-pathname fh)))
      ;; rmdir system call doesn't return a decent error if the object
      ;; is not a directory (it returns "invalid argument", so check
      ;; here first.
      (if* (zerop (logand (unicode-stat path) excl::*s-ifdir*))
	 then (if debug-this-procedure
		  (logit " ==> [Not a directory]"))
	      (xdr-int *nfsdxdr* *nfserr-notdir*)
	 else (unicode-rmdir path) ;; throws error if non-empty
	      (update-atime-and-mtime dirfh)
	      (nfs-remove-file-from-dir filename dirfh)
	      (uncache-attr fh)
	      (remove-fhandle fh filename)
	      (if debug-this-procedure
		  (logit " ==> OK"))
	      (xdr-int *nfsdxdr* #.*nfs-ok*))
      (if (= vers 3)
	  (nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs dirfh)))))


(define-nfs-proc pathconf ((fh fhandle)) 
  (xdr-int *nfsdxdr* #.*nfs-ok*)
  (nfs-xdr-post-op-attr *nfsdxdr* fh)
  (xdr-unsigned-int *nfsdxdr* 1023) ;; linkmax (NTFS limit)
  (xdr-unsigned-int *nfsdxdr* 255) ;; name_max
  (xdr-bool *nfsdxdr* t) ;; no_trunc
  (xdr-bool *nfsdxdr* t) ;; chown_restricted;
  (xdr-bool *nfsdxdr* t) ;; case_insensitive;
  (xdr-bool *nfsdxdr* t)) ;; case_preserving

(define-nfs-proc readlink ((fh fhandle))
  (with-permission (fh :read)
    (let ((result (unicode-readlink (fh-pathname fh))))
      (xdr-int *nfsdxdr* #.*nfs-ok*)
      (if (eq vers 3)
	  (nfs-xdr-post-op-attr *nfsdxdr* fh))
      (if debug-this-procedure
	  (logit " ==> ~a" result))
      (xdr-filename *nfsdxdr* result))))

(define-nfs-proc symlink ((dirfh fhandle) 
			  (filename filename)
			  (symlink filename)
			  (sattr sattr))
  (with-permission (dirfh :write)
    (let ((newpath (add-filename-to-dirname (fh-pathname dirfh) filename :create)))
      (unicode-symlink symlink newpath)
      (update-atime-and-mtime dirfh)
      (nfs-add-file-to-dir filename dirfh)
      (if debug-this-procedure
	  (logit " ==> OK"))
      (xdr-int *nfsdxdr* #.*nfs-ok*))))

(define-nfs-proc symlink3 ((dirfh fhandle) 
			   (filename filename) 
			   (attrs sattr)
			   (symlink filename))
  (with-permission (dirfh :write)
    (let* ((pre-op-attrs (get-pre-op-attrs dirfh))
	   (newpath (add-filename-to-dirname (fh-pathname dirfh) filename :create)))
      (unicode-symlink symlink newpath)
      (update-atime-and-mtime dirfh)
      (nfs-add-file-to-dir filename dirfh)
      (if debug-this-procedure
	  (logit " ==> OK"))
      (let ((fh (lookup-fh-in-dir dirfh filename)))
	(xdr-int *nfsdxdr* #.*nfs-ok*)
	(nfs-xdr-post-op-fh *nfsdxdr* fh)
	(nfs-xdr-post-op-attr *nfsdxdr* fh)
	(nfs-xdr-wcc-data *nfsdxdr* pre-op-attrs dirfh)))))

;;; Unsupported section

(define-nfs-proc mknod ((dirfh fhandle) (filename filename))
  (with-permission (dirfh :write)
    (nfs-unsupported)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nfs-okay-to-write (fh cred)
  (declare (optimize speed (safety 0) (debug 0)))
  (and 
   ;; Check export permissions
   (let ((exp (fh-export fh)))
     (case (sunrpc:opaque-auth-flavor cred)
       (#.sunrpc:*auth-unix* 
	(xdr:with-opaque-xdr (xdr (sunrpc:opaque-auth-body cred))
	  (export-user-write-access-allowed-p 
	   exp
	   (sunrpc:auth-unix-uid (sunrpc:xdr-auth-unix xdr)))))
       (#.sunrpc:*auth-null*
	(export-user-write-access-allowed-p exp nil))))
   ;; Check mode bits.  Note that we're not considering
   ;; nfs-export-umask and nfs-export-set-mode-bits here.  Those are hacks
   ;; to provide a virtual reality to NFS clients.  We must consider actual reality here.
   (let* ((attr (lookup-attr fh))
	  (mode (nfs-attr-mode attr)))
     (logtest mode #o0200))))

  
(defun nfs-okay-to-read (fh cred)
  (declare (optimize speed (safety 0) (debug 0)))
  (and
   ;; Check export permissions
   (let ((exp (fh-export fh)))
     (case (sunrpc:opaque-auth-flavor cred)
       (#.sunrpc:*auth-unix* 
	(xdr:with-opaque-xdr (xdr (sunrpc:opaque-auth-body cred))
	  (export-user-read-access-allowed-p 
	   exp
	   (sunrpc:auth-unix-uid (sunrpc:xdr-auth-unix xdr)))))
       (#.sunrpc:*auth-null*
	(export-user-read-access-allowed-p exp nil))))
   ;; Check mode bits.  Note that we're not considering
   ;; nfs-export-umask and nfs-export-set-mode-bits here.  Those are hacks
   ;; to provide a virtual reality to NFS clients.  We must consider actual reality here.
   (let* ((attr (lookup-attr fh))
	  (mode (nfs-attr-mode attr)))
     (logtest mode #o0400))))

;;; configuration program interface

(defun nfsd-get-config-file-path (peer xid params cbody)
  (declare (ignore cbody params))
  (when (sunrpc:local-peer-p peer)
    (sunrpc:with-successful-reply (xdr peer xid sunrpc:*nullverf*)
      (xdr-string xdr (if* *configfile*
			 then (namestring *configfile*)
			 else "not set yet")))))

(defun nfsd-reload-configuration (peer xid params cbody)
  (declare (ignore cbody params))
  (when (sunrpc:local-peer-p peer)
    (sunrpc:with-successful-reply (xdr peer xid sunrpc:*nullverf*)
      (logit-stamp "Reloading configuration file...~%")
      (read-nfs-cfg *configfile*)
      (when mount:*showmount-disabled*
	(logit-stamp "Disabling export listing via showmount.~%"))
      (xdr-unsigned-int xdr 1))))

;; Console program interface 
(defun nfsd-get-console-port (peer xid params cbody)
  (declare (ignore cbody params))
  (when (sunrpc:local-peer-p peer)
    (sunrpc:with-successful-reply (xdr peer xid sunrpc:*nullverf*)
      (let ((listener *console-listener*))
	(if* listener
	   then (xdr-int xdr (socket:local-port listener))
	   else (xdr-int xdr 0))))))

