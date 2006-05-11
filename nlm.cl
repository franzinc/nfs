;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2005 Franz Inc, Oakland, CA.  All rights reserved.
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
;; $Id: nlm.cl,v 1.13 2006/05/11 21:58:59 dancy Exp $

;; This file implements the Network Lock Monitor (NLM) protocol. 

;; Ref: http://www.opengroup.org/onlinepubs/009629799/chap9.htm

(in-package :nlm)

(eval-when (compile load eval)
  (use-package :nsm))

(sunrpc:def-rpc-program (NLM 100021)
  (
   #+ignore
   (0 ;; version
     (1 nlm-sm-notify nlm-sm-status void)
     )
   (1 ;; version
     (1 nlm-test nlm-testargs nlm-testres)
     (2 nlm-lock nlm-lockargs nlm-res)
     (3 nlm-cancel nlm-cancargs nlm-res)
     (4 nlm-unlock nlm-unlockargs nlm-res)
     ;;(5 nlm-granted nlm-testargs nlm-res)

     (6 nlm-test-msg nlm-testargs void)
     (7 nlm-lock-msg nlm-lockargs void)
     (8 nlm-cancel-msg nlm-cancargs void)
     (9 nlm-unlock-msg nlm-unlockargs void)
     ;;(10 nlm-granted-msg nlm-testargs void)
     ;;(11 nlm-test-res nlm-testres void)
     ;;(12 nlm-lock-res nlm-res void)
     ;;(13 nlm-cancel-res nlm-res void)
     ;;(14 nlm-unlock-res nlm-res void)
     (15 nlm-granted-res nlm-res void)
     
     ;; extra, so that nsm can call us back when something has 
     ;; sent in a notify.
     (99 nlm-nsm-callback nsm-callback-status void)

   )
   (3 ;; version
    ;;(20 nlm-share nlm-shareargs nlm-shareres)
    ;;(21 nlm-unshare nlm-shareargs nlm-shareres)
    (22 nlm-nm-lock nlm-lockargs nlm-res)
    (23 nlm-free-all nlm-notify void)
   )
   (4 ;; version
     (1 nlm4-test nlm4-testargs nlm4-testres)
     (2 nlm4-lock nlm4-lockargs nlm4-res)
     (3 nlm4-cancel nlm4-cancargs nlm4-res)
     (4 nlm4-unlock nlm4-unlockargs nlm4-res)
     ;;(5 nlm4-granted nlm4-testargs nlm4-res)
     
     (6 nlm4-test-msg nlm4-testargs void)
     (7 nlm4-lock-msg nlm4-lockargs void)
     (8 nlm4-cancel-msg nlm4-cancargs void)
     (9 nlm4-unlock-msg nlm4-unlockargs void)
     ;;(10 nlm4-granted-msg nlm4-testargs void)
     ;;(11 nlm4-test-res nlm4-testres void)
     ;;(12 nlm4-lock-res nlm4-res void)
     ;;(13 nlm4-cancel-res nlm4-res void)
     ;;(14 nlm4-unlock-res nlm4-res void)
     (15 nlm4-granted-res nlm4-res void)
     ;;(20 nlm4-share nlm4-shareargs nlm4-shareres)
     ;;(21 nlm4-unshare nlm4-shareargs nlm4-shareres)
     (22 nlm4-nm-lock nlm4-lockargs nlm4-res)
     (23 nlm4-free-all nlm-notify void)
   )
  ))

;;;;;;

(defparameter *nlm-gate* (mp:make-gate nil))
(defparameter *nlm-debug* nil)
(defparameter *nlm-retry-interval* 2) ;; seconds
(defparameter *nlm-grant-notify-interval* 30) ;; seconds

;; XXX - We do not do grace period stuff because when our NFS 
;; server restarts, all filehandles are invalid, so there would be
;; no way for a client to reclaim a lock anyway.

(defun NLM-init ()
  (mp:process-run-function "nlm retry loop" #'nlm-lock-retry-loop)
  (mp:process-run-function "nlm notify loop" #'nlm-grant-notify-loop)
  (mp:open-gate *nlm-gate*))

;; Helpers

(defun nlm-status-to-string (status)
  (case status
    (#.*nlm-granted* "GRANTED")
    (#.*nlm-denied* "DENIED")
    (#.*nlm-denied-nolocks* "DENIED_NOLOCKS")
    (#.*nlm-blocked* "BLOCKED")
    (#.*nlm-denied-grace-period* "DENIED_GRACE_PERIOD")
    (#.*nlm4-deadlck* "DEADLOCK")
    (#.*nlm4-rofs* "Read-only Filesystem")
    (#.*nlm4-stale-fh* "Stale filehandle")
    (#.*nlm4-fbig* "Offset or length too big")
    (#.*nlm4-failed* "FAILED")
    (t (format nil "~d" status))))

(defun nlm-log-status (status)
  (user::logit "NLM: ==> ~a~%" (nlm-status-to-string status)))

(defmacro if-nlm-v4 (vers form1 &optional form2)
  (let ((v (gensym)))
    `(let ((,v ,vers))
       (if (= ,v 4) ,form1 ,form2))))


(defmacro nlm-vers-to-nfs-vers (vers)
  `(if-nlm-v4 ,vers 3 2))

(defstruct (nlm-lock-internal
	    (:print-object nlm-lock-internal-printer))
  peer-addr ;; For retry locks (To send GRANTED message)
  vers ;; so we can make the right kind of callback
  cookie ;; cookie from the original lock request
  exclusive
  caller-name
  fh
  oh
  svid
  offset
  len)

(defun nlm-lock-internal-printer (obj stream)
  (format stream "(V~a, caller: ~a, file: ~a, pid: ~a, offset: ~a, len: ~a)"
	  (nlm-lock-internal-vers obj)
	  (nlm-lock-internal-caller-name obj)
	  (let ((fh (nlm-lock-internal-fh obj)))
	    (if* (user::fh-p fh)
	       then (user::fh-pathname fh)
	       else fh))
	  (nlm-lock-internal-svid obj)
	  (nlm-lock-internal-offset obj)
	  (nlm-lock-internal-len obj)))

(defun nlm-internalize-lock (lock exclusive vers addr cookie)
  (make-nlm-lock-internal 
   :peer-addr addr
   :vers vers
   :cookie (if cookie (opaque-data cookie))
   :exclusive exclusive
   :caller-name (nlm-lock-caller-name lock)
   :fh (if-nlm-v4 vers
		  (user::opaque-to-fhandle3 (nlm-lock-fh lock))
		  (xdr:with-opaque-xdr (xdr (nlm-lock-fh lock))
		    (user::xdr-fhandle xdr 2)))
   :oh (opaque-data (nlm-lock-oh lock))
   :svid (nlm-lock-svid lock)
   :offset (nlm-lock-l-offset lock)
   :len (nlm-lock-l-len lock)))

(defvar *nlm-state-lock* (mp:make-process-lock))
(defvar *nlm-locks* nil)
(defvar *nlm-retry-list* nil)
(defvar *nlm-notify-list* nil)

(defun nlm-lock-match-p (lock1 lock2)
  (and (eq (nlm-lock-internal-fh lock1) (nlm-lock-internal-fh lock2))
       (equalp (nlm-lock-internal-oh lock1) (nlm-lock-internal-oh lock2))
       (= (nlm-lock-internal-svid lock1) (nlm-lock-internal-svid lock2))
       (= (nlm-lock-internal-offset lock1) (nlm-lock-internal-offset lock2))
       (= (nlm-lock-internal-len lock1) (nlm-lock-internal-len lock2))))

(defun nlm-find-lock (lock list)
  (dolist (entry list)
    (if (nlm-lock-match-p lock entry)
	(return entry))))

(defun nlm-find-lock-by-cookie (cookie list)
  (dolist (entry list)
    (if (equalp (nlm-lock-internal-cookie entry) cookie)
	(return entry))))

;; Returns a list
(defun nlm-find-locks-by-addr (addr list)
  (let (res)
    (dolist (entry list)
      (if (= (nlm-lock-internal-peer-addr entry) addr)
	  (push entry res)))
    res))

(defun overlapped-p (start1 end1 start2 end2)
  (not
   (or
    ;; There are only two non-overlapping scenarios:
    ;; file1 ends before file2 begins.
    ;; file1 begins after file2 ends.
    ;; Anything outside of these two scenarios is an overlap situation.
    
    ;; Check for file1 ending before file2 begins.  
    (and (/= end1 0) (<= end1 start2))
    ;; Check for file1 beginning after file2 end.
    (and (/= end2 0) (>= start1 end2)))))

(defun nlm-find-overlapping-lock (fh offset length)
  (let ((start1 offset)
	(end1 (if* (= length 0)
		 then 0
		 else (+ offset length))))
    (dolist (entry *nlm-locks*)
      (if (eq (nlm-lock-internal-fh entry) fh)
	  (let* ((start2 (nlm-lock-internal-offset entry))
		 (end2 (+ start2 (nlm-lock-internal-len entry))))
	    (if (overlapped-p start1 end1 start2 end2)
		(return entry)))))))

;; XXX 
;; Windows has no concept of locking from "here to the current/future
;; of end of file" like Unix does.  This what is meant when length is
;; 0.  We simulate it here by using a very large length.  On 32-bit
;; platforms, the third arg to _locking is 32-bits so there may be
;; problems with locking large files.  We may need to provide an
;; interface to LockFile() which supports 64-bit args (in two
;; 32-bit pieces).

;; returns t if lock was successful, nil otherwise
(defun nlm-do-lock-1 (f offset length)
  (file-position f offset)
  (if (= length 0)
      (setf length #x7fffffff))
  (handler-case (excl.osi:locking f #.excl.osi:*lk-nblck* length)
    (syscall-error (c)
      (if* (= (syscall-error-errno c) #.excl.osi:*eacces*)
	 then nil
	 else (error c)))
    (:no-error (&rest args)
      (declare (ignore args))
      t)))

(defun nlm-do-lock (lock)
  (user::with-nfs-open-file (f (nlm-lock-internal-fh lock)
			 (if* (nlm-lock-internal-exclusive lock) 
			    then :output 
			    else :input)
			 :of of)
    (when (nlm-do-lock-1 f 
			 (nlm-lock-internal-offset lock)
			 (nlm-lock-internal-len lock))
      (incf (user::openfile-refcount of))
      t)))
      
(defun nlm-do-unlock-1 (f offset length)
  (file-position f offset)
  (if (= length 0)
      (setf length #x7fffffff))
  (excl.osi:locking f #.excl.osi:*lk-unlck* length))

(defun nlm-do-unlock (lock)
  (user::with-nfs-open-file (f (nlm-lock-internal-fh lock) :any :of of)
    (nlm-do-unlock-1 f 
		     (nlm-lock-internal-offset lock)
		     (nlm-lock-internal-len lock))
    (decf (user::openfile-refcount of))))
  

(defun nlm-try-lock (lock)
  (let ((vers (nlm-lock-internal-vers lock)))
    ;; Sanity check beforehand.  
    (if* (or (> (nlm-lock-internal-offset lock) #x7fffffff)
	     (> (nlm-lock-internal-len lock) #x7fffffff))
       then (if-nlm-v4 vers #.*nlm4-fbig* #.*nlm-denied-nolocks*)
       else (handler-case (nlm-do-lock lock)
	      (error (c)
		(user::logit "NLM: Unexpected error during lock call: ~a~%" c)
		(if-nlm-v4 vers #.*nlm4-failed* #.*nlm-denied-nolocks*))
	      (:no-error (success)
		(if* success
		   then #.*nlm-granted*
		   else #.*nlm-denied*))))))

;; There is no testing operation using the _locking interface
;; so we have to lock and, if that was succesful, unlock.
(defun nlm-do-test-lock (lock)
  (let ((status (nlm-try-lock lock)))
    (when (= status #.*nlm-granted*)
      (nlm-do-unlock lock))
    status))

;; A lock will either be on the retry list (meaning it hasn't been
;; granted yet) or on the notify list (lock has been granted but
;; the granted message hasn't been acknowledged yet).
(defun nlm-cancel-pending-retry (lock)
  (mp:with-process-lock (*nlm-state-lock*)
    (let (status)
      (let ((entry (nlm-find-lock lock *nlm-notify-list*)))
	(if* entry
	   then (if *nlm-debug*
		    (user::logit "NLM: Removing ~a from notify list.~%" entry))
		(setf *nlm-notify-list* (delete entry *nlm-notify-list*))
		(handler-case (nlm-do-unlock entry)
		  (error (c)
		    (user::logit "NLM: Unexpected error while unlocking ~a: ~a~%"
			   entry c)))))
      
      (let ((entry (nlm-find-lock lock *nlm-retry-list*)))
	(if* entry
	   then (if *nlm-debug*
		    (user::logit "NLM: Removing ~a from retry list.~%" entry))
		(setf *nlm-retry-list* (delete entry *nlm-retry-list*))
		(setf status #.*nlm-granted*)
	   else (setf status #.*nlm-denied-nolocks*)))
      
      (nlm-remove-monitoring (nlm-lock-internal-peer-addr lock))
      
      status)))
	      

;; HP/UX uses auth-null so we can't check against the username.
;; We just do host access checking.  
(defun nlm-access-ok (lock addr)
  (user::export-host-access-allowed-p 
   (user::fh-export (nlm-lock-internal-fh lock)) addr))

;; Procedures

;; LOCK

(defun nlm4-lock (arg vers peer cbody)
  (nlm-lock arg vers peer cbody))

(defun nlm-lock (arg vers peer cbody &key async (monitor t))
  (declare (ignore cbody))
  (let* ((exclusive (nlm-lockargs-exclusive arg))
	 (alock (nlm-lockargs-alock arg))
	 (cookie (nlm-lockargs-cookie arg))
	 (block (nlm-lockargs-block arg))
	 (reclaim (nlm-lockargs-reclaim arg))
	 (state (nlm-lockargs-state arg))
	 (lock (nlm-internalize-lock alock exclusive vers
				     (sunrpc:rpc-peer-addr peer)
				     cookie))
	 (fh (nlm-lock-internal-fh lock))
	 (addr (sunrpc:rpc-peer-addr peer))
	 (status (if-nlm-v4 vers #.*nlm4-failed* #.*nlm-denied-nolocks*)))
    
    (if *nlm-debug*
	(user::logit "~
NLM~a: ~a: LOCK~a (~a, block: ~a, excl: ~a, reclaim: ~a, state: ~a)~%"
	       (if-nlm-v4 vers "4" "")
	       (sunrpc:peer-dotted peer)
	       (if async "_MSG" "")
	       lock
	       block exclusive reclaim state))
    
    ;; XXX -- need proper synchronization to prevent concurrent
    ;; access to fhandles hash tables, and other relevant shared
    ;; structured.  openfile stuff has been modified but I still
    ;; need to check for stuff that calls close-open-file.

    (if (not monitor)
	(setf block nil))
    
    (if* (not (user::fh-p fh))
       then (if-nlm-v4 vers 
		       (setf status #.*nlm4-stale-fh*))
     elseif (not (nlm-access-ok lock addr))
       then (if *nlm-debug*
		(user::logit "NLM: ==> Access denied by configuration.~%"))
	    (setf status #.*nlm-denied*)
       else (mp:with-process-lock (*nlm-state-lock*)
	      (if* (nlm-find-lock lock *nlm-locks*)
		 then (setf status #.*nlm-granted*)
		 else (nlm-cancel-pending-retry lock)
		      (setf status (nlm-try-lock lock))
		
		      (if* (= status #.*nlm-granted*)
			 then (mp:with-process-lock (*nlm-state-lock*)
				(push lock *nlm-locks*))
			      (if monitor
				  (nlm-add-monitoring addr)))
		
		      (if* (and (= status #.*nlm-denied*) block)
			 then (setf status #.*nlm-blocked*)
			      (if *nlm-debug*
				  (user::logit "NLM: Lock unavailable. Adding ~a retry list.~%" lock))
			      (push lock *nlm-retry-list*)))))

    (if *nlm-debug*
	(nlm-log-status status))
    
    (make-nlm-res
     :cookie (nlm-lockargs-cookie arg)
     :stat (make-nlm-stat :stat status))))

;; UNLOCK

(defun nlm4-unlock (arg vers peer cbody)
  (nlm-unlock arg vers peer cbody))

(defun nlm-unlock (arg vers peer cbody &key async)
  (declare (ignore cbody))
  (let* ((addr (sunrpc:rpc-peer-addr peer))
	 (lock (nlm-internalize-lock (nlm-unlockargs-alock arg) nil vers
				     addr (nlm-unlockargs-cookie arg)))
	 (fh (nlm-lock-internal-fh lock))

	 ;; always say OK.  Doing otherwise makes linux log kernel
	 ;; messages.
	 (status #.*nlm-granted*)) 
    (if *nlm-debug*
	(user::logit "~
NLM: ~a: UNLOCK~a~a ~a~%"
	       (sunrpc:peer-dotted peer)
	       (if-nlm-v4 vers "4" "")
	       (if async "_MSG" "")
	       lock))
    
    (if* (not (user::fh-p fh))
       then (if-nlm-v4 vers 
		       (setf status #.*nlm4-stale-fh*))
     elseif (not (nlm-access-ok lock addr))
       then (if *nlm-debug*
		(user::logit "NLM: ==> Access denied by configuration.~%"))
	    (setf status #.*nlm-denied*)
       else (mp:with-process-lock (*nlm-state-lock*)
	      (let ((entry (nlm-find-lock lock *nlm-locks*)))
		(if* entry
		   then (handler-case (nlm-do-unlock entry)
			  (error (c)
			    (user::logit "~
NLM: Unexpected error during UNLOCK call: ~a~%" c)
			    (setf status
			      (if-nlm-v4 vers 
					 #.*nlm4-failed*
					 #.*nlm-denied-nolocks*)))
			  (:no-error (&rest args)
			    (declare (ignore args))
			    (setf status #.*nlm-granted*)
			    (setf *nlm-locks* (delete entry *nlm-locks*))))
			(nlm-remove-monitoring addr)))))
  
    (if *nlm-debug*
	(nlm-log-status status))
    
    (make-nlm-res
     :cookie (nlm-unlockargs-cookie arg)
     :stat (make-nlm-stat :stat status))))

;; CANCEL

(defun nlm4-cancel (arg vers peer cbody)
  (nlm-cancel arg vers peer cbody))

(defun nlm-cancel (arg vers peer cbody &key async)
  (declare (ignore cbody))
  (let* ((addr (sunrpc:rpc-peer-addr peer))
	 (lock (nlm-internalize-lock (nlm-cancargs-alock arg) nil vers
				    addr (nlm-cancargs-cookie arg)))
	 (status #.*nlm-granted*)) ;; always report success
    
    (if *nlm-debug*
	(user::logit "~
NLM: ~a: CANCEL~a~A (~a, block: ~a, excl: ~a)~%"
	       (sunrpc:peer-dotted peer)
	       (if-nlm-v4 vers "4" "")
	       (if async "_MSG" "")
	       lock
	       (nlm-cancargs-block arg)
	       (nlm-cancargs-exclusive arg)))

    (if* (nlm-access-ok lock addr)
       then (nlm-cancel-pending-retry lock)
       else (if *nlm-debug*
		(user::logit "==> Access denied by configuration.~%"))
	    (setf status #.*nlm-denied*))
    
    (if *nlm-debug*
	(nlm-log-status status))
    
    (make-nlm-res
     :cookie (nlm-cancargs-cookie arg)
     :stat (make-nlm-stat :stat status))))

;; TEST

(defun nlm4-test (arg vers peer cbody)
  (nlm-test arg vers peer cbody))

(defun nlm-test (arg vers peer cbody &key async)
  (declare (ignore cbody))
  (let* ((exclusive (nlm-testargs-exclusive arg))
	 (addr (sunrpc:rpc-peer-addr peer))
	 (lock (nlm-internalize-lock (nlm-testargs-alock arg) exclusive vers
				     addr (nlm-testargs-cookie arg)))
	 (fh (nlm-lock-internal-fh lock))
	 (offset (nlm-lock-internal-offset lock))
	 (len (nlm-lock-internal-len lock))
	 (status (if-nlm-v4 vers #.*nlm4-failed* #.*nlm-denied-nolocks*))
	 holder)
  
    (if *nlm-debug*
	(user::logit "NLM: ~a: TEST~a~a ~a, Exclusive: ~a~%"
	       (sunrpc:peer-dotted peer)
	       (if-nlm-v4 vers "4" "")
	       (if async "_MSG" "")
	       lock
	       exclusive))

    (if* (not (user::fh-p fh))
       then (if-nlm-v4 vers 
		       (setf status #.*nlm4-stale-fh*))
     elseif (not (nlm-access-ok lock addr))
       then (if *nlm-debug*
		(user::logit "NLM: ==> Access denied by configuration.~%"))
	    (setf status #.*nlm-denied*)
       else (mp:with-process-lock (*nlm-state-lock*)
	      (setf status (nlm-do-test-lock lock))
	      (when (= status #.*nlm-denied*)
		(setf holder (nlm-find-overlapping-lock fh offset len))
		(setf holder
		  (if* holder 
		     then (make-nlm-holder 
			   :exclusive (nlm-lock-internal-exclusive holder)
			   :svid (nlm-lock-internal-svid holder)
			   :oh (nlm-lock-internal-oh holder)
			   :l-offset (nlm-lock-internal-offset holder)
			   :l-len (nlm-lock-internal-len holder))
		     else ;; Make something up.  The lock must have been
			  ;; established by an external process (or the
			  ;; supplied file handle is stale)
			  (make-nlm-holder
			   :exclusive t
			   :svid 1
			   :oh (load-time-value (make-array 0 :element-type '(unsigned-byte 8)))
			   :l-offset 0
			   :l-len 0))))))
    
    (if *nlm-debug*
	(nlm-log-status status))
    
    (make-nlm-testres :cookie (nlm-testargs-cookie arg)
		      :stat (make-nlm-testrply 
			     :stat status
			     :holder holder))))

;; FREE ALL
(defun nlm4-free-all (arg vers peer cbody)
  (nlm-free-all arg vers peer cbody))

(defun nlm-free-all (arg vers peer cbody)
  (declare (ignore cbody))
  (let ((name (nlm-notify-name arg))
	(addr (sunrpc:rpc-peer-addr peer)))
    (if *nlm-debug*
	(user::logit "NLM~a: ~a: FREE ALL (~a)~%"
	       (if-nlm-v4 vers "4" "")
	       (sunrpc:peer-dotted peer)
	       name))
    
    (nlm-cleanup-common addr)))

;; NM (non-monitored) lock
(defun nlm4-nm-lock (arg vers peer cbody)
  (nlm-nm-lock arg vers peer cbody))

(defun nlm-nm-lock (arg vers peer cbody)
  (nlm-lock arg vers peer cbody :monitor nil))

;; Make asynchronous versions of the 4 main functions as well.
;; These are the -msg functions.  They call the normal (non -msg) functions
;; with :async t)
;; After they do their work they use call-nlm[4]-FUNC-res-[14] to send
;; the result.

(defmacro defun-nlm-async (name)
  (let ((funcname 
	 (intern (concatenate 'string "nlm-" (symbol-name name) "-msg")))
	(funcname4
	 (intern (concatenate 'string "nlm4-" (symbol-name name) "-msg")))
	(realfunc 
	 (intern (concatenate 'string "nlm-" (symbol-name name))))
	(callfunc (intern (concatenate 'string "call-nlm-" (symbol-name name)
				       "-res-1")))
	(callfunc4 (intern (concatenate 'string "call-nlm4-" (symbol-name name)
					"-res-4"))))
    
    `(eval-when (compile load eval)
       (defun ,funcname (arg vers peer cbody)
	 (ignore-errors
	  (sunrpc:with-rpc-client (cli (sunrpc:rpc-peer-addr peer) 
				       #.*nlm-prog* vers :udp)
	    (,callfunc cli (,realfunc arg vers peer cbody :async t)
		       :no-reply t))))

       (defun ,funcname4 (arg vers peer cbody)
	 (ignore-errors
	  (sunrpc:with-rpc-client (cli (sunrpc:rpc-peer-addr peer) 
				       #.*nlm-prog* vers :udp)
	    (,callfunc4 cli (,realfunc arg vers peer cbody :async t)
			:no-reply t)))))))
       

(defun-nlm-async test)
(defun-nlm-async lock)
(defun-nlm-async unlock)
(defun-nlm-async cancel)

;;; little daemons
 
(defun nlm-lock-retry-loop ()
  (loop
    (mp:with-process-lock (*nlm-state-lock*)
      (let (new-granted)
	(dolist (entry *nlm-retry-list*)
	  (when (= #.*nlm-granted* (nlm-try-lock entry))
	    (if *nlm-debug*
		(user::logit "NLM: Deferred lock ~a granted.~%" entry))
	    (push entry new-granted)))
	
	(if new-granted
	    (dolist (entry new-granted)
	      (nlm-add-monitoring (nlm-lock-internal-peer-addr entry))
	      (nlm-send-granted-msg entry)
	      (setf *nlm-retry-list* (delete entry *nlm-retry-list*))))
	
	(setf *nlm-notify-list* (nconc *nlm-notify-list* new-granted))))
      
    (sleep *nlm-retry-interval*)))

(defun nlm-externalize-lock (lock)
  (make-nlm-lock 
   :caller-name (nlm-lock-internal-caller-name lock)
   :fh (user::fhandle-to-vec (nlm-lock-internal-fh lock)
			     (if-nlm-v4 (nlm-lock-internal-vers lock) 3 2))
   :oh (nlm-lock-internal-oh lock)
   :svid (nlm-lock-internal-svid lock)
   :l-offset (nlm-lock-internal-offset lock)
   :l-len (nlm-lock-internal-len lock)))

(defun nlm-send-granted-msg (entry)
  (let ((addr (nlm-lock-internal-peer-addr entry))
	(vers (nlm-lock-internal-vers entry)))
    (if *nlm-debug*
	(user::logit "NLM: Sending GRANTED~a_MSG to ~a~%" 
		     (if-nlm-v4 vers "4" "")
		     (socket:ipaddr-to-dotted addr)))
    
    (#+ignore ignore-errors progn
     (sunrpc:with-rpc-client (cli addr #.*nlm-prog* vers :udp)
       (let ((args (make-nlm-testargs 
		    :cookie (nlm-lock-internal-cookie entry)
		    :exclusive (nlm-lock-internal-exclusive entry)
		    :alock (nlm-externalize-lock entry))))
	 (funcall (if-nlm-v4 vers 
			     #'call-nlm4-granted-msg-4
			     #'call-nlm-granted-msg-1)
		  cli args :no-reply t))))))

(defun nlm-grant-notify-loop ()
  (loop
    (mp:with-process-lock (*nlm-state-lock*)
      (dolist (entry *nlm-notify-list*)
	(nlm-send-granted-msg entry)))
    
    (sleep *nlm-grant-notify-interval*)))

(defun nlm4-granted-res (arg vers peer cbody)
  (nlm-granted-res arg vers peer cbody))

;; This is the callback the client uses to ack the granted msg.
(defun nlm-granted-res (arg vers peer cbody)
  (declare (ignore cbody))
  (let ((status (nlm-stat-stat (nlm-res-stat arg)))
	(cookie (opaque-data (nlm-res-cookie arg))))
    (if *nlm-debug*
	(user::logit "NLM: ~a GRANTED~a_RES (Stat: ~a)~%" 
	       (socket:ipaddr-to-dotted (sunrpc:rpc-peer-addr peer))
	       (if-nlm-v4 vers "4" "")
	       (nlm-status-to-string status)))
    
    (mp:with-process-lock (*nlm-state-lock*)
      (let ((lock (nlm-find-lock-by-cookie cookie *nlm-notify-list*)))
	(if* (null lock)
	   then (if *nlm-debug* 
		    (if* (nlm-find-lock-by-cookie cookie *nlm-locks*)
		       then (user::logit "NLM: ==> Duplicate (already received ack).~%")
		       else (user::logit "NLM: ==> No matching lock found.~%")))
	   else (setf *nlm-notify-list* (delete lock *nlm-notify-list*))
		(if* (= status #.*nlm-granted*)
		   then (if *nlm-debug*
			    (user::logit "NLM: ==> ~a fully obtained.~%" lock))
			(push lock *nlm-locks*)
		   else (if *nlm-debug*
			    (user::logit "NLM: ==> Client rejecting lock ~a~%" lock))
			;; unlock it.
			(handler-case (nlm-do-unlock lock)
			  (error (c)
			    (user::logit "~
NLM: Unexpected error while unlocking ~a: ~a~%" lock c)))))))))

;; Used by FREE ALL and by the nsm-callback.
(defun nlm-cleanup-common (addr)
  (mp:with-process-lock (*nlm-state-lock*)
    (let ((entries (nlm-find-locks-by-addr addr *nlm-locks*)))
      (dolist (entry entries)
	(if *nlm-debug*
	    (user::logit "NLM: Unlocking ~a~%" entry))
	(nlm-do-unlock entry)
	(setf *nlm-locks* (delete entry *nlm-locks*))))
    
    (let ((entries (nlm-find-locks-by-addr addr *nlm-notify-list*)))
      (dolist (entry entries)
	(if *nlm-debug*
	    (user::logit "NLM: Removing ~a from notify list.~%" entry))
	(setf *nlm-notify-list* (delete entry *nlm-notify-list*))))
    
    (let ((entries (nlm-find-locks-by-addr addr *nlm-retry-list*)))
      (dolist (entry entries)
	(if *nlm-debug*
	    (user::logit "NLM: Removing ~a from retry list.~%" entry))
	(setf *nlm-retry-list* (delete entry *nlm-retry-list*))))
    
    (nlm-remove-monitoring addr)))


;; A client restarted.  Release their locks.
;; arg is an nsm-callback-status
(defun nlm-nsm-callback (arg vers peer cbody)
  (declare (ignore cbody vers))
  (let ((host (nsm-callback-status-mon-name arg))
	(state (nsm-callback-status-state arg))
	(addr (sunrpc:rpc-peer-addr peer)))
    
    (if *nlm-debug*
	(user::logit "NLM: NSM reported new state for ~a: ~a~%" 
	       host state))
    
    (when (= addr #.(socket:dotted-to-ipaddr "127.0.0.1"))
      (nlm-cleanup-common (socket:dotted-to-ipaddr host)))))

(defvar *nlm-monitored-hosts* nil)

(defun nlm-addr-in-monitored-list (addr)
  (member addr *nlm-monitored-hosts* :test #'socket:ipaddr-equalp))

(defmacro with-nsm ((cli) &body body)
  `(sunrpc:with-rpc-client (,cli "127.0.0.1" *sm-prog* 
				 *sm-vers* :udp)
     ,@body))

(defun nlm-add-monitoring (addr)
  (block nil
    (let ((dotted (socket:ipaddr-to-dotted addr))
	  (priv #.(make-array 16 :element-type '(unsigned-byte 8)
			      :initial-element 0)))
      
      (mp:with-process-lock (*nlm-state-lock*)
	(if* (nlm-addr-in-monitored-list addr)
	   then (if *nlm-debug*
		    (user::logit "~
NLM: ~a already set up for monitoring. (OK)~%"
				 dotted))
		(return))
	(if *nlm-debug*
	    (user::logit "NLM: Calling NSM to add monitoring for ~a~%"
			 dotted))
	
	(with-nsm (cli)
	  (handler-case
	      (call-sm-mon-1 cli 
				 (make-mon :mon-id 
					   (make-mon-id 
					    :mon-name dotted
					    :my-id (make-my-id 
						    :my-name "127.0.0.1"
						    :my-prog #.*nlm-prog*
						    :my-vers 1 
						    :my-proc 99))
					   :priv priv))
	    (error (c)
	      (user::logit "~
NLM: Unexpected error while calling NSM MON: ~a~%" c))
	    (:no-error (res)
	      (let ((status (sm-stat-res-res-stat res))) ;; Cripes!
		(if* (= status #.*stat-succ*)
		   then (push addr *nlm-monitored-hosts*)
		   else (if *nlm-debug*
			    (user::logit "NLM: ==> Failed~%")))))))))))


(defun nlm-remove-monitoring (addr)
  (block nil
    (let ((dotted (socket:ipaddr-to-dotted addr)))
      (mp:with-process-lock (*nlm-state-lock*)
	(if* (not (nlm-addr-in-monitored-list addr))
	   then (if *nlm-debug*
		    (user::logit "~
NLM: nlm-remove-monitoring: ~a not on monitoring list. (OK)~%"
				 dotted))
		(return))
	
	(when (and 
	       (null (nlm-find-locks-by-addr addr *nlm-locks*))
	       (null (nlm-find-locks-by-addr addr *nlm-retry-list*))
	       (null (nlm-find-locks-by-addr addr *nlm-notify-list*)))
	  (if *nlm-debug*
	      (user::logit "NLM: Calling NSM to remove monitoring for ~a~%" 
			   dotted))

	  (with-nsm (cli)
	    (handler-case
		(call-sm-unmon-1 cli 
				     (make-mon-id 
				      :mon-name dotted
				      :my-id (make-my-id 
					      :my-name "127.0.0.1"
					      :my-prog #.*nlm-prog*
					      :my-vers 1 
					      :my-proc 99)))
	      (error (c)
		(user::logit "~
NLM: Unexpected error while calling NSM UNMON: ~a~%" c)))

	    ;; Remove from list even if the call didn't go through.
	    (setf *nlm-monitored-hosts* 
	      (delete addr *nlm-monitored-hosts*
		      :test #'socket:ipaddr-equalp))))))))

(eval-when (compile load eval)
  (export '(*nlm-gate* *nlm-debug* NLM)))
