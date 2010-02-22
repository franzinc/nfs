;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2010 Franz Inc, Oakland, CA.  All rights reserved.
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

;; This file implements the Network Status Monitor (NSM) protocol. 

;; Ref: http://www.opengroup.org/onlinepubs/009629799/chap11.htm

(in-package :nsm)

(defparameter *nsm-port* nil)

(sunrpc:def-rpc-program (NSM 100024 :port *nsm-port*)
  (
   (1 ;; version
     (0 sm-null void void)
     (1 sm-stat sm-name sm-stat-res)
     (2 sm-mon mon sm-stat-res)
     (3 sm-unmon mon-id sm-stat)
     (4 sm-unmon-all my-id sm-stat)
     (5 sm-simu-crash void void)
     (6 sm-notify stat-chge void)
   )
  ))

(defvar *nsm-state* 0) ;; down

(defstruct nsm-monitor
  host ;; string
  requestor ;; string 
  prog
  vers
  proc
  priv ;; usb8
  state)

(defun nsm-monitor-to-string (obj)
  (format nil "[Monitor ~a (Callback: ~a, Prg: ~a, V: ~a, Proc: ~a)]"
	  (nsm-monitor-host obj)
	  (nsm-monitor-requestor obj)
	  (nsm-monitor-prog obj)
	  (nsm-monitor-vers obj)
	  (nsm-monitor-proc obj)))

(defvar *nsm-monitored-hosts* nil)
(defvar *nsm-callbacks-list* nil)
(defvar *nsm-our-name* nil)

(defparameter *nsm-state-file* "sys:nsm-state")
(defparameter *nsm-debug* nil)
(defparameter *nsm-callback-retry-interval* 10) ;; seconds
(defparameter *nsm-notify-retry-interval* 10) ;; seconds
(defparameter *nsm-gate* (mp:make-gate nil))
(defvar *nsm-state-lock* (mp:make-process-lock))

;;;;;;;;;

(defun NSM-init ()
  (mp:process-run-function "nsm callback retry loop" 
    #'sm-callback-retry-loop)
  (nsm-load-state) 
  (nsm-advance-state)
  (nsm-notify-peers) ;; Let folks know that we're back.
  (mp:open-gate *nsm-gate*))

(defun nsm-save-state ()
  (mp:with-process-lock (*nsm-state-lock*)
    (let ((tmpfile (concatenate 'string *nsm-state-file* ".tmp")))
      (with-open-file (f tmpfile :direction :output
		       :if-exists :supersede)
	(write (list *nsm-state* *nsm-monitored-hosts*) :stream f)
	(excl.osi:fsync f))
      (macrolet ((trans (thing)
		   `(namestring (translate-logical-pathname ,thing))))
	(user::my-rename (trans tmpfile) (trans *nsm-state-file*))))))

(defun nsm-load-state ()
  (mp:with-process-lock (*nsm-state-lock*)
    (multiple-value-setq (*nsm-state* *nsm-monitored-hosts*)
      (if* (probe-file *nsm-state-file*)
	 then (with-open-file (f *nsm-state-file*)
		(let ((res (read f)))
		  (values (first res) (second res))))
	 else (values -1 nil)))
    (dolist (mon *nsm-monitored-hosts*)
      (let ((priv (nsm-monitor-priv mon)))
	(if priv
	    (setf (nsm-monitor-priv mon)
	      (coerce priv '(simple-array (unsigned-byte 8) (*)))))))))
      

(defun nsm-advance-state ()
  (mp:with-process-lock (*nsm-state-lock*)
    (incf *nsm-state*)
    (until (and (oddp *nsm-state*) (> *nsm-state* 0))
      (incf *nsm-state*))
    ;; Make sure it remains a signed-positive.
    (if (> *nsm-state* #.(1- (expt 2 31)))
	(setf *nsm-state* 1))
    (if *nsm-debug* (user::logit-stamp "NSM: New state: ~d~%" *nsm-state*))
    (nsm-save-state)))

(defun nsm-log-status (status)
  (user::logit "==> ~a~%"
	 (case status
	   (#.*stat-fail* "FAIL")
	   (#.*stat-succ* "SUCC")
	   (t (format nil "~a" status)))))

(defun nsm-convert-mon-id (mon-id &key priv)
  (let* ((priv (if priv (opaque-data priv)))
	 (name (mon-id-mon-name mon-id)) ;; host to monitor
	 (my-id (mon-id-my-id mon-id))
	 (callback-host (my-id-my-name my-id))
	 (callback-prog (my-id-my-prog my-id))
	 (callback-vers (my-id-my-vers my-id))
	 (callback-proc (my-id-my-proc my-id)))
    (make-nsm-monitor :host name
		      :requestor callback-host
		      :prog callback-prog
		      :vers callback-vers
		      :proc callback-proc
		      :priv priv)))

;; Call with lock held.
(defun nsm-find-entry (mon-id)
  (let ((host (nsm-monitor-host mon-id))
	(requestor (nsm-monitor-requestor mon-id))
	(prog (nsm-monitor-prog mon-id))
	(vers (nsm-monitor-vers mon-id))
	(proc (nsm-monitor-proc mon-id)))
    (dolist (entry *nsm-monitored-hosts*)
      (if (and (string= (nsm-monitor-host entry) host)
	       (string= (nsm-monitor-requestor entry) requestor)
	       (= (nsm-monitor-prog entry) prog)
	       (= (nsm-monitor-vers entry) vers)
	       (= (nsm-monitor-proc entry) proc))
	  (return entry)))))

;; Returns a list of matches
(defun nsm-find-entry-by-my-id (requestor prog vers proc)
  (let (res)
    (dolist (entry *nsm-monitored-hosts*)
      (if (and (string= (nsm-monitor-requestor entry) requestor)
	       (= (nsm-monitor-prog entry) prog)
	       (= (nsm-monitor-vers entry) vers)
	       (= (nsm-monitor-proc entry) proc))
	  (push entry res)))
    res))

;;;;;;;;;;; Procedures

(defun sm-null (arg vers peer cbody)
  (declare (ignore arg vers cbody))
  (if *nsm-debug*
      (user::logit-stamp "NSM: ~a: NULL~%" (sunrpc:peer-dotted peer))))

;; in: sm-name, out: sm-stat-res
(defun sm-stat (arg vers peer cbody)
  (declare (ignore vers cbody))
  (let ((name (sm-name-mon-name arg))
	(status #.*stat-fail*))
    (if *nsm-debug*
	(user::logit-stamp "NSM: ~a: STAT (~a) " (sunrpc:peer-dotted peer) name))
    
    ;; This is what linux does.
    (if (ignore-errors (socket:lookup-hostname name))
	(setf status #.*stat-succ*))

    (if *nsm-debug*
	(nsm-log-status status))

    (mp:with-process-lock (*nsm-state-lock*)
      (make-sm-stat-res :res-stat status
			:state *nsm-state*))))

;; in: mon, out: sm-stat-res
(defun sm-mon  (arg vers peer cbody)
  (declare (ignore vers cbody))
  (let* ((mon-id (mon-mon-id arg))
	 (struct (nsm-convert-mon-id mon-id :priv (mon-priv arg)))
	 (status #.*stat-fail*))
    
    (if *nsm-debug*
	(user::logit-stamp "NSM: ~a: MON ~a~%" (sunrpc:peer-dotted peer)
	       (nsm-monitor-to-string struct)))

    (mp:with-process-lock (*nsm-state-lock*)
      (if* (sunrpc:local-peer-p peer)
	 then (if* (member struct *nsm-monitored-hosts* :test #'equalp)
		 then (if *nsm-debug*
			  (user::logit-stamp "NSM: ==> Already monitored.~%"))
		 else (push struct *nsm-monitored-hosts*)
		      (nsm-save-state)
		      (if *nsm-debug*
			  (user::logit-stamp "NSM: ==> Adding entry to monitor list.~%")))
	      (setf status #.*stat-succ*)
	 else (user::logit-stamp "NSM: ==> Rejecting non-local request.~%"))
      
      (when *nsm-debug*
	(user::logit-stamp "NSM: ")
	(nsm-log-status status))
      
      (make-sm-stat-res :res-stat status
			:state *nsm-state*))))
    
;; in: mon-id, out: sm-stat
(defun sm-unmon  (arg vers peer cbody)
  (declare (ignore vers cbody))
  (let ((struct (nsm-convert-mon-id arg)))
    
    (if *nsm-debug*
	(user::logit-stamp "NSM: ~a: UNMON ~a~%" (sunrpc:peer-dotted peer)
	       (nsm-monitor-to-string struct)))
    
    (mp:with-process-lock (*nsm-state-lock*)
      (if* (sunrpc:local-peer-p peer)
	 then (let ((entry (nsm-find-entry struct)))
		(if* entry
		   then (if *nsm-debug*
			    (user::logit-stamp "NSM: ==> Removing entry from monitor list.~%"))
			(setf *nsm-monitored-hosts* 
			  (delete entry *nsm-monitored-hosts*))
			(nsm-save-state)
		   else (if *nsm-debug*
			    (user::logit-stamp "NSM: ==> No matching entry (probably a dupe)~%"))))
	 else (if *nsm-debug*
		  (user::logit-stamp "NSM: ==> Ignoring non-local request~%")))
      
      (make-sm-stat :state *nsm-state*))))


;; in: my-id, out: sm-stat
(defun sm-unmon-all  (arg vers peer cbody)
  (declare (ignore vers cbody))
  (let ((requestor (my-id-my-name arg))
	(prog (my-id-my-prog arg))
	(vers (my-id-my-vers arg))
	(proc (my-id-my-proc arg)))
    
    (if *nsm-debug*
	(user::logit-stamp "~
NSM: ~a: UNMON_ALL Requestor: ~a, Prog: ~a, V: ~a, Proc: ~a~%"
		     (sunrpc:peer-dotted peer)
		     requestor prog vers proc))
    
    (mp:with-process-lock (*nsm-state-lock*)
      (when (sunrpc:local-peer-p peer)
	(let ((entries (nsm-find-entry-by-my-id requestor prog vers proc)))
	  (when entries
	    (dolist (entry entries)
	      (if *nsm-debug*
		  (user::logit-stamp "NSM: Removing ~a from monitor list.~%" 
			 (nsm-monitor-to-string entry)))
	      (setf *nsm-monitored-hosts* 
		(delete entry *nsm-monitored-hosts*)))
	    (nsm-save-state))))
      
      (make-sm-stat :state *nsm-state*))))
	

;; in: void, out: void
(defun sm-simu-crash  (arg vers peer cbody)
  (declare (ignore arg vers cbody))
  (if *nsm-debug*
      (user::logit-stamp "NSM: ~a: SIMU_CRASH~%" (sunrpc:peer-dotted peer)))
  
  (mp:with-process-lock (*nsm-state-lock*)
    (when (sunrpc:local-peer-p peer)
      (nsm-advance-state) ;; auto-saves
      (nsm-notify-peers))))

;; Clients call this to notify us that they rebooted.  

;; in: stat-chge, out: void
(defun sm-notify  (arg vers peer cbody)
  (declare (ignore vers cbody))
  (let* ((host (stat-chge-mon-name arg))
	 (newstate (stat-chge-state arg))
	 (dotted (sunrpc:peer-dotted peer)))
	
    (if *nsm-debug*
	(user::logit-stamp "NSM: ~a: NOTIFY (~a, ~a)~%" 
	       dotted host newstate))
    
    ;; Notify interested parties (if the status actually changed)
    (mp:with-process-lock (*nsm-state-lock*)
      (dolist (entry *nsm-monitored-hosts*)
	(when (and (string= (nsm-monitor-host entry) dotted)
		   (not (equal (nsm-monitor-state entry) newstate)))
	  (if *nsm-debug*
	      (user::logit-stamp "NSM: ==> Adding ~a to callback list.~%" 
		     (nsm-monitor-to-string entry)))
	  (setf (nsm-monitor-state entry) newstate)
	  (push entry *nsm-callbacks-list*))))))

;; Returns t if we got a reply of some sort.
;; called by sm-callback-retry-loop
(defun sm-do-callback (entry)
  (handler-case 
      (sunrpc:with-rpc-client (cli (nsm-monitor-requestor entry)
				   (nsm-monitor-prog entry)
				   (nsm-monitor-vers entry)
				   :udp)
	(sunrpc:callrpc cli (nsm-monitor-proc entry) 
			#'xdr-nsm-callback-status ;; inproc      
			(make-nsm-callback-status 
			 :mon-name (nsm-monitor-host entry)
			 :state (nsm-monitor-state entry)
			 :priv (nsm-monitor-priv entry))))
    (error (c)
      (if *nsm-debug*
	  (user::logit-stamp "NSM: Error while sending callback ~a: ~a~%"
		       (nsm-monitor-to-string entry) c))
      nil)
    (:no-error (results)
      (declare (ignore results))
      t)))
	       

(defun sm-callback-retry-loop ()
  (loop
    ;; Make a copy of the list before traversing it.  That way we can
    ;; release the lock while we process the callbacks.  If we don't,
    ;; we could get into a deadlock if the callback procedures make
    ;; additional NSM calls.
    
    (let (entries completed)
      (mp:with-process-lock (*nsm-state-lock*)
	(setf entries (copy-list *nsm-callbacks-list*)))
      
      (dolist (entry entries)
	(when (sm-do-callback entry)
	  (if *nsm-debug*
	      (user::logit-stamp "NSM: Callback ~a completed.~%" 
		     (nsm-monitor-to-string entry)))
	  (push entry completed)))
      
      (mp:with-process-lock (*nsm-state-lock*)
	(dolist (entry completed)
	  (setf *nsm-callbacks-list* (delete entry *nsm-callbacks-list*)))))
    
    (sleep *nsm-callback-retry-interval*)))

(defun nsm-notify-peers ()
  (mp:with-process-lock (*nsm-state-lock*)
    (when *nsm-monitored-hosts*
      (when (null *nsm-our-name*)
	(setf *nsm-our-name* (excl.osi:gethostname)))
  
      (let ((entries *nsm-monitored-hosts*))
	(setf *nsm-monitored-hosts* nil)
	(nsm-save-state)
    
	(dolist (entry entries)
	  (if *nsm-debug*
	      (user::logit-stamp "~
NSM: Notifying ~a of our new state.~%" 
		     (nsm-monitor-host entry)))
	
	  (mp:process-run-function 
	      (format nil "~
NSM notifying ~a of new state" (nsm-monitor-host entry))
	    #'nsm-notify-peer (nsm-monitor-host entry) *nsm-state*))))))

(defun nsm-notify-peer (host state)
  ;; XXX -- When should we give up?
  (loop
    (handler-case 
	(sunrpc:with-rpc-client (cli host #.*sm-prog* #.*sm-vers* :udp)
	  (call-sm-notify-1 cli (make-stat-chge :mon-name *nsm-our-name*
						:state state)))
      (error (c)
	(if *nsm-debug*
	    (user::logit-stamp "NSM: Error while sending notify to ~a: ~a~%"
		   host c)))
      (:no-error (results)
	(declare (ignore results))
	(if *nsm-debug*
	    (user::logit-stamp "NSM: Successfully notified ~a of our new state.~%" host))
	(return-from nsm-notify-peer)))
    
    (sleep *nsm-notify-retry-interval*)))

(eval-when (compile load eval)
  (export '(*nsm-gate* *nsm-debug* *nsm-port* NSM)))
