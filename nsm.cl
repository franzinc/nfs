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
;; $Id: nsm.cl,v 1.2 2005/07/28 16:41:41 dancy Exp $

(in-package :user)

;; This file implements the Network Status Monitor (NSM) protocol. 

;; Ref: http://www.opengroup.org/onlinepubs/009629799/chap11.htm

(eval-when (compile load eval)
  (defconstant *smprog* 100024)
  (defconstant *smvers* 1)

  (defconstant *sm-maxstrlen* 1024)

  (defconstant *sm-null* 0)
  (defconstant *sm-stat* 1)
  (defconstant *sm-mon* 2)
  (defconstant *sm-unmon* 3)
  (defconstant *sm-unmon-all* 4)
  (defconstant *sm-simu-crash* 5)
  (defconstant *sm-notify* 6)
  
  (defconstant *sm-stat-succ* 0)
  (defconstant *sm-stat-fail* 1))

(defvar *nsm-tcp-socket* nil)
(defvar *nsm-udp-socket* nil)

(defvar *nsm-state* 0) ;; down

(defstruct nsm-monitor
  host ;; dotted string
  requestor ;; string 
  prog
  vers
  proc
  priv)

(defparameter *nsm-monitored-hosts* nil)
(defparameter *nsm-our-name* nil)

(defparameter *nsm-state-file* "sys:nsm-state")
(defparameter *nsm-debug* t)


(defun make-nsm-sockets ()
  (handler-case 
      (progn
	(unless *nsm-tcp-socket*
	  (setf *nsm-tcp-socket*
	    (socket:make-socket :type :hiper
				:connect :passive)))
	(unless *nsm-udp-socket*
	  (setf *nsm-udp-socket*
	    (socket:make-socket :type :datagram))))
    (error (c)
      (bailout "
Unexpected error while creating a nsm socket: ~a~%" c)))
  (logit "NSM: Using UDP port ~d~%" (socket:local-port *nsm-udp-socket*))
  (logit "NSM: Using TCP port ~d~%" (socket:local-port *nsm-tcp-socket*)))

(defun close-nsm-sockets ()
  (when *nsm-tcp-socket*
    (close *nsm-tcp-socket*)
    (setf *nsm-tcp-socket* nil))
  (when *nsm-udp-socket*
    (close *nsm-udp-socket*)
    (setf *nsm-udp-socket* nil)))

(defmacro with-nsm-sockets (() &body body)
  `(unwind-protect
       (progn
	 (make-nsm-sockets)
	 ,@body)
     (close-nsm-sockets)))

(defun nsm ()
  (with-nsm-sockets ()
    (with-portmapper-mapping (*smprog* 
			      *smvers*
			      (socket:local-port *nsm-tcp-socket*) 
			      IPPROTO_TCP)
      (with-portmapper-mapping (*smprog* 
				*smvers*
				(socket:local-port *nsm-udp-socket*) 
				IPPROTO_UDP)
	(let* ((buffer (make-array #.(* 64 1024) 
				   :element-type '(unsigned-byte 8)))
	       (server (make-rpc-server :tcpsock *nsm-tcp-socket*
					:udpsock *nsm-udp-socket*
					:buffer buffer)))
	  (declare (dynamic-extent buffer server))
	  (nsm-load-state) 
	  (advance-state)
	  (nsm-save-state)
	  (nsm-notify-peers) ;; Let folks know that we're back.
	  (loop
	    (multiple-value-bind (xdr peer)
		(rpc-get-message server)
	      (nsm-message-handler xdr peer))))))))


(defun nsm-message-handler (xdr peer)
  (block nil
    (let (msg cbody)
      (setf msg (create-rpc-msg xdr))
      (setf cbody (rpc-msg-cbody msg))
      
      ;; sanity checks first
      (when (null cbody)
	(logit "Invalid message from ~A~%" peer)
	(return))
      
      (when (/= (call-body-prog cbody) *smprog*)
	(logit "NSM: Sending program unavailable response for prog=~D to ~A~%"
	       (call-body-prog cbody)
	       (socket:ipaddr-to-dotted (rpc-peer-addr peer)))
	(rpc-send-prog-unavail peer (rpc-msg-xid msg) (null-verf))
	(return))

      (unless (= (call-body-vers cbody) *smvers*)
	(logit "NSM: Sending program version mismatch response (requested version was ~D) to ~A~%" 
	       (call-body-vers cbody)
	       (socket:ipaddr-to-dotted (rpc-peer-addr peer)))
	(rpc-send-prog-mismatch peer (rpc-msg-xid msg) (null-verf) 
				*smvers* *smvers*)
	(return))
      
      (case (call-body-proc cbody)
	(#.*sm-null*
	 (nsm-null peer (rpc-msg-xid msg)))
	(#.*sm-stat*
	 (nsm-stat peer (rpc-msg-xid msg) cbody))
	(#.*sm-mon*
	 (nsm-mon peer (rpc-msg-xid msg) cbody))
	(#.*sm-unmon*
	 (nsm-unmon peer (rpc-msg-xid msg) cbody))
	(#.*sm-unmon-all*
	 (nsm-unmon-all peer (rpc-msg-xid msg) cbody))
	(#.*sm-simu-crash*
	 (nsm-simu-crash peer (rpc-msg-xid msg)))
	(#.*sm-notify*
	 (nsm-notify peer (rpc-msg-xid msg) cbody))
	(t
	 ;; should send a negative response
	 (logit "NSM: unhandled procedure ~D requested by ~A~%"
		(call-body-proc cbody)
		(socket:ipaddr-to-dotted (rpc-peer-addr peer))))))))

(defun nsm-null (peer xid)
  (if *nsm-debug* (logit "NSM: ~a: SM_NULL~%" 
			 (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (null-verf) xdr)))

;; Args:
;; string mon_name
;; string myname
;; int prog, vers, proc
;; opaque priv[16];

;; return: res.. and state.
(defun nsm-mon (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((monitor-host (xdr-string params)) ;; dotted string
	  (requestor (xdr-string params))
	  (prog (xdr-int params))
	  (vers (xdr-int params))
	  (proc (xdr-int params))
	  (priv (xdr-opaque-fixed params :len 16 :make-vec t))
	  (peer-addr (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
      (when *nsm-debug* 
	(logit "NSM: ~A: SM_MON (M: ~a, R: ~a)~%"
	       peer-addr monitor-host requestor)
	(logit "NSM: ~A: + Callback to: Prog: ~a, Vers: ~a, Proc: ~a~%"
	       peer-addr prog vers proc))
      (with-successful-reply (res peer xid (null-verf))
	(if* (/= (socket:dotted-to-ipaddr "127.0.0.1") peer-addr)
	   then
		(logit "NSM: ~A: --> Rejecting request~%" peer-addr)
		(xdr-int res *sm-stat-fail*)
		(xdr-int res -1)
	   else
		(pushnew
		 (make-nsm-monitor :host monitor-host
				   :requestor "127.0.0.1"
				   :prog prog
				   :vers vers
				   :proc proc
				   :priv priv)
		 *nsm-monitored-hosts* :test #'equalp)
		(nsm-save-state)
		(xdr-int res *sm-stat-succ*)
		(xdr-int res *nsm-state*))))))


(defun nsm-notify (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((host (xdr-string params))
	  (state (xdr-int params))
	  (peer-addr (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
      (when *nsm-debug* 
	(logit "NSM: ~a: SM_NOTIFY (H: ~a, S: ~a)~%"
	       peer-addr host state))
      (with-successful-reply (res peer xid (null-verf))
	;; Find a matching entry.  We match by using the IP address
	;; of the peer, not the name provided.
	(let ((entry (find peer-addr *nsm-monitored-hosts* 
			   :key #'nsm-monitor-host :test #'string=)))
	  (when entry
	    ;; Invoke the callback procedure
	    (ignore-errors
	     (callrpc (nsm-monitor-requestor entry)
		      (nsm-monitor-prog entry)
		      (nsm-monitor-vers entry)
		      (nsm-monitor-proc entry)
		      :udp
		      #'(lambda (xdr dummy)
			  (declare (ignore dummy))
			  (xdr-string xdr host)
			  (xdr-int xdr state)
			  (xdr-opaque-fixed xdr 
					    :vec (nsm-monitor-priv entry)))
		      nil))))))))

(defun nsm-simu-crash (peer xid)
  (let ((peer-addr (socket:ipaddr-to-dotted (rpc-peer-addr peer)))
	(xdr (create-xdr :direction :build)))
    
    (if *nsm-debug* (logit "NSM: ~a: SM_SIMU_CRASH~%" peer-addr))
    (if* (/= (socket:dotted-to-ipaddr "127.0.0.1") peer-addr)
       then
	    (logit "NSM: ~a: -> Ignoring request.~%" peer-addr)
       else
	    (advance-state)
	    (nsm-notify-peers))
    (send-successful-reply peer xid (null-verf) xdr)))

(defun nsm-stat (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((monitor-host (xdr-string params)))
      (when *nsm-debug* 
	(logit "NSM: ~a requested nsm-stat ~a.~%"
	       (socket:ipaddr-to-dotted (rpc-peer-addr peer))  monitor-host)
	(with-successful-reply (res peer xid (null-verf))
	  ;; This is what Solaris does.
	  (xdr-int res *sm-stat-succ*)
	  (xdr-int res *nsm-state*))))))

(defun nsm-unmon (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((monitor-host (xdr-string params)) ;; dotted string
	  (requestor (xdr-string params))
	  (prog (xdr-int params))
	  (vers (xdr-int params))
	  (proc (xdr-int params))
	  (peer-addr (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
      (when *nsm-debug* 
	(logit "NSM: ~a: SM_UNMON (M: ~a, R: ~a, Prog: ~a, Vers: ~a, Proc: ~a)~%"
	       peer-addr monitor-host requestor prog vers proc))
      
      (with-successful-reply (res peer xid (null-verf))
	(if* (/= (socket:dotted-to-ipaddr "127.0.0.1") peer-addr)
	   then
		(logit "NSM: ~A: --> Ignore request~%" peer-addr)
	   else
		(setf *nsm-monitored-hosts* 
		  (delete-if 
		   #'(lambda (entry)
		       (and (string= (nsm-monitor-host entry) monitor-host)
			    (string= (nsm-monitor-requestor entry) requestor)
			    (= (nsm-monitor-prog entry) prog)
			    (= (nsm-monitor-vers entry) vers)
			    (= (nsm-monitor-proc entry) proc)))
		   *nsm-monitored-hosts*)))
	
	(xdr-int res *nsm-state*)))))

(defun nsm-unmon-all (peer xid cbody)
  (with-xdr-xdr ((call-body-params cbody) :name params)
    (let ((requestor (xdr-string params))
	  (prog (xdr-int params))
	  (vers (xdr-int params))
	  (proc (xdr-int params))
	  (peer-addr (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
      (when *nsm-debug* 
	(logit "NSM: ~a: SM_UNMON_ALL (R: ~a, Prog: ~a, Vers: ~a, Proc: ~a)~%"
	       peer-addr requestor prog vers proc))
      
      (with-successful-reply (res peer xid (null-verf))
	(if* (/= (socket:dotted-to-ipaddr "127.0.0.1") peer-addr)
	   then
		(logit "NSM: ~A: --> Ignore request~%" peer-addr)
	   else
		(setf *nsm-monitored-hosts* 
		  (delete-if 
		   #'(lambda (entry)
		       (and (string= (nsm-monitor-requestor entry) requestor)
			    (= (nsm-monitor-prog entry) prog)
			    (= (nsm-monitor-vers entry) vers)
			    (= (nsm-monitor-proc entry) proc)))
		   *nsm-monitored-hosts*)))
	
	(xdr-int res *nsm-state*)))))


;;;;;;;;;

(defun nsm-save-state ()
  (let ((tmpfile (concatenate 'string *nsm-state-file* ".tmp")))
    (with-open-file (f tmpfile :direction :output
		     :if-exists :supersede)
      (write (list *nsm-state* *nsm-monitored-hosts*) :stream f)
      (fsync f))
    ;; No way to do an atomic rename on Windows. *sigh*.
    (if (probe-file *nsm-state-file*)
	(delete-file *nsm-state-file*))
    (rename tmpfile *nsm-state-file*)))

(defun nsm-load-state ()
  (multiple-value-setq (*nsm-state* *nsm-monitored-hosts*)
    (if (probe-file *nsm-state-file*)
	(with-open-file (f *nsm-state-file*)
	  (let ((res (read f)))
	    (values (first res) (second res))))
      (values -1 nil))))

(defun advance-state ()
  (incf *nsm-state*)
  (until (and (oddp *nsm-state*) (> *nsm-state* 0))
    (incf *nsm-state*))
  ;; Make sure it remains a signed-positive.
  (if (> *nsm-state* #.(1- (expt 2 31)))
      (setf *nsm-state* 1)))

(defun nsm-notify-peers ()
  (if (null *nsm-our-name*)
      (setf *nsm-our-name* (gethostname)))
  
  ;; Do each in its own process so that an unresponsive peer
  ;; doesn't hold up the whole process.
  (dolist (entry *nsm-monitored-hosts*)
    (mp:process-run-function "NSM peer notify in progress" 
      #'nsm-notify-peer (nsm-monitor-host entry))))

(defun nsm-notify-peer (host)
  ;; Don't make any heroic efforts.
  (ignore-errors
   (callrpc host *smprog* *smvers* *sm-notify* :udp
	    ;; inproc
	    #'(lambda (xdr dummy)
		(declare (ignore dummy))
		(xdr-string xdr *nsm-our-name*)
		(xdr-int xdr *nsm-state*))
	    nil)))


