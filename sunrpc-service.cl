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

;; Service stuff

(in-package :sunrpc)

(eval-when (compile)
  (declaim (optimize (speed 3))))

(defmacro with-portmapper-mappings ((service prog versions udpport tcpport) 
				    &body body)
  (let ((s (gensym))
	(p (gensym))
	(vs (gensym))
	(uport (gensym))
	(tport (gensym))
	(v (gensym)))
    `(let ((,s ,service)
	   (,p ,prog)
	   (,vs ,versions)
	   (,uport ,udpport)
	   (,tport ,tcpport))
       (if (not (listp, vs))
	   (setf ,vs (list ,vs)))
       
       (when (/= ,p portmap:*pmap-prog*) 
	 ;; Cleanup first
	 (dolist (,v ,vs)
	   (portmap-unset ,p ,v))
	 
	 ;; Now register.
	 (dolist (,v ,vs)
	   (when ,uport
	     (if (null (portmap-set ,p ,v :udp ,uport))
		 (user::bailout "~a: Failed to register with portmapper.~%" ,s)))
	   (when ,tport
	     (if (null (portmap-set ,p ,v :tcp ,tport))
		 (user::bailout "~a: Failed to register with portmapper.~%" ,s)))))
       
       (unwind-protect (progn ,@body)
	 ;; Unregister
	 (when (/= ,p portmap:*pmap-prog*) 
	   (dolist (,v ,vs)
	     (ignore-errors (portmap-unset ,p ,v))))))))

;; valid means:
;; message type is *call*
;; rpcvers is 2
(defmacro with-valid-call ((msg peer cbody) &body body)
  (let ((m (gensym))
	(p (gensym)))
    `(let ((,m (rpc-msg-body ,msg))
	   (,p ,peer)
	   ,cbody)
       (when (eq (rpc-msg-body-u-mtype ,m) #.*call*)
	 (setf ,cbody (rpc-msg-body-u-cbody ,m))
	 (if* (not (eq (call-body-rpcvers ,cbody) 2))
	    then (send-rpc-mismatch-reply ,p (rpc-msg-xid ,m) 2 2)
	    else ,@body)))))

(defun make-rpc-socket (service &rest params)
  (handler-case (apply #'socket:make-socket params)
    (error (c)
      (user::bailout "
~a: Error while creating socket: ~a~%" service c))))

(defmacro with-rpc-sockets ((service usock tsock &key port) &body body)
  (let ((s (gensym))
	(p (gensym)))
    `(let* ((,s ,service)
	    (,p ,port)
	    (,usock (make-rpc-socket ,s :type :datagram :local-port ,p))
	    (,tsock (make-rpc-socket ,s :type :hiper :connect :passive
				     :local-port ,p)))
       (unwind-protect (progn ,@body)
	 (ignore-errors (close ,tsock))
	 (ignore-errors (close ,usock))))))


(defmacro def-rpc-program-1 ((program prognum versions usock tsock
			      &key port) 
			     &body body)
  `(with-rpc-sockets (,program ,usock ,tsock :port ,port)
     (user::logit-stamp "~a: Using UDP port ~d~%" 
			,program (socket:local-port ,usock))
     (user::logit-stamp "~a: Using TCP port ~d~%" 
			,program (socket:local-port ,tsock))
     (with-portmapper-mappings (,program ,prognum ',versions
					 (socket:local-port ,usock) 
					 (socket:local-port ,tsock))
       ,@body)))

(defun prepend-xdr (sym)
  (intern (concatenate 'string (symbol-name 'xdr) "-" (symbol-name sym))))

;; 'program' is a string.
(defmacro def-rpc-program-main (program prognum proc-versions usock tsock
				lowest-version highest-version)
  (let ((server (gensym))
	(msgxdr (gensym))
	(peer (gensym))
	(msg (gensym))
	(cbody (gensym))
	(vers (gensym))
	(res (gensym))
	(init-func (intern (concatenate 'string program "-init")))
	version-cases)

    (dolist (vdef proc-versions)
      (let (proc-cases)
	(dolist (procdef (cdr vdef))
	  (push
	   (let ((encoder (fourth procdef)))
	     (if (not (eq encoder :ignore))
		 (setf encoder (prepend-xdr encoder)))
	     `(,(first procdef)
	       (setf func (quote ,(second procdef)))
	       (setf args-decoder (quote ,(prepend-xdr (third procdef))))
	       (setf res-encoder (quote ,encoder))))
	   proc-cases))
	
	(push `(t 
		(user::logit-stamp "~
~a: ~a requested procedure ~d, version ~a, which is unavailable.~%" 
			     ,program
			     (peer-dotted ,peer)
			     (call-body-proc ,cbody) (call-body-vers ,cbody))
		       
		(send-proc-unavail-reply ,peer (rpc-msg-xid ,msg)
					 *nullverf*)
		(return))
	      proc-cases)
	
	(setf proc-cases (nreverse proc-cases))
	(push 
	 `(,(car vdef)
	   (case procnum
	     ,@proc-cases))
	 version-cases)))
    
    (setf version-cases (nreverse version-cases))
    
    `(let ((,server (make-rpc-server :tcpsock ,tsock
				     :udpsock ,usock)))
       (if (fboundp ',init-func)
	   (funcall ',init-func))
       
       (loop
	 (block nil
	   (let* ((,msgxdr (rpc-get-message ,server))
		  (,msg (xdr-rpc-msg ,msgxdr))
		  (,peer (rpc-server-peer ,server)))
	     (with-valid-call (,msg ,peer ,cbody)
	       ;; sanity checks first
	       (if* (/= (call-body-prog ,cbody) ,prognum)
		  then (user::logit-stamp "~
~a: Sending program unavailable response for prog=~D to ~A~%"
				    ,program
				    (call-body-prog ,cbody)
				    (peer-dotted ,peer))
		       (send-prog-unavail-reply ,peer (rpc-msg-xid ,msg) 
						*nullverf*)
		       (return))
	       
	       (let ((,vers (call-body-vers ,cbody))
		     (procnum (call-body-proc ,cbody))
		     func args-decoder res-encoder)
		 (case ,vers
		   ,@version-cases)
		 
		 (if* (null func)
		    then (user::logit-stamp "~
~a: Sending program version mismatch response (requested version was ~D) to ~A~%" 
				      ,program
				      ,vers
				      (peer-dotted ,peer))
			 (send-prog-mismatch-reply
			  ,peer (rpc-msg-xid ,msg) *nullverf* 
			  ,lowest-version ,highest-version)
			 (return))
		 
		 (if* (eq res-encoder :ignore)
		    then (funcall func 
				  (funcall args-decoder ,msgxdr) 
				  ,vers
				  ,peer
				  ,cbody)
		    else (with-successful-reply (,res ,peer 
						      (rpc-msg-xid ,msg) 
						      *nullverf*)
			   (funcall res-encoder ,res 
				    (funcall func 
					     (funcall args-decoder ,msgxdr)
					     ,vers
					     ,peer
					     ,cbody))))))))))))

(defmacro def-rpc-program ((prgname prognum &key port) definitions)
  (let ((program (symbol-name prgname))
	(usock (gensym))
	(tsock (gensym)))

    (let (all-versions ;; for use in portmapper call
	  proc-versions) ;; for use in main loop
      (dolist (vdef definitions)
	(let ((versions (first vdef)))
	  (if (not (listp versions))
	      (setf versions (list versions)))
	  
	  (setf all-versions (append all-versions versions))
 
	  (push (cons versions (rest vdef)) proc-versions)))
      
      (setf all-versions (sort all-versions #'<))
      (setf proc-versions (nreverse proc-versions))
      
      `(defun ,prgname ()
	 (declare (optimize (speed 3)))
	 (def-rpc-program-1 (,program ,prognum ,all-versions ,usock ,tsock
				      :port ,port)
	     (def-rpc-program-main ,program ,prognum ,proc-versions 
				   ,usock ,tsock
				   ,(first all-versions)
				   ,(car (last all-versions))))))))

(defstruct rpc-server
  tcpsock
  udpsock
  tcpclientlist
  (buffer (make-array #.*rpc-buffer-size* :element-type '(unsigned-byte 8)))
  (peer (make-rpc-peer))) ;; peer associated with the last message received.

(defun cleanup-tcp-client-connection (server stream &key condition)
  ;; Closes down the tcp stream and removes it from server's tcp client list
  (when *rpc-debug* 
    (if* condition
       then (user::logit-stamp "client abort (~a) ~S~%" condition stream)
       else (user::logit-stamp "client disconnected from ~S~%" stream)))
  (ignore-errors (close stream))
  (ignore-errors (close stream :abort t))
  (setf (rpc-server-tcpclientlist server)
    (delete stream (rpc-server-tcpclientlist server))))

(defun rpc-receive-and-handle-message (server handler)
  ;; Gets an RPC message and calls handler with the message and peer, handling
  ;; socket-error's in the process and returning (return value undefined).
  (let ((message (rpc-get-message server)))
    (handler-bind
	((socket-error
	  (lambda (c)
	    (let ((s (stream-error-stream c)))
	      (when (member s (rpc-server-tcpclientlist server))
		(cleanup-tcp-client-connection server s :condition c)
		(return-from rpc-receive-and-handle-message))))))
      (funcall handler message (rpc-server-peer server)))))

;; Returns an xdr
;; Also fills in 'peer' slot of 'server'.
(defun rpc-get-message (server)
  (symbol-macrolet ((clientlist (rpc-server-tcpclientlist server)))
    (let ((tcpsock (rpc-server-tcpsock server))
	  (udpsock (rpc-server-udpsock server))
	  (buffer (rpc-server-buffer server))
	  (peer (rpc-server-peer server))
	  waitlist
	  readylist
	  record)

      (loop
	(setf waitlist clientlist)
	(if tcpsock
	    (push tcpsock waitlist))
	(if udpsock
	    (push udpsock waitlist))
	;;(logit "waiting for input.~%")
	;;(logit "waitlist is ~S~%" waitlist)
	(handler-case (setf readylist (mp:wait-for-input-available waitlist))
	  (socket-error (c)
	    (case (stream-error-identifier c)
	      (:connection-reset 
	       (let ((stream (stream-error-stream c)))
		 (cleanup-tcp-client-connection server stream :condition c)
		 nil))
	      (t
	       (error c)))))
	
	;;(logit "readylist is ~A~%" readylist)
	
	(when (member tcpsock readylist)
	  (if *rpc-debug* 
	      (user::logit-stamp "~
Accepting new tcp connection and adding it to the client list.~%"))
	  (push (socket:accept-connection tcpsock) clientlist)
	  (setf readylist (delete tcpsock readylist)))
	
	(when (member udpsock readylist)
	  (multiple-value-bind (vec count addr port)
	      (handler-case (socket:receive-from udpsock (length buffer) 
						 :buffer buffer)
		(socket-error (c) 
		  (if *rpc-debug* 
		      (user::logit-stamp "Ignoring socket error: ~a~%" c))
		  nil)
		(errno-stream-error (c)
		  (case (stream-error-code c)
		    (10035 ;; WSAEWOULDBLOCK.. ignore
		     (if *rpc-debug* 
			 (user::logit-stamp "Ignoring stream error: ~a~%" c))
		     nil)
		    (t
		     (user::logit-stamp "rpc-get-message: Unexpected stream error during receive-from: ~a~%" c)
		     (error c))))
		(t (c)
		  (user::logit-stamp "rpc-get-message: Unexpected error during receive-from: ~a~%" c)
		  (error c)))

	    (when vec
	      (setf (rpc-peer-type peer) :datagram)
	      (setf (rpc-peer-socket peer) udpsock)
	      (setf (rpc-peer-addr peer) addr)
	      (setf (rpc-peer-port peer) port)
	      
	      (return-from rpc-get-message (create-xdr :vec vec :size count))))
	  
	  (setf readylist (delete udpsock readylist)))
	
	;; all remaining entries on readylist will be tcp clients
	(dolist (s readylist)
	  (setf record (read-record s buffer))
	  (if* (null record)
	     then (cleanup-tcp-client-connection server s)
	     else (setf (rpc-peer-type peer) :stream)
		  (setf (rpc-peer-socket peer) s)
		  (setf (rpc-peer-addr peer) (socket:remote-host s))
		  
		  (return-from rpc-get-message (create-xdr :vec record))))))))


(eval-when (compile load eval)
  (export '(def-rpc-program 
	    make-rpc-server rpc-server-peer 
	    rpc-get-message rpc-receive-and-handle-message
	    with-rpc-sockets with-portmapper-mappings with-valid-call)))
	    
	    
