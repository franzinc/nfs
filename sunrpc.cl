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
;; $Id: sunrpc.cl,v 1.21 2004/02/19 22:33:10 dancy Exp $

(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

;; enum msg_type
(eval-when (compile load eval)
(defconstant *CALL* 0)
(defconstant *REPLY* 1)

;; enum reply_stat
(defconstant *MSG_ACCEPTED* 0)
(defconstant *MSG_DENIED* 1)

;; enum accept_stat
(defconstant *SUCCESS* 0)
(defconstant *PROG_UNAVAIL* 1)
(defconstant *PROG_MISMATCH* 2)
(defconstant *PROC_UNAVAIL* 3)
(defconstant *GARBAGE_ARGS* 4)

;; enum reject_stat
(defconstant *RPC_MISMATCH* 0)
(defconstant *AUTH_ERROR* 1)

;; enum auth_stat
(defconstant *AUTH_BADCRED* 1)
(defconstant *AUTH_REJECTEDCRED* 2)
(defconstant *AUTH_BADVERF* 3)
(defconstant *AUTH_REJECTEDVERF* 4)
(defconstant *AUTH_TOOWEAK* 5)
)

(defstruct rpc-peer
  type
  socket
  addr
  port)


(defstruct rpc-server
  tcpsock
  udpsock
  tcpclientlist
  (buffer (make-array (* 1024 64) :element-type '(unsigned-byte 8))))

(defparameter *rpc-debug* nil)

;; returns an xdr
(defun rpc-get-message (server)
  (symbol-macrolet ((clientlist (rpc-server-tcpclientlist server)))
    (let ((tcpsock (rpc-server-tcpsock server))
	  (udpsock (rpc-server-udpsock server))
	  (buffer (rpc-server-buffer server))
	  waitlist
	  readylist
	  record)
      (loop
	(setf waitlist clientlist)
	(if tcpsock
	    (push tcpsock waitlist))
	(if udpsock
	    (push udpsock waitlist))
	;;(format t "waiting for input.~%")
	;;(format t "waitlist is ~S~%" waitlist)
	(handler-case (setf readylist (mp:wait-for-input-available waitlist))
	  (socket-error (c)
	    (case (stream-error-identifier c)
	      (:connection-reset 
	       (let ((stream (stream-error-stream c)))
		 (if *rpc-debug* (format t "closing error socket ~S~%" stream))
		 (close stream)
		 (setf clientlist (remove stream clientlist))
		 nil))
	      (t 
	       (error c)))))
	
	;;(format t "readylist is ~A~%" readylist)
	
	(when (member tcpsock readylist)
	  (if *rpc-debug* 
	      (format t "~
Accepting new tcp connection and adding it to the client list.~%"))
	  (push (socket:accept-connection tcpsock) clientlist)
	  (setf readylist (remove tcpsock readylist)))
	
	(when (member udpsock readylist)
	  (multiple-value-bind (vec count addr port)
	      (handler-case (socket:receive-from udpsock (length buffer) :buffer buffer)
		(socket-error (c) 
		  (if *rpc-debug* 
		      (format t "Ignoring error condition ~S~%" c))
		  nil))
	    (when vec
	      (return-from rpc-get-message 
		(values (create-xdr :vec vec :size count) 
			(make-rpc-peer :type :datagram :socket udpsock
				       :addr addr :port port)))))
	  
	  (setf readylist (remove udpsock readylist)))
	
	;; all remaining entries on readylist will be tcp clients
	(dolist (s readylist)
	  (setf record (read-record s buffer))
	  (if* (null record)
	     then
		  (if *rpc-debug* (format t "Client ~s disconnected.~%" s))
		  (ignore-errors (close s))
		  (ignore-errors (close s :abort t))
		  (setf clientlist (remove s clientlist))
	     else
		  (return-from rpc-get-message
		    (values (create-xdr :vec record)
			    (make-rpc-peer :type :stream :socket s
					   :addr (socket:remote-host s))))))))))

  
  
(defun read-int-from-stream (stream)
  (declare (optimize (speed 3)))
  (let* ((vec (make-array 1 :element-type '(signed-byte 32)))
	 (res (read-vector vec stream :endian-swap :network-order)))
    (declare 
     (dynamic-extent vec)
     (type (simple-array (signed-byte 32) (*)) vec)
     (type (unsigned-byte 8) res))
    (if (= res 0)        
        (return-from read-int-from-stream nil));; indicate EOF
    (if (/= res 4)
	(error "Incomplete read during read-int-from-stream"))
    (aref vec 0)))


(defun read-record (stream buffer)
  (handler-case 
      (let ((size (read-int-from-stream stream)))
	(if (null size)
	    (return-from read-record nil)) ;; indicate EOF
	(when (>= size 0)
	  (error "read-record: Fragments aren't handled yet"))
	(setf size (logand size #x7fffffff))
	(if (> size (length buffer))
	    (error "read-record: Record is too big for the buffer! (~D > ~D)"
		   size (length buffer)))
	;;(format t "Message is ~d bytes~%" size)
	(read-complete-vector buffer stream size))
    (t (c)
      (format t "read-record got error ~A~%Returning nil~%" c)
      nil)))

(defun read-complete-vector (vec stream end)
  (declare
   (optimize (speed 3))
   (type fixnum end))
  (let ((pos 0))
    (declare (type fixnum pos))
    (while (/= pos end)
      (setf pos (read-vector vec stream :start pos :end end))))
  vec)
  

(defstruct rpc-msg 
  xid
  mtype ;; CALL = 0, REPLY = 1
  cbody ;; (for CALL)
  rbody ;; (for REPLY)
  )

(defun create-rpc-msg (xdr)
  (let* ((msg (make-rpc-msg))
         (xid (xdr-unsigned-int xdr))
         (mtype (xdr-int xdr)))
    (setf (rpc-msg-xid msg) xid)
    (setf (rpc-msg-mtype msg) mtype)
    (ecase mtype
      (#.*CALL* 
       (setf (rpc-msg-cbody msg) (create-call-body-from-xdr xdr)))
      (#.*REPLY*
       (setf (rpc-msg-rbody msg) (create-reply-body-from-xdr xdr))))

    msg))

(defstruct call-body
  rpcvers
  prog
  vers
  proc
  cred
  verf
  params
  )

(defun create-call-body-from-xdr (xdr)
  (let ((cbody (make-call-body)))
    (setf (call-body-rpcvers cbody) (xdr-int xdr))
    (unless (= 2 (call-body-rpcvers cbody))
      (error "create-call-body-from-xdr: Unsupported RPC Version requested: ~D~%"
	     (call-body-rpcvers cbody)))
    (setf (call-body-prog cbody) (xdr-int xdr))
    (setf (call-body-vers cbody) (xdr-int xdr))
    (setf (call-body-proc cbody) (xdr-int xdr))
    ;;(format t "create-call-body-from-xdr: Getting credentials~%")
    (setf (call-body-cred cbody) (xdr-opaque-auth xdr))
    ;;(format t "create-call-body-from-xdr: Getting verifier~%")
    (setf (call-body-verf cbody) (xdr-opaque-auth xdr))
    (setf (call-body-params cbody) (xdr-xdr xdr))
    cbody))

(defun pprint-cbody (cbody)
  (format t "RPC Version: ~D~%" (call-body-rpcvers cbody))
  (format t "Program: ~D~%" (call-body-prog cbody))
  (format t "Program version: ~D~%" (call-body-vers cbody))
  (format t "Program procedure: ~D~%" (call-body-proc cbody))
  ;;(format t "Cred: ~S~%" (call-body-cred cbody))
  ;;(format t "Verf: ~S~%" (call-body-verf cbody))
  )

(defstruct reply-body
  stat ;; MSG_ACCEPTED = 0, MSG_DENIED = 1
  areply ;; (for MSG_ACCEPTED)
  rreply ;; (for MSG_DENIED)
  )

(defstruct accepted-reply
  verf
  stat
  results ;; (for SUCCESS)
  mismatch-info ;; (for PROG_MISMATCH)
  )

(defstruct mismatch-info
  low
  high)

(defstruct rejected-reply
  reject-stat ;; RPC_MISMATCH = 0, AUTH_ERROR = 1
  mismatch-info ;; (for RPC_MISMATCH)
  auth-stat ;; (for AUTH_ERROR)
  )

(defun create-reply-body-from-xdr (xdr)
  (let* ((reply-stat (xdr-int xdr))
	 (reply-body (make-reply-body :stat reply-stat)))
    (ecase reply-stat
      (#.*MSG_ACCEPTED*
       (let ((ar (make-accepted-reply :verf (xdr-opaque-auth xdr)
				      :stat (xdr-int xdr))))
	 (setf (reply-body-areply reply-body) ar)
	 (ecase (accepted-reply-stat ar)
	   (#.*SUCCESS* 
	    (setf (accepted-reply-results ar) (xdr-xdr xdr)))
	   (#.*PROG_MISMATCH* 
	    (setf (accepted-reply-mismatch-info ar)
	      (make-mismatch-info
	       :low (xdr-unsigned-int xdr)
	       :high (xdr-unsigned-int xdr))))
	   ((#.*PROG_UNAVAIL* #.*PROC_UNAVAIL* #.*GARBAGE_ARGS*)
	    ))))
      (#.*MSG_DENIED*
       (error "MSG_DENIED not done yet")))
    
    reply-body))

(defun verbose-accept-stat (code)
  (ecase code
    (#.*SUCCESS*
     "success")
    (#.*PROG_UNAVAIL*
     "program unavailable")
    (#.*PROG_MISMATCH*
     "program version mismatch")
    (#.*PROC_UNAVAIL*
     "procedure unavailable")
    (#.*GARBAGE_ARGS*
     "garbage arguments")))
  

;; useful for routines that just want to fail if a reply failed
;; for some reason.
(defmacro with-good-reply-msg ((msg expected-xid xdrsym) &body body)
  (let ((m (gensym))
	(xid (gensym))
	(thing (gensym)))
    `(let ((,m ,msg)
	   (,xid ,expected-xid))
       (if (/= (rpc-msg-xid ,m) ,xid)
	   (error "Got XID ~D but expected ~D" (rpc-msg-xid ,m) ,xid))
       (let ((,thing (rpc-msg-rbody ,m)))
	 (if (/= *MSG_ACCEPTED* (reply-body-stat ,thing))
	     (error "Message was denied"))
	 (setf ,thing (reply-body-areply ,thing))
	 (if (/= *SUCCESS* (accepted-reply-stat ,thing))
	     (error "Reply status: ~A (code: ~D)" 
		    (verbose-accept-stat (accepted-reply-stat, thing))
		    (accepted-reply-stat ,thing)))
	 (let ((,xdrsym (car (accepted-reply-results ,thing))))
	   ,@body)))))

(defun rpc-send (xdr peer)
  (ecase (rpc-peer-type peer)
    (:stream
     (let ((sizevec (make-array 1 :element-type '(unsigned-byte 32))))
       (declare 
	(dynamic-extent sizevec)
	(type (simple-array (unsigned-byte 32) (*)) vec))
       (set-uint-in-array (logior #x80000000 (xdr-size xdr)) sizevec 0)
       (ignore-errors
	(write-vector sizevec (rpc-peer-socket peer))
	(write-vector (xdr-get-complete-vec xdr) (rpc-peer-socket peer)
		      :end (xdr-size xdr))
	(force-output (rpc-peer-socket peer)))))
    (:datagram
     (ignore-errors (socket:send-to (rpc-peer-socket peer) 
				    (xdr-get-complete-vec xdr) 
				    (xdr-size xdr) 
				    :remote-host (rpc-peer-addr peer)
				    :remote-port (rpc-peer-port peer))))))

#+ignore
(defun rpc-send (xdr peer)
  (ecase (rpc-peer-type peer)
    (:stream
     (let ((sizevec (make-array 1 :element-type '(signed-byte 32))))
       (declare 
	(dynamic-extent sizevec)
	(type (simple-array (signed-byte 32) (*)) vec))
       (setf (aref sizevec 0) (logior #x80000000 (xdr-size xdr)))
       (ignore-errors
	(write-vector sizevec (rpc-peer-socket peer) :endian-swap :network-order)
	(write-vector (xdr-get-complete-vec xdr) (rpc-peer-socket peer)
		      :end (xdr-size xdr))
	(force-output (rpc-peer-socket peer)))))
    (:datagram
     (ignore-errors (socket:send-to (rpc-peer-socket peer) 
				    (xdr-get-complete-vec xdr) 
				    (xdr-size xdr) 
				    :remote-host (rpc-peer-addr peer)
				    :remote-port (rpc-peer-port peer))))))

(defun rpc-send-reply (peer xid rbody)  ;; rbody should be an xdr
  (let ((xdr (create-xdr :direction :build)))
    (xdr-unsigned-int xdr xid)
    (xdr-int xdr 1) ;; REPLY
    (xdr-xdr xdr rbody)
    (rpc-send xdr peer)))
    
(defun send-accepted-reply (peer xid verf stat reply)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-unsigned-int xdr xid)
    (xdr-int xdr 1) ;; REPLY
    (xdr-int xdr 0) ;; MSG_ACCEPTED
    (xdr-xdr xdr verf) 
    (xdr-int xdr stat) 
    (xdr-xdr xdr reply)
    (rpc-send xdr peer)))

(defun send-successful-reply (peer xid verf results)
  (send-accepted-reply peer xid verf 0 results))



(defmacro with-successful-reply ((xdr-name peer xid verf) &body body)
  (let ((xdrbuf (gensym)))
    `(let* ((,xdrbuf (make-array #.(* 64 1024) :element-type '(unsigned-byte 8)))
	    (,xdr-name (create-xdr :direction :build :vec ,xdrbuf)))
       (declare 
	(dynamic-extent ,xdrbuf))
       (xdr-unsigned-int ,xdr-name ,xid)
       (xdr-int ,xdr-name *REPLY*) 
       (xdr-int ,xdr-name *MSG_ACCEPTED*) 
       (xdr-xdr ,xdr-name ,verf) 
       (xdr-int ,xdr-name *SUCCESS*) 
       ,@body
       (rpc-send ,xdr-name ,peer))))



(defun rpc-send-rejected-reply (peer xid rreply)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr 1) ;; MSG_DENIED
    (xdr-xdr xdr rreply)
    (rpc-send-reply peer xid xdr)))

(defun rpc-send-auth-error-rejected-reply (peer xid stat)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-int xdr 1) ;; AUTH_ERROR
    (xdr-int xdr stat) ;; auth_stat
    (rpc-send-rejected-reply peer xid xdr)))

(defun rpc-send-prog-unavail (peer xid verf)
  (let ((xdr (create-xdr :direction :build)))
    (send-accepted-reply peer xid verf 1 xdr)))

(defun rpc-send-proc-unavail (peer xid verf)
  (let ((xdr (create-xdr :direction :build)))
    (send-accepted-reply peer xid verf 3 xdr)))

(defun rpc-send-prog-mismatch (peer xid verf lowest highest)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-unsigned-int xdr lowest)
    (xdr-unsigned-int xdr highest)
    (send-accepted-reply peer xid verf 2 xdr)))
  
;;;; client stuff

(defun rpc-connect-to-peer (host port proto)
  (let* ((type (ecase proto
		 (:tcp 
		  :stream)
		 (:udp
		  :datagram)))
	 (socket (socket:make-socket 
		  :type type
		  :remote-host host 
		  :remote-port port)))
    (make-rpc-peer :type type 
		   :socket socket 
		   :addr (socket:remote-host socket)
		   :port port)))

(defun rpc-disconnect (peer)
  (close (rpc-peer-socket peer))) 

(defmacro with-rpc-peer ((peer host port proto) &body body)
  `(let (,peer)
     (unwind-protect
	 (progn
	   (setf ,peer (rpc-connect-to-peer ,host ,port ,proto))
	   ,@body)
       (if ,peer
	   (rpc-disconnect ,peer)))))

;; returns xdr
(defun rpc-get-reply (peer &key buffer)
  (when (null buffer)
    (setf buffer (make-array #.(* 64 1024) :element-type '(unsigned-byte 8))))
  
  (create-rpc-msg
   (ecase (rpc-peer-type peer)
     (:datagram
      (multiple-value-bind (vec count)
	  (socket:receive-from (rpc-peer-socket peer) 
			       (length buffer)
			       :buffer buffer)
	(create-xdr :vec vec :size count)))
     (:stream
      (let ((record (read-record (rpc-peer-socket peer) buffer)))
	(create-xdr :vec record))))))

(defun rpc-send-call (peer xid cbody) ;; cbody should be an xdr
  (let ((xdr (create-xdr :direction :build)))
    (xdr-unsigned-int xdr xid)
    (xdr-int xdr 0) ;; CALL
    (xdr-xdr xdr cbody)
    (rpc-send xdr peer)))

(defun null-verf ()
  (let ((xdr (create-xdr :direction :build)))
    (xdr-auth-null xdr)
    xdr))

(defun null-auth ()
  (null-verf))

;; unfortunate name... hard to distinguish from create-call-body-from-xdr
(defun rpc-make-call-body (prog vers proc cred verf rest)
  (let ((xdr (create-xdr :direction :build)))
    (xdr-unsigned-int xdr 2) ;; rpcvers
    (xdr-unsigned-int xdr prog)
    (xdr-unsigned-int xdr vers)
    (xdr-unsigned-int xdr proc)
    (xdr-xdr xdr cred)
    (xdr-xdr xdr verf)
    (xdr-xdr xdr rest)
    xdr))

(defun callrpc (host prognum versnum procnum proto inproc in
		&key (retries 3)
		     (timeout 5)
		     port
		     outproc)
  (when(null port)
    (setf port (portmap-getport-client host prognum versnum proto))
    (if (= port 0)
	(error "program not registered")))
  
  (with-rpc-peer (peer host port proto)
    (let ((xdr (create-xdr :direction :build))
	  (xid (random #.(expt 2 32))))

      (if inproc
	  (funcall inproc xdr in))
      
      (dotimes (i retries) 
	(rpc-send-call
	 peer
	 xid
	 (rpc-make-call-body prognum versnum procnum
			     (null-auth) (null-verf) xdr))
	(let ((msg (mp:with-timeout (timeout :timeout)
		     (rpc-get-reply peer))))
	  (if (not (eq msg :timeout))
	      (with-good-reply-msg (msg xid results)
		(return-from callrpc 
		  (if outproc
		      (funcall outproc results)
		    results))))))
      
      (error "rpc call failed"))))
