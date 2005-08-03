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
;; $Id: sunrpc-service.cl,v 1.1 2005/08/03 20:56:34 dancy Exp $

(in-package :user)

;;;; Easy interface for establishing an RFC service.

;;;; Limitations: Only supports one set of procedures across all
;;;;  versions of the service.  

;;;;  Establishes sockets.
;;;;  Registers with portmapper
;;;;  Loops collecting messages and calling dispatch function
;;;;  Closes sockets when terminated

;;;; Input needed:
;;;;  String naming service (for debug output).
;;;;   range of versions
;;;;   Mapping of procedure numbers to procedures.

(defmacro with-rpc-socket ((service sym &rest params) &body body)
  (let ((c (gensym)))
    `(let ((,sym (handler-case (socket:make-socket ,@params)
		   (error (,c)
		     (bailout "
~a: Error while creating socket: ~a~%" ,service ,c)))))
       (unwind-protect
	   (progn ,@body)
	 (ignore-errors (close ,sym))
	 (ignore-errors (close ,sym :abort t))))))


;; versions may be a single number or a sorted list of numbers.
(defun rpc-service (service prog versions procedures)
  (declare (optimize (speed 3)))
  
  (if (not (listp versions))
      (setf versions (list versions)))
  
  (with-rpc-socket (service tcpsock :type :hiper :connect :passive)
    (with-rpc-socket (service udpsock :type :datagram)
      (logit "~a: Using UDP port ~d~%" (socket:local-port udpsock))
      (logit "~a: Using TCP port ~d~%" (socket:local-port tcpsock))
      (with-portmapper-mapping (prog versions tcpsock IPPROTO_TCP)
	(with-portmapper-mapping (prog versions udpsock IPPROTO_UDP)
	  (let* ((buffer (make-array #.(* 64 1024) 
				     :element-type '(unsigned-byte 8)))
		 (server (make-rpc-server :tcpsock tcpsock
					  :udpsock udpsock
					  :buffer buffer)))
	    (declare (dynamic-extent buffer server))
	    (rpc-service-inner server service prog versions procedures)))))))

(defun rpc-service-inner (server service prog versions procedures) 
  (loop
    (multiple-value-bind (xdr peer)
	(rpc-get-message server)
      (block nil
	(let* ((msg (create-rpc-msg xdr))
	       (cbody (rpc-msg-cbody msg))
	       (peer-addr (socket:ipaddr-to-dotted
			   (rpc-peer-addr peer)))
	       vers proc)
	    
	  (when (null cbody)
	    (logit "~a: Invalid message from ~A~%" service peer-addr)
	    (return))
	    
	  (setf vers (call-body-vers cbody))
	  (setf proc (call-body-proc cbody))
	    
	  (when (/= (call-body-prog cbody) prog)
	    (logit "~a: Sending program unavailable response for prog=~D to ~A~%"
		   service
		   (call-body-prog cbody)
		   peer-addr)
	    (rpc-send-prog-unavail peer (rpc-msg-xid msg) 
				   (null-verf))
	    (return))
	    
	  (when (not (member vers versions))
	    (logit "~a: Sending program version mismatch response (requested version was ~d) to ~a~%" 
		   service vers peer-addr)
	    (rpc-send-prog-mismatch peer (rpc-msg-xid msg) 
				    (null-verf) 
				    (first versions)
				    (car (last versions)))
	    (return))
	    
	  (let ((handler (cdr (assoc proc procedures))))
	    (when (null handler) 
	      ;;should send a negative response
		  (logit "~a: unhandled procedure ~D requested by ~A~%"
			 service proc peer-addr)
		  (return))
	      
	    (funcall handler peer (rpc-msg-xid msg) cbody)))))))
