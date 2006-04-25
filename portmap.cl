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
;; $Id: portmap.cl,v 1.29 2006/04/25 21:14:26 dancy Exp $

;; portmapper


(in-package :user)

(eval-when (compile load eval)
(defconstant *pmapport* 111)
(defconstant *pmapprog* 100000)
(defconstant *pmapvers* 2)
(defconstant *pmap-proc-null* 0)
(defconstant *pmap-proc-set* 1)
(defconstant *pmap-proc-unset* 2)
(defconstant *pmap-proc-getport* 3)
(defconstant *pmap-proc-dump* 4)
(defconstant *pmap-proc-callit* 5)
)


(defstruct (mapping
	    (:print-object mapping-printer))
  (prog 0)
  (vers 0)
  (prot 0)
  (port 0))

(defun mapping-printer (obj stream)
  (format stream "[Prg: ~d, V: ~d, ~a, Port: ~d]"
	  (mapping-prog obj)
	  (mapping-vers obj)
	  (protocol-to-string (mapping-prot obj))
	  (mapping-port obj)))
  
(defparameter *pmap-gate* (mp:make-gate nil))
(defparameter *mappings* nil)
(defparameter *pmap-tcp-socket* nil)
(defparameter *pmap-udp-socket* nil)

(defparameter *portmap-debug* nil)

(defparameter *use-system-portmapper* :auto)

;; XXX -- Don't use reuse-address on Windows.

(defun make-pmap-sockets ()
  (unless *pmap-tcp-socket*
    (handler-case 
	(setf *pmap-tcp-socket*   
	  (socket:make-socket :type :hiper
			      :connect :passive
			      :local-port *pmapport*
			      ;;:reuse-address t
			      ))
      (socket-error (c)
	(if (eq (stream-error-identifier c) :address-in-use)
	    (bailout "Cannot start portmapper.  Address already in use.~%")
	  (bailout "~
Unexpected error while creating portmapper tcp socket: ~a~%" c)))
      (error (c)
	(bailout "~
Unexpected error while creating portmapper tcp socket: ~a~%" c))))
  
  (unless *pmap-udp-socket*
    (handler-case
	(setf *pmap-udp-socket*
	  (socket:make-socket :type :datagram
			      :local-port *pmapport*))
      (error (c) 
	(bailout "~
Unexpected error while creating portmapper udp socket: ~a~%" c)))))


(defun close-pmap-sockets ()
  (when *pmap-tcp-socket*
    (close *pmap-tcp-socket*)
    (setf *pmap-tcp-socket* nil))
  (when *pmap-udp-socket*
    (close *pmap-udp-socket*)
    (setf *pmap-udp-socket* nil)))


(defun portmapper ()
  (when (eq *use-system-portmapper* :auto)
    (setf *use-system-portmapper* nil)
    (when (ping-portmapper)
      (logit "PMAP: Using system portmapper.~%")
      (setf *use-system-portmapper* t)
      (mp:open-gate *pmap-gate*)))
  
  (when (not *use-system-portmapper*)
    (make-pmap-sockets)
    (portmap-add-program *pmapprog* *pmapvers* *pmapport* IPPROTO_TCP)
    (portmap-add-program *pmapprog* *pmapvers* *pmapport* IPPROTO_UDP)
    (let ((server (make-rpc-server :tcpsock *pmap-tcp-socket*
				   :udpsock *pmap-udp-socket*)))
      (mp:open-gate *pmap-gate*)
      (loop
	(multiple-value-bind (xdr peer)
	    (rpc-get-message server)
	  (portmap-message-handler xdr peer))))))
  
  
      
(defun portmap-message-handler (xdr peer)
  (block nil
    (let (msg cbody)
      (setf msg (create-rpc-msg xdr))
      (setf cbody (rpc-msg-cbody msg))
      ;;(pprint-cbody cbody)
      (unless (= (rpc-msg-mtype msg) 0)
	(error "Unexpected data!"))
      
      (let ((prog (call-body-prog cbody))
	    (proc (call-body-proc cbody))
	    (vers (call-body-vers cbody))
	    (xid (rpc-msg-xid msg))
	    (dotted (if *portmap-debug* 
			(socket:ipaddr-to-dotted (rpc-peer-addr peer)))))
	
	(if* (/= prog #.*pmapprog*)
	   then (logit "~
PMAP: Sending program unavailable response for prog=~D to ~A~%"
		       prog dotted)
		(rpc-send-prog-unavail peer xid *nullverf*)
		(return))
	
	(if* (/= vers #.*pmapvers*)
	   then (logit "~
PMAP: Sending program version mismatch response (requested version was ~D) to ~A~%" 
		       vers dotted)
		(rpc-send-prog-mismatch peer xid *nullverf* 
					#.*pmapvers* #.*pmapvers*)
		(return))

	(case proc
	  (#.*pmap-proc-null*
	   (portmap-null peer xid))
	  (#.*pmap-proc-dump*
	   (portmap-dump peer xid))
	  (#.*pmap-proc-getport*
	   (portmap-getport peer xid (call-body-params cbody)))
	  (#.*pmap-proc-callit* 
	   (portmap-callit peer xid (call-body-params cbody)))
	  (t 
	   (logit "PMAP: ~a: unhandled procedure ~D~%" dotted proc)
	   (rpc-send-proc-unavail peer xid *nullverf*)))))))


(defun portmap-null (peer xid)
  (if *portmap-debug* 
      (logit "PMAP: ~a: NULL~%"
	     (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid *nullverf* xdr)))


(defun portmap-dump (peer xid)
  (if *portmap-debug* 
      (logit "PMAP: ~a: DUMP~%"
	     (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
  (let ((xdr (create-xdr :direction :build)))
    (dolist (mapping *mappings*)
      (xdr-int xdr 1) ;; indicate that data follows
      (xdr-int xdr (mapping-prog mapping))
      (xdr-int xdr (mapping-vers mapping))
      (xdr-int xdr (mapping-prot mapping))
      (xdr-int xdr (mapping-port mapping)))
    (xdr-int xdr 0) ;; no more data
    (send-successful-reply peer xid *nullverf* xdr)))

(defun portmap-getport (peer xid params)
  (let* ((m (with-xdr-xdr (params)
	      (xdr-portmap-mapping params)))
         (m2 (locate-matching-mapping m))
         (xdr (create-xdr :direction :build)))
    
    (if *portmap-debug* 
	(logit "PMAP: ~A: GETPORT ~A ==> ~a~%" 
	       (socket:ipaddr-to-dotted (rpc-peer-addr peer))
	       m
	       (if m2 (mapping-port m2) "Not found")))
    
    (if* m2
       then (xdr-unsigned-int xdr (mapping-port m2))
       else (xdr-unsigned-int xdr 0))
    
    (send-successful-reply peer xid *nullverf* xdr)))

#|
(defun portmap-callit (peer xid params)
  (declare (ignore xid peer))
  (with-xdr-xdr (params)
    (let ((prog (xdr-unsigned-int params))
	  (vers (xdr-unsigned-int params))
	  (proc (xdr-unsigned-int params)))
      (logit "portmap-callit(~D, ~D, ~D, ...)~%~%" prog vers proc))))
|#

(defun portmap-callit (peer xid params)
  (declare (ignore xid params))
  (if *portmap-debug*
      (logit "PMAP: ~a: CALLIT (not supported)~%"
	     (socket:ipaddr-to-dotted (rpc-peer-addr peer)))))
    
(defun locate-matching-mapping (m)
  (dolist (m2 *mappings*)
    (when (and
           (= (mapping-prog m) (mapping-prog m2))
           ;;(= (mapping-vers m) (mapping-vers m2)) ;; for amd
           (= (mapping-prot m) (mapping-prot m2)))
      (return-from locate-matching-mapping m2)))
  nil)

(defun portmap-add-program (prog vers port proto)
  (let ((mapping (make-mapping :prog prog
			       :vers vers
			       :prot proto
			       :port port)))
    (if* *use-system-portmapper*
       then (portmap-set-client mapping)
       else (unless (find mapping *mappings* :test #'equalp)
	      (push mapping *mappings*)))))

(defun portmap-remove-program (prog vers port proto)
  (let ((mapping (make-mapping
                  :prog prog
                  :vers vers
                  :prot proto
                  :port port)))
    (if* *use-system-portmapper*
       then
	    (portmap-unset-client mapping)
       else
	    (setf *mappings* 
	      (delete mapping *mappings* :test #'equalp)))))

(defstruct callargs
  prog
  vers
  proc
  args)

(defstruct callresult
  port
  res)

;; portmapper client stuff

;; Direction is determined by the existence of the optional
;; 'mapping' argument.
(defun xdr-portmap-mapping (xdr &optional mapping)
  (if* mapping
     then
	  (xdr-unsigned-int xdr (mapping-prog mapping))
	  (xdr-unsigned-int xdr (mapping-vers mapping))
	  (let ((prot (mapping-prot mapping)))
	    (xdr-unsigned-int 
	     xdr 
	     (cond 
	      ((numberp prot)
	       prot)
	      ((eq prot :udp)
	       IPPROTO_UDP)
	      ((eq prot :ucp)
	       IPPROTO_TCP)
	      (t
	       (error "invalid prot in mapping: ~S" mapping)))))
	  (xdr-unsigned-int xdr (mapping-port mapping))
     else
	  (let ((m (make-mapping))) 
	    (setf (mapping-prog m) (xdr-unsigned-int xdr))
	    (setf (mapping-vers m) (xdr-unsigned-int xdr))
	    (setf (mapping-prot m) (xdr-unsigned-int xdr))
	    (setf (mapping-port m) (xdr-unsigned-int xdr))
	    m)))

(defun portmap-getport-client (host prognum versnum proto)
  (let ((m (make-mapping :prog prognum
			 :vers versnum
			 :prot proto)))
    (callrpc host *pmapprog* *pmapvers* *pmap-proc-getport* 
	     :udp #'xdr-portmap-mapping m :port *pmapport*
	     :outproc #'xdr-unsigned-int)))


(defun portmap-dump-client (host)
  (let ((mappings
	 (callrpc host *pmapprog* *pmapvers* *pmap-proc-dump* :udp nil nil 
		  :port *pmapport*
		  :outproc 
		  #'(lambda (xdr) (xdr-list xdr #'xdr-portmap-mapping)))))
    (logit "   program vers proto   port~%")
    (dolist (m mappings)
      (logit "~10d ~4d ~5@a ~6d~%" 
	     (mapping-prog m)
	     (mapping-vers m)
	     (ecase (mapping-prot m)
	       (#.IPPROTO_TCP
		"tcp")
	       (#.IPPROTO_UDP
		"udp"))
	     (mapping-port m)))))

(defun portmap-set-client (mapping)
  (if (/= (callrpc "127.0.0.1" *pmapprog* *pmapvers* *pmap-proc-set*
		   :udp #'xdr-portmap-mapping mapping 
		   :port *pmapport*
		   :outproc #'xdr-unsigned-int)
	  1)
      (error "portmap_set failed")))

(defun portmap-unset-client (mapping)
  (if (/= (callrpc "127.0.0.1" *pmapprog* *pmapvers* *pmap-proc-unset*
		   :udp #'xdr-portmap-mapping mapping 
		   :port *pmapport*
		   :outproc #'xdr-unsigned-int)
	  1)
      (error "portmap_unset failed")))

;; See if a portmapper is running locally.
;; return nil if not.
(defun ping-portmapper ()
  (ignore-errors
   (callrpc "127.0.0.1" #.*pmapprog* #.*pmapvers* #.*pmap-proc-null* :udp
	    nil nil :port #.*pmapport*)))


;; vers may be a list
(defmacro with-portmapper-mapping ((prog vers port proto) &body body)
  (let ((progsym (gensym))
	(verssym (gensym))
	(portsym (gensym))
	(protosym (gensym))
	(v (gensym)))
    `(let ((,progsym ,prog)
	   (,verssym ,vers)
	   (,portsym ,port)
	   (,protosym ,proto))
       (if (not (listp ,verssym))
	   (setf ,verssym (list ,verssym)))
       ;; clean up.  Important when using system portmapper.
       ;; however,  this could affect existing servers.  
       ;; XXXXX --- unsafe
       (dolist (,v ,verssym)
	 (ignore-errors
	  (portmap-remove-program ,progsym ,v ,portsym ,protosym))
	 (portmap-add-program ,progsym ,v ,portsym ,protosym))
       (unwind-protect
	   (progn
	     ,@body)
	 (dolist (,v ,verssym)
	   (portmap-remove-program ,progsym ,v ,portsym ,protosym))))))
