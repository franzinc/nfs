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
;; $Id: portmap.cl,v 1.15 2004/02/03 20:58:13 dancy Exp $

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
(defconstant IPPROTO_TCP 6)
(defconstant IPPROTO_UDP 17)
)


(defstruct mapping
  (prog 0)
  (vers 0)
  (prot 0)
  (port 0))
  
(defparameter *pmap-gate* (mp:make-gate nil))
(defparameter *mappings* nil)
(defparameter *pmap-tcp-socket* nil)
(defparameter *pmap-udp-socket* nil)

(defparameter *portmap-debug* nil)

(defparameter *use-system-portmapper* :auto)

(defun make-pmap-sockets ()
  (unless *pmap-tcp-socket*
    (setf *pmap-tcp-socket*   
      (socket:make-socket :type :stream
                          :connect :passive
                          :local-port *pmapport*
                          :reuse-address t)))
  (unless *pmap-udp-socket*
    (setf *pmap-udp-socket*
      (socket:make-socket :type :datagram
                          :local-port *pmapport*))))
      

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
      (format t "Using system portmapper.~%")
      (setf *use-system-portmapper* t)
      (mp:open-gate *pmap-gate*)))
  
  (when (not *use-system-portmapper*)
    (make-pmap-sockets)
    (portmap-add-program *pmapprog* *pmapvers* *pmapport* IPPROTO_TCP)
    (portmap-add-program *pmapprog* *pmapvers* *pmapport* IPPROTO_UDP)
    (let* ((buffer (make-array #.(* 64 1024) :element-type '(unsigned-byte 8)))
	   (server (make-rpc-server :tcpsock *pmap-tcp-socket*
				    :udpsock *pmap-udp-socket*
				    :buffer buffer)))
      (declare (dynamic-extent buffer server))
      (mp:open-gate *pmap-gate*)
      (loop
	(multiple-value-bind (xdr peer)
	    (rpc-get-message server)
	  (portmap-message-handler xdr peer))))))
  
  
      
(defun portmap-message-handler (xdr peer)
  (let (msg cbody)
    (setf msg (create-rpc-msg xdr))
    (setf cbody (rpc-msg-cbody msg))
    (if *portmap-debug* (write-line ""))
    ;;(pprint-cbody cbody)
    (unless (= (rpc-msg-mtype msg) 0)
      (error "Unexpected data!"))
    (when (and (= (call-body-prog cbody) *pmapprog*)
               (= (call-body-vers cbody) *pmapvers*))
      (case (call-body-proc cbody)
	(#.*pmap-proc-null*
	 (portmap-null peer (rpc-msg-xid msg)))
	(#.*pmap-proc-dump*
	 (portmap-dump peer (rpc-msg-xid msg)))
	(#.*pmap-proc-getport*
	 (portmap-getport peer (rpc-msg-xid msg) (call-body-params cbody)))
	(#.*pmap-proc-callit* 
	 (portmap-callit peer (rpc-msg-xid msg) (call-body-params cbody)))
	(t 
	 ;; should send a negative response
	 (format t "portmap: unhandled procedure ~D~%"
		 (call-body-proc cbody)))))))

(defun portmap-null (peer xid)
  (if *portmap-debug* (format t "portmap-null~%~%"))
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (null-verf) xdr)))


(defun portmap-dump (peer xid)
  (if *portmap-debug* (format t "portmap-dump~%~%"))
  (let ((xdr (create-xdr :direction :build)))
    (dolist (mapping *mappings*)
      (xdr-int xdr 1) ;; indicate that data follows
      (xdr-int xdr (mapping-prog mapping))
      (xdr-int xdr (mapping-vers mapping))
      (xdr-int xdr (mapping-prot mapping))
      (xdr-int xdr (mapping-port mapping)))
    (xdr-int xdr 0) ;; no more data
    (send-successful-reply peer xid (null-verf) xdr)))

(defun portmap-getport (peer xid params)
  (let* ((m (with-xdr-xdr (params)
	      (xdr-portmap-mapping params)))
         (m2 (locate-matching-mapping m))
         (xdr (create-xdr :direction :build)))
    (if *portmap-debug* (format t "portmap-getport: ~A~%" m))
    (if* m2
       then
	    (if *portmap-debug*
		(format t "Program found. Returning port ~D~%~%" 
			(mapping-port m2)))
	    (xdr-unsigned-int xdr (mapping-port m2))
       else
	    (if *portmap-debug*
		(format t "Program not found.  Returning 0~%~%"))
	    (xdr-unsigned-int xdr 0))
    (send-successful-reply peer xid (null-verf) xdr)))

#|
(defun portmap-callit (peer xid params)
  (declare (ignore xid peer))
  (with-xdr-xdr (params)
    (let ((prog (xdr-unsigned-int params))
	  (vers (xdr-unsigned-int params))
	  (proc (xdr-unsigned-int params)))
      (format t "portmap-callit(~D, ~D, ~D, ...)~%~%" prog vers proc))))
|#

(defun portmap-callit (peer xid params)
  (declare (ignore xid peer params))
  )


    
(defun locate-matching-mapping (m)
  (dolist (m2 *mappings*)
    (when (and
           (= (mapping-prog m) (mapping-prog m2))
           ;;(= (mapping-vers m) (mapping-vers m2)) ;; for amd
           (= (mapping-prot m) (mapping-prot m2)))
      (return-from locate-matching-mapping m2)))
  nil)

(defun portmap-add-program (prog vers port proto)
  (let ((mapping (make-mapping
                  :prog prog
                  :vers vers
                  :prot proto
                  :port port)))
    (if* *use-system-portmapper*
       then
	    (portmap-set-client mapping)
       else
	    (unless (find mapping *mappings* :test #'equalp)
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
    (format t "   program vers proto   port~%")
    (dolist (m mappings)
      (format t "~10d ~4d ~5@a ~6d~%" 
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
   (callrpc "127.0.0.1" *pmapprog* *pmapvers* *pmap-proc-null* :udp
	    nil nil :port *pmapport*)))


(defmacro with-portmapper-mapping ((prog vers port proto) &body body)
  (let ((progsym (gensym))
	(verssym (gensym))
	(portsym (gensym))
	(protosym (gensym)))
    `(let ((,progsym ,prog)
	   (,verssym ,vers)
	   (,portsym ,port)
	   (,protosym ,proto))
       ;; clean up.  Important when using system portmapper.
       ;; however,  this could affect existing servers.  
       ;; XXXXX --- unsafe
       (ignore-errors
	(portmap-remove-program ,progsym ,verssym ,portsym ,protosym))
       (portmap-add-program ,progsym ,verssym ,portsym ,protosym)
       (unwind-protect
	   (progn
	     ,@body)
	 (portmap-remove-program ,progsym ,verssym ,portsym ,protosym)))))

