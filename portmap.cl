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
;; $Id: portmap.cl,v 1.11 2002/09/19 20:21:32 dancy Exp $

;; portmapper


(in-package :user)

(defconstant *pmapport* 111)
(defconstant *pmapprog* 100000)
(defconstant *pmapvers* 2)

(defstruct mapping
  prog
  vers
  prot
  port)

(defconstant IPPROTO_TCP 6)
(defconstant IPPROTO_UDP 17)

(defparameter *mappings* nil)
(defparameter *pmap-tcp-socket* nil)
(defparameter *pmap-udp-socket* nil)

(defparameter *portmap-debug* nil)

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
  (make-pmap-sockets)
  (portmap-add-program *pmapprog* *pmapvers* *pmapport* IPPROTO_TCP)
  (portmap-add-program *pmapprog* *pmapvers* *pmapport* IPPROTO_UDP)
  (let ((server (make-rpc-server :tcpsock *pmap-tcp-socket*
				 :udpsock *pmap-udp-socket*)))
    (loop
      (multiple-value-bind (xdr peer)
          (rpc-get-message server)
        (portmap-message-handler xdr peer)))))
      
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
	(0
	 (portmap-null peer (rpc-msg-xid msg)))
	(4
	 (portmap-dump peer (rpc-msg-xid msg)))
	(3
	 (portmap-getport peer (rpc-msg-xid msg) (call-body-params cbody)))
	(5 
	 (portmap-callit peer (rpc-msg-xid msg) (call-body-params cbody)))
	(t 
	 ;; should send a negative response
	 (format t "portmap: unhandled procedure ~D~%"
		 (call-body-proc cbody)))))))

(defun portmap-verf ()
  (let ((xdr (create-xdr :direction :build)))
    (xdr-auth-null xdr)
    xdr))

(defun portmap-null (peer xid)
  (if *portmap-debug* (format t "portmap-null~%~%"))
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (portmap-verf) xdr)))


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
    (send-successful-reply peer xid (portmap-verf) xdr)))

(defun portmap-getport (peer xid params)
  (let* ((m (with-xdr-xdr (params)
	      (make-mapping-from-xdr params)))
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
    (send-successful-reply peer xid (portmap-verf) xdr)))

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

(defun make-mapping-from-xdr (xdr)
  (let ((m (make-mapping))) 
    (setf (mapping-prog m) (xdr-int xdr))
    (setf (mapping-vers m) (xdr-int xdr))
    (setf (mapping-prot m) (xdr-int xdr))
    (setf (mapping-port m) (xdr-int xdr))
    m))
  

(defun portmap-add-program (prog vers port proto)
  (let ((mapping (make-mapping
                  :prog prog
                  :vers vers
                  :prot proto
                  :port port)))
    (unless (find mapping *mappings* :test #'equalp)
      (push mapping *mappings*))))
  
(defstruct callargs
  prog
  vers
  proc
  args)

(defstruct callresult
  port
  res)

