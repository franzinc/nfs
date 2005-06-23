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
;; $Id: mountd.cl,v 1.20 2005/06/23 20:59:42 dancy Exp $

(in-package :user)

(defconstant *mountprog* 100005)

(defparameter *mountd-tcp-socket* nil)
(defparameter *mountd-udp-socket* nil)

(defparameter *mountd-debug* nil)

(defun make-mountdsockets ()
  (unless *mountd-tcp-socket*
    (setf *mountd-tcp-socket*
      (socket:make-socket :type :hiper
                          :connect :passive)))
  (unless *mountd-udp-socket*
    (setf *mountd-udp-socket*
      (socket:make-socket :type :datagram))))

(defun close-mountdsockets ()
  (when *mountd-tcp-socket*
    (close *mountd-tcp-socket*)
    (setf *mountd-tcp-socket* nil))
  (when *mountd-udp-socket*
    (close *mountd-udp-socket*)
    (setf *mountd-udp-socket* nil)))

(defmacro with-mountd-sockets (() &body body)
  `(unwind-protect
       (progn
	 (make-mountdsockets)
	 ,@body)
     (close-mountdsockets)))

(defun mountd ()
  (with-mountd-sockets ()
    (with-portmapper-mapping (*mountprog* 
			      '(1 2 3)
			      (socket:local-port *mountd-tcp-socket*) 
			      IPPROTO_TCP)
      (with-portmapper-mapping (*mountprog* 
				'(1 2 3)
				(socket:local-port *mountd-udp-socket*) 
				IPPROTO_UDP)
	(let* ((buffer (make-array (* 64 1024) 
				   :element-type '(unsigned-byte 8)))
	       (server (make-rpc-server :tcpsock *mountd-tcp-socket*
					:udpsock *mountd-udp-socket*
					:buffer buffer)))
	  (declare (dynamic-extent buffer server))
	  (loop
	    (multiple-value-bind (xdr peer)
		(rpc-get-message server)
	      (mountd-message-handler xdr peer))))))))


(defun mountd-message-handler (xdr peer)
  (block nil
    (let (msg cbody)
      (setf msg (create-rpc-msg xdr))
      (setf cbody (rpc-msg-cbody msg))
      
      ;; sanity checks first
      (when (null cbody)
	(logit "Invalid message from ~A~%" peer)
	(return))
      
      (when (/= (call-body-prog cbody) *mountprog*)
	(logit "Mountd: Sending program unavailable response for prog=~D to ~A~%"
	       (call-body-prog cbody)
	       (socket:ipaddr-to-dotted (rpc-peer-addr peer)))
	(rpc-send-prog-unavail peer (rpc-msg-xid msg) (null-verf))
	(return))

      (unless (<= 1 (call-body-vers cbody) 3)
	(logit "Mountd: Sending program version mismatch response (requested version was ~D) to ~A~%" 
	       (call-body-vers cbody)
	       (socket:ipaddr-to-dotted (rpc-peer-addr peer)))
	(rpc-send-prog-mismatch peer (rpc-msg-xid msg) (null-verf) 1 3)
	(return))
      
      (case (call-body-proc cbody)
	(0
	 (mountd-null peer (rpc-msg-xid msg)))
	(1
	 (mountd-mount peer (rpc-msg-xid msg) cbody))
	(2
	 (mountd-dump peer (rpc-msg-xid msg)))
	(3
	 (mountd-umount peer (rpc-msg-xid msg) cbody))
	(4 
	 (mountd-umntall peer (rpc-msg-xid msg)))
	(5
	 (mountd-export peer (rpc-msg-xid msg)))
	(t
	 ;; should send a negative response
	 (logit "mountd: unhandled procedure ~D requested by ~A~%"
		(call-body-proc cbody)
		(socket:ipaddr-to-dotted (rpc-peer-addr peer))))))))

(defun mountd-null (peer xid)
  (if *mountd-debug* (logit "mountd-null~%~%"))
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (null-verf) xdr)))

(defparameter *mounts* nil)

;; returns:
;; error code
;; or
;; 0 followed by:
;;  fhandle
;; (nfsv3) plus a variable size listed of ints indicating the 
;;         acceptable auth flavors.  We only accept the unix
;;         auth flavor.

(defun mountd-mount (peer xid cbody)
  (block nil
    (with-xdr-xdr ((call-body-params cbody) :name params)
      (let* ((vers (call-body-vers cbody))
	     (dirpath (xdr-string params))
	     (exp (locate-export dirpath)))
	(if *mountd-debug* 
	    (logit "Mountd(v~D): ~A requests mount of ~A.~%"
		   vers
		   (socket:ipaddr-to-dotted (rpc-peer-addr peer))
		   dirpath))
	(with-successful-reply (res peer xid (null-verf))
	  (cond 
	   ((null exp)
	    (if *mountd-debug*
		(logit "Mount request denied (no such export).~%"))
	    (xdr-int res NFSERR_NOENT))
	   ((not (export-host-access-allowed-p exp (rpc-peer-addr peer)))
	    (if *mountd-debug* 
		(logit "Mount request denied (host not allowed).~%"))
	    (xdr-int res NFSERR_ACCES))
	   (t
	    (if *mountd-debug*
		(logit "Mount request accepted.~%"))
	    (pushnew (list (rpc-peer-addr peer) dirpath) *mounts* 
		     :test #'equalp)
	    (xdr-int res NFS_OK)
	    (if (= vers 1) ;; mountd v1 handles nfs v2
		(setf vers 2))
	    (xdr-fhandle res vers (get-export-fhandle exp))
	    (when (= vers 3)
	      (xdr-int res 1) ;; one entry
	      (xdr-int res *AUTH_UNIX*)))))))))

(defun mountd-dump (peer xid)
  (if *mountd-debug* 
      (logit "Mountd: ~A mountd-dump.~%"
	     (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
  (with-successful-reply (res peer xid (null-verf))
    (dolist (entry *mounts*)
      (xdr-int res 1) ;; data follows
      (xdr-string res (socket:ipaddr-to-dotted (first entry)))
      (xdr-string res (second entry)))
    (xdr-int res 0))) ;; no more data

(defun mountd-umount (peer xid cbody)
  (let ((dirpath (with-xdr-xdr ((call-body-params cbody) :name x)
		   (xdr-string x)))
	(xdr (create-xdr :direction :build)))
    (if *mountd-debug* 
	(logit "Mountd: ~A requests unmount of ~A.~%"
	       (socket:ipaddr-to-dotted (rpc-peer-addr peer))
	       dirpath))
    (setf *mounts* 
      (remove (list (rpc-peer-addr peer) dirpath) 
	      *mounts*
	      :test #'equalp))
    (send-successful-reply peer xid (null-verf) xdr)))

(defun mountd-umntall (peer xid)
  (let ((xdr (create-xdr :direction :build)))
    (if *mountd-debug* 
	(logit "Mountd: ~A: umntall~%"
	       (socket:ipaddr-to-dotted (rpc-peer-addr peer))))
    (setf *mounts* 
      (remove (rpc-peer-addr peer) *mounts* :key #'first))
    (send-successful-reply peer xid (null-verf) xdr)))

(defun mountd-export (peer xid)
  (if *mountd-debug* (logit "mountd-export~%~%"))
  (let ((xdr (create-xdr :direction :build)))
    (dotimes (n (length *exports*))
      (xdr-int xdr 1) ;; indicate that data follows
      (xdr-string xdr (nfs-export-name (svref *exports* n)))
      (xdr-int xdr 0)) ;; no group information
    (xdr-int xdr 0) ;; no more exports
    (send-successful-reply peer xid (null-verf) xdr)))

;; debugging/informational.  No callers.
(defun showmounts ()
  (dolist (mnt *mounts*)
    (logit "~A -> ~A~%"
	   (socket:ipaddr-to-dotted (first mnt))
	   (second mnt))))
