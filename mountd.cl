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

;; mountd
;; $Id: mountd.cl,v 1.10 2001/09/04 15:50:09 dancy Exp $

(in-package :user)

(defconstant MNTPATHLEN 1024) ;; max number of bytes in a pathname argument
(defconstant MNTNAMLEN 255) ;; max number of bytes in a name argument

(defconstant *mountprog* 100005)
(defconstant *mountvers* 1)

(defparameter *mountd-tcp-socket* nil)
(defparameter *mountd-udp-socket* nil)


(defun make-mountdsockets ()
  (unless *mountd-tcp-socket*
    (setf *mountd-tcp-socket*
      (socket:make-socket :type :stream
                          :connect :passive
                          :reuse-address t)))
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


(defun mountd ()
  (make-mountdsockets)
  (portmap-add-program *mountprog* *mountvers*
		       (socket:local-port *mountd-tcp-socket*) IPPROTO_TCP)
  (portmap-add-program *mountprog* *mountvers*
		       (socket:local-port *mountd-udp-socket*) IPPROTO_UDP)
  (let ((server (make-rpc-server :tcpsock *mountd-tcp-socket*
				 :udpsock *mountd-udp-socket*)))
    (loop
      (multiple-value-bind (xdr peer)
          (rpc-get-message server)
        (mountd-message-handler xdr peer)))))


(defun mountd-message-handler (xdr peer)
  (let (msg cbody)
    (setf msg (create-rpc-msg xdr))
    (setf cbody (rpc-msg-cbody msg))
    ;;(pprint-cbody cbody)
    (unless (= (rpc-msg-mtype msg) 0)
      (error "Unexpected data!"))
    ;; sanity checks first
    (if* (not (= (call-body-prog cbody) *mountprog*))
       then
	    (format t "Sending program unavailable response for prog=~D~%"
		    (call-body-prog cbody))
	    (rpc-send-prog-unavail peer (rpc-msg-xid msg) (mountd-null-verf))
	    (return-from mountd-message-handler))
    (if* (not (= (call-body-vers cbody) *mountvers*))
       then
	    (write-line "Sending program version mismatch response")
	    (rpc-send-prog-mismatch peer (rpc-msg-xid msg)
				    (mountd-null-verf) *mountvers* *mountvers*)
	    (return-from mountd-message-handler))
    (case (call-body-proc cbody)
      (0
       (mountd-null peer (rpc-msg-xid msg)))
      (1
       (mountd-mount peer (rpc-msg-xid msg) cbody))
       ;;; right now, we don't do anything special for umounts
      (3
       (mountd-null peer (rpc-msg-xid msg)))
      (4
       (mountd-null peer (rpc-msg-xid msg)))
      (5
       (mountd-export peer (rpc-msg-xid msg)))
      (t
       ;; should send a negative response
       (format t "mountd: unhandled procedure ~D~%"
	       (call-body-proc cbody))))))

(defun mountd-null-verf ()
  (let ((xdr (create-xdr :direction :build)))
    (xdr-auth-null xdr)
    xdr))

(defun mountd-null (peer xid)
  (format t "mountd-null~%~%")
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (mountd-null-verf) xdr)))

;; returns:
;; error code
;; or
;; 0 followed by:
;;  fhandle

(defun mountd-mount (peer xid cbody)
  (format t "mountd-mount~%")
  (let ((oa (call-body-cred cbody))
        (dirpath (with-xdr-xdr ((call-body-params cbody) :name x)
		   (xdr-string x)))
        au
        rootpathname
        )
    (unless (= (opaque-auth-flavor oa) 1)
      (return-from mountd-mount
	(rpc-send-auth-error-rejected-reply peer xid 2)))
    (setf au (xdr-opaque-auth-struct-to-auth-unix-struct oa))
    ;;(format t "Trying to mount w/ credetials: ~S~%" au)
    (format t "mountd-mount ~A by ~A~%~%" dirpath (auth-unix-machinename au))
    (setf rootpathname (locate-export dirpath))
    (with-successful-reply (res peer xid (mountd-null-verf))
      (if rootpathname
	  (progn
	    (xdr-int res 0)
	    (pathname-to-fhandle-with-xdr res rootpathname))
	(xdr-int res 2))))) ;; No such file or directory

    
(defparameter *exports* nil)
  

;;; returns a pathname
(defun locate-export (dirpath)
  (let ((res (find dirpath *exports*
		   :test (lambda (x pair) (string= (car pair) x)))))
    (when res
      (pathname (second res)))))

(defun mountd-export (peer xid)
  (format t "mountd-export~%~%")
  (let ((xdr (create-xdr :direction :build)))
    (dolist (export *exports*)
      (xdr-int xdr 1) ;; indicate that data follows
      (xdr-string xdr (first export))
      (xdr-int xdr 1) ;; group follows
      (xdr-string xdr "*") ;; group name
      (xdr-int xdr 0) ;; no more groups
      )
    (xdr-int xdr 0) ;; no more data
    (send-successful-reply peer xid (mountd-null-verf) xdr)))
 
