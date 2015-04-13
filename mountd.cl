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

(in-package :mount)

(sunrpc:def-rpc-program (MNT 100005 :port *mountd-port-number*)
    (
     (1 ;; version
      (0 mountproc-null void void)
      (1 mountproc-mnt dirpath fhstatus)
      (2 mountproc-dump void mountlist)
      (3 mountproc-umnt dirpath void)
      (4 mountproc-umntall void void)
      (5 mountproc-export void exports)
      (6 mountproc-exportall void exports)
      )
     (2 ;; version
      (0 mountproc-null void void)
      (1 mountproc-mnt dirpath fhstatus)
      (2 mountproc-dump void mountlist)
      (3 mountproc-umnt dirpath void)
      (4 mountproc-umntall void void)
      (5 mountproc-export void exports)
      (6 mountproc-exportall void exports)
      (7 mountproc-pathconf dirpath ppathcnf)
      )
     (3 ;; version
      (0 mountproc3-null void void)
      (1 mountproc3-mnt dirpath mountres3)
      (2 mountproc3-dump void mountlist)
      (3 mountproc3-umnt dirpath void)
      (4 mountproc3-umntall void void)
      (5 mountproc3-export void exports)
      )
     ))


(defparameter *mountd-gate* (mp:make-gate nil))
(defparameter *mounts* nil)

(defun MNT-init ()
  (mp:open-gate *mountd-gate*))

;;; Override the automatically generated xdr-fhandle* functions.
(without-redefinition-warnings 
 (defun xdr-fhandle (xdr &optional arg)
   (user::xdr-fhandle xdr 2 arg))
 
 (defun xdr-fhandle3 (xdr &optional arg)
   (user::xdr-fhandle xdr 3 arg)))

;;;; Procedures

(defun mountproc-null (arg vers peer cbody)
  (declare (ignore arg cbody))
  (if *mountd-debug* 
      (user::logit-stamp "MNT~a: ~a: NULL~%" vers (sunrpc:peer-dotted peer))))

(defun mountproc3-null (arg vers peer cbody)
  (mountproc-null arg vers peer cbody))

(defun mountproc-mnt-common (dirpath vers peer)
  (multiple-value-bind (exp tail) 
      (user::locate-nearest-export-by-nfs-path dirpath)
    (if *mountd-debug* 
	(user::logit-stamp "MNT~d: ~a: MOUNT ~a "
			   vers (sunrpc:peer-dotted peer) dirpath))
    (if* (null exp)
       then (if *mountd-debug* (user::logit "==> Denied (no such export).~%"))
	    gen-nfs:*nfserr-noent*
     elseif (not (user::export-host-access-allowed-p 
		  exp (sunrpc:rpc-peer-addr peer)))
       then (if *mountd-debug* 
		(user::logit "==> Denied (host not allowed).~%"))
	    gen-nfs:*nfserr-acces*
       else (let ((fh (user::get-fhandle-for-path tail exp)))
	      (if* fh
		 then (if *mountd-debug* (user::logit "==> Accepted.~%"))
		      (pushnew (list (sunrpc:rpc-peer-addr peer) dirpath) 
			       *mounts* 
			       :test #'equalp)
		      fh
		 else (if *mountd-debug* (user::logit "==> Not found.~%"))
		      gen-nfs:*nfserr-noent*)))))

(defun mountproc-mnt (dirpath vers peer cbody)
  (declare (ignore cbody))
  (let ((fh (mountproc-mnt-common dirpath vers peer)))
    (if* (numberp fh)
       then (make-fhstatus :fhs-status fh) ;; error code
       else (make-fhstatus :fhs-status 0 :fhs-fhandle fh))))

(defun mountproc3-mnt (dirpath vers peer cbody)
  (declare (ignore cbody))
  (let ((fh (mountproc-mnt-common dirpath vers peer)))
    (if* (numberp fh)
       then (make-mountres3 :fhs-status fh) ;; error code
       else (make-mountres3 :fhs-status *mnt3-ok* 
			    :mountinfo 
			    (make-mountres3-ok :fhandle fh
					       :auth-flavors 
					       (list sunrpc:*auth-unix*))))))

(defun mountproc-dump (arg vers peer cbody)
  (declare (ignore arg cbody))
  (if *mountd-debug* 
      (user::logit-stamp "MNT~d: ~a: DUMP~%" vers (sunrpc:peer-dotted peer)))
  (let (res)
    (dolist (pair *mounts*)
      (setf res 
	(make-mountbody :ml-hostname (socket:ipaddr-to-dotted (first pair))
			:ml-directory (second pair)
			:ml-next res)))
    res))

(defun mountproc3-dump (arg vers peer cbody)
  (mountproc-dump arg vers peer cbody))

(defun mountproc-umnt (dirpath vers peer cbody)
  (declare (ignore cbody))
  (if *mountd-debug* 
      (user::logit-stamp "MNT~d: ~a: UMOUNT ~a~%" vers (sunrpc:peer-dotted peer) dirpath))
  (setf *mounts* 
    (delete (list (sunrpc:rpc-peer-addr peer) dirpath) 
	    *mounts*
	    :test #'equalp)))

(defun mountproc3-umnt (dirpath vers peer cbody)
  (mountproc-umnt dirpath vers peer cbody))

(defun mountproc-umntall (arg vers peer cbody)
  (declare (ignore arg cbody))
  (if *mountd-debug* 
      (user::logit-stamp "MNT~d: ~a: UMOUNT ALL~%" vers (sunrpc:peer-dotted peer)))
  (setf *mounts* 
    (delete (sunrpc:rpc-peer-addr peer) *mounts* :key #'first)))

(defun mountproc3-umntall (arg vers peer cbody)
  (mountproc-umntall arg vers peer cbody))

(defun mountproc-export (arg vers peer cbody)
  (declare (ignore arg cbody))
  (when mount:*showmount-disabled*
    (when *mountd-debug* 
      (user::logit-stamp "MNT~d: ~A: EXPORT is disabled via config.~%" vers (sunrpc:peer-dotted peer)))
    (return-from mountproc-export))
  (if *mountd-debug* 
      (user::logit-stamp "MNT~d: ~a: EXPORT~%" vers (sunrpc:peer-dotted peer))) 
  (let (res)
    (dotimes (n (length user::*exports*))
      (let ((export (svref user::*exports* n)))
	(setf res  
	      (make-exportnode 
	       :ex-dir (user::nfs-export-name export)
	       :ex-groups 
	       (let ((groups (reverse (user::nfs-export-hosts-allow export)))
		     grp)
		 (dotimes (g (length groups))
		   (setf grp
			 (make-groupnode
			  :gr-name 
			  (user::network-address-to-printable-string 
                            (elt groups g))
			  :gr-next grp)))
		 grp)
	       :ex-next res))))
    res))

(defun mountproc3-export (arg vers peer cbody)
  (mountproc-export arg vers peer cbody))

(defun mountproc-exportall (arg vers peer cbody)
  (mountproc-export arg vers peer cbody))

(defconstant *pc-error* 0)
(defconstant *pc-link-max* 1)
(defconstant *pc-max-canon* 2)
(defconstant *pc-max-input* 3)
(defconstant *pc-name-max* 4)
(defconstant *pc-path-max* 5)
(defconstant *pc-pipe-buf* 6)
(defconstant *pc-no-trunc* 7)
(defconstant *pc-vdisable* 8)
(defconstant *pc-chown-restricted* 9)

(defun mountproc-pathconf (dirpath vers peer cbody)
  (declare (ignore cbody))
  (if* *mountd-debug*
     then (user::logit-stamp "MNT~d: ~a: PATHCONF ~a~%" vers (sunrpc:peer-dotted peer) dirpath))
  
  ;; Return information in the same way that Solaris 9 does
  (make-ppathcnf :pc-link-max 1023 ;; NTFS limit
		 :pc-max-canon 0 :pc-max-input 0
		 :pc-name-max 255 :pc-path-max 255
		 :pc-pipe-buf 0 :pc-vdisable 0 :pc-xxx 0
		 :pc-mask (list 
			   (logior 
			    ;; Indicate which fields are invalid
			    (ash 1 *pc-max-canon*)
			    (ash 1 *pc-max-input*)
			    (ash 1 *pc-pipe-buf*)
			    (ash 1 *pc-vdisable*)
			    ;; And indicate behavior
			    (ash 1 *pc-no-trunc*)
			    (ash 1 *pc-chown-restricted*))
			   0)))

;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
  (export '(*mountd-debug* *mountd-port-number* *mountd-gate*
	    MNT)))
	    
