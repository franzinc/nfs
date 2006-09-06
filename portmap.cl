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
;; $Id: portmap.cl,v 1.33 2006/09/06 21:14:44 dancy Exp $

;; portmapper daemon and support functions

(in-package :portmap)

(sunrpc:def-rpc-program (PMAP 100000)
  (
   (2 ;; version
     (0 pmapproc-null void void)
     (1 pmapproc-set mapping bool)
     (2 pmapproc-unset mapping bool)
     (3 pmapproc-getport mapping unsigned-int)
     (4 pmapproc-dump void pmaplist)
     ;;(5 pmapproc-callit call-args call-result)
     (5 pmapproc-callit call-args :ignore)
   )
  ))

(defparameter *use-system-portmapper* :auto)
(defparameter *portmap-debug* nil)

(defparameter *pmap-gate* (mp:make-gate nil))
(defparameter *mappings* nil)

(defun PMAP-init ()
  ;; Register self.
  (setf *mappings*
    (list (make-mapping :prog #.*pmap-prog* :vers #.*pmap-vers*
			:prot #.*ipproto-udp* :port #.*pmap-port*)
	  (make-mapping :prog #.*pmap-prog* :vers #.*pmap-vers*
			:prot #.*ipproto-tcp* :port #.*pmap-port*)))
  (mp:open-gate *pmap-gate*))

(defun ping-portmapper ()
  (sunrpc:with-rpc-client (cli "127.0.0.1" #.*pmap-prog* #.*pmap-vers* :udp
			       :port #.*pmap-port*)
    (if (ignore-errors (call-pmapproc-null-2 cli nil))
	t)))

(defun portmapper ()
  (when (eq *use-system-portmapper* :auto)
    (setf *use-system-portmapper* nil)
    (when (ping-portmapper)
      (user::logit-stamp "PMAP: Using system portmapper.~%")
      (setf *use-system-portmapper* t)
      (mp:open-gate *pmap-gate*)))
  
  (when (not *use-system-portmapper*)
    (PMAP)))


;;;;;;;;; server procedures

(defmethod print-object ((obj mapping) stream)
  (format stream "[Prg: ~d, V: ~d, ~a, Port: ~d]"
	  (portmap:mapping-prog obj)
	  (portmap:mapping-vers obj)
	  (sunrpc:protocol-to-string (portmap:mapping-prot obj))
	  (portmap:mapping-port obj)))

(defun pmapproc-null (args vers peer cbody)
  (declare (ignore args vers cbody))
  (if *portmap-debug* 
      (user::logit-stamp "PMAP: ~a: NULL~%" (sunrpc:peer-dotted peer))))

(defun mapping-matches (m1 m2)
  (and (= (mapping-prog m1) (mapping-prog m2))
       (= (mapping-vers m1) (mapping-vers m2))
       (= (mapping-prot m1) (mapping-prot m2))))

;;; Ref: http://www.opengroup.org/onlinepubs/009629799/PMAPPROC_SET.htm
;;; The procedure refuses to establish a mapping if one already exists
;;; for the tuple "(prog, vers, prot)"

(defun pmapproc-set (m vers peer cbody)
  (declare (ignore vers cbody))

  (let (res)
    
    (without-interrupts
      (when (and (sunrpc:local-peer-p peer)
		 (not (find m *mappings* :test #'mapping-matches)))
	(push m *mappings*)
	(setf res t)))
    
    (if *portmap-debug*
	(user::logit-stamp "PMAP: ~a: SET ~a ==> ~a~%" (sunrpc:peer-dotted peer) m res))
    
    res))

(defun mapping-matches-noproto (m1 m2)
  (and (= (mapping-prog m1) (mapping-prog m2))
       (= (mapping-vers m1) (mapping-vers m2))))

;; Only program and vers are considered.
(defun pmapproc-unset (m vers peer cbody)
  (declare (ignore vers cbody))
  
  (let (res)
    (without-interrupts
      (when (and (sunrpc:local-peer-p peer)
		 (find m *mappings* :test #'mapping-matches-noproto))
	(setf *mappings* (delete m *mappings* :test #'mapping-matches-noproto))
	(setf res t)))
    
    (if *portmap-debug*
	(user::logit-stamp "PMAP: ~a: UNSET ~a ==> ~a~%" (sunrpc:peer-dotted peer) m res))
    
    res))

(defun pmapproc-getport (m vers peer cbody)
  (declare (ignore vers cbody))
  
  (let ((res 0))
    (without-interrupts
      (let ((entry (find m *mappings* :test #'mapping-matches)))
	(if entry
	    (setf res (mapping-port entry)))))
  
    (if *portmap-debug*
	(user::logit-stamp "PMAP: ~a: GETPORT ~a ==> ~a~%" (sunrpc:peer-dotted peer) m res))
    
    res))

(defun pmapproc-dump (arg vers peer cbody)
  (declare (ignore arg vers cbody))
  
  (let (head)
    (without-interrupts
      (dolist (m *mappings*)
	(setf head (make-pmapentry :map m :next head))))

    (if *portmap-debug*
	(user::logit-stamp "PMAP: ~a: DUMP~%" (sunrpc:peer-dotted peer)))
    
    head))

;; Silently ignore
(defun pmapproc-callit (args vers peer cbody)
  (declare (ignore args vers peer cbody)))

;;;;;;;;;;;

(eval-when (compile load eval)
  (export '(*portmap-debug* *pmap-gate* *use-system-portmapper*
	    portmapper ping-portmapper)))
    

