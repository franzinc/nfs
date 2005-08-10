;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2005 Franz Inc, Oakland, CA.  All rights reserved.
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
;; $Id: main.cl,v 1.11 2005/08/10 23:42:47 dancy Exp $

(eval-when (compile eval load) (require :ntservice))

(in-package :user)

(defparameter *pmap-process* nil)
(defparameter *mountd-process* nil)
(defparameter *nfsd-process* nil)

;;#+nfs-debug (eval-when (eval load) (require :trace))

(defun startem (&rest args)
  (declare (ignore args))
  ;;#+nfs-debug (trace stat)
  (setup-logging)
  (logit "Allegro NFS Server version ~A initializing...~%" 
	 *nfsd-long-version*)
  (setf *pmap-process* (mp:process-run-function "portmapper" #'portmapper))
  (mp:process-wait "Waiting for portmapper to start" 
		   #'mp:gate-open-p *pmap-gate*)
  (setf *mountd-process* (mp:process-run-function "mountd" #'mountd))
  (mp:process-wait "Waiting for mountd to start" 
		   #'mp:gate-open-p *mountd-gate*)
  (setf *nfsd-process* (mp:process-run-function "nfsd" #'nfsd)))

(defun stopem ()
  (if *nfsd-process*
      (ignore-errors (mp:process-kill *nfsd-process*)))
  (if *mountd-process*
      (ignore-errors (mp:process-kill *mountd-process*)))
  (if *pmap-process*
      (ignore-errors (mp:process-kill *pmap-process*))))

(defun mainloop ()
  #+(version>= 7) (console-control :close :hide)
  (loop (sleep most-positive-fixnum)))      

(defun debugmain ()
  (setf *configfile* "nfs.cfg")
  (read-nfs-cfg *configfile*)
  (setf *mountd-debug* t)
  (setf *nfs-debug* t)
  (setf *portmap-debug* t)
  ;;(setf *rpc-debug* t)
  (startem))

(defun main (&rest args)
  (flet ((tnserver ()
	   #+nfs-telnet-server
	   (progn
	     (logit "Starting telnet server on port 1234~%")
	     (start-telnet-server :port 1234))))
    (let ((exepath (if (first args) (first args) "nfs.exe"))
	  (*global-gc-behavior* nil)
	  quiet)
      (setf *configfile* (merge-pathnames "nfs.cfg" exepath))
      (pop args) ;; program name

      #+nfs-demo (demoware-setup)
    
      (if (member "/quiet" args :test #'string=)
	  (setf quiet t))
      (setf args (remove "/quiet" args :test #'string=))

      (dolist (arg args)
	(cond
	 ((string= arg "/install")
	  (create-service exepath))
	 ((string= arg "/remove")
	  (delete-service))
	 ((string= arg "/start")
	  (start-service))
	 ((string= arg "/stop")
	  (stop-service))
	 ((string= arg "/service")
	  (read-nfs-cfg *configfile*)
	  (tnserver)
	  (ntservice:execute-service #'mainloop 
				     :init #'startem
				     :stop #'stopem)
	  ;; just in case
	  (exit 0))
	 (t
	  (logit "Ignoring unrecognized command line argument: ~A~%" arg))))
    
      ;; If there were any switches, exit now.
      (when args
	(exit (if quiet 0 1)))

      ;; standalone execution.
      (read-nfs-cfg *configfile*)
      (startem)
      (tnserver)
      (mainloop))))

(defun create-service (path)
  (multiple-value-bind (success code)
      (ntservice:create-service 
       "nfs" 
       "NFS Server" 
       (format nil "~A /service" path)
       :start :auto)
    (if* success
       then (logit "NFS service successfully installed.~%")
       else (logit "NFS service installation failed: ~A"
		    (ntservice:winstrerror code)))))

(defun delete-service ()
  (multiple-value-bind (success err place)
      (ntservice:delete-service "nfs")
    (if* success
       then (logit "NFS service successfully uninstalled.~%")
       else (logit "NFS service deinstallation failed.~%(~A) ~A"
		    place (ntservice:winstrerror err)))))



(defun start-service ()
  (multiple-value-bind (success err place)
      (ntservice:start-service "nfs")
    (if* success
       then (logit "NFS service started.~%")
       else (start-stop-service-err "start" err place))))

(defun stop-service ()
  (multiple-value-bind (success err place)
      (ntservice:stop-service "nfs")
    (if* success
       then (logit "NFS service stopped.~%")
       else (start-stop-service-err "stop" err place))))

(defun start-stop-service-err (type err place)
  (logit "NFS service ~a failed: " type)
  (if place
      (logit "(~A): " place))
  (logit "~A" 
	  (if (numberp err)
	      (ntservice:winstrerror err)
	    err))
  (logit "~%")
  (finish-output))
