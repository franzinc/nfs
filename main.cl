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

(eval-when (compile eval load) (require :ntservice))

(in-package :user)

(defparameter *pmap-process* nil)
(defparameter *mountd-process* nil)
(defparameter *nfsd-process* nil)
(defparameter *nsm-process* nil)
(defparameter *nlm-process* nil)

#+nfs-telnet-server ;; because the first thing you want to do is `trace'
(eval-when (eval load) (require :trace))

(defun ping-nfsd ()
  (multiple-value-bind (res error)
      (ignore-errors 
       (sunrpc:with-rpc-client (cli "127.0.0.1" gen-nfs:*nfs-program* 2 :udp)
	 (gen-nfs:call-nfsproc-null-2 cli nil)))
    (declare (ignore res))
    (if (not error)
	t)))

(defun check-nfs-already-running ()
  (if (ping-nfsd)
      (bailout "~
An NFS server is already running on this machine.  Aborting.~%")))

(defun start-subprocess (name function &key start-gate)
  "Returns the subprocess object"
  (flet ((subprocess-wrapper ()
	   (handler-bind 
	       ((error #'(lambda (c)
			   (logit-stamp "~%Unhandled condition in thread ~a: ~a~%" 
					(mp:process-name mp:*current-process*)
					c)
			   (logit-stamp "Backtrace:~%~a~%"
					(with-output-to-string (s)
					  (top-level.debug:zoom s :count t :length 10))))))
	     (funcall function))))
    
    (let ((proc (mp:process-run-function name #'subprocess-wrapper)))
      (when start-gate
	(mp:process-wait (format nil "Waiting for ~a to start" name)
			 #'mp:gate-open-p start-gate))
      
      proc)))

(defun announce (state)
  (logit-stamp #+nfs-demo "Allegro NFS Server TRIAL version ~a ~a.~%" 
	       #-nfs-demo "Allegro NFS Server version ~a ~a.~%" 
	       *nfsd-long-version*
	       state))

(defun startem (&rest args)
  (declare (ignore args)
	   (special nlm:*nlm-gate*))
  ;;#+nfs-debug (trace stat)
  (setup-logging)
  (announce "initializing")
  (logit-stamp "commit id: ~a~%" *nfsd-commit-id*)
  (logit-stamp "Built with Allegro CL ~a~%" (lisp-implementation-version))
  (check-nfs-already-running)

  (setf *pmap-process* 
    (start-subprocess "portmapper" #'portmap:portmapper :start-gate portmap:*pmap-gate*))

  (setf *mountd-process* 
    (start-subprocess "mountd" #'mount:MNT :start-gate mount:*mountd-gate*))
  
  (setf *nsm-process* 
    (start-subprocess "nsm" #'nsm:NSM :start-gate nsm:*nsm-gate*))

  (setf *nlm-process* 
    (start-subprocess "nlm" #'nlm:NLM :start-gate nlm:*nlm-gate*))

  (setf *nfsd-process* 
    (start-subprocess "nfsd" #'nfsd)))

(defvar *shutting-down* (mp:make-gate nil))

(defun stopem ()
  (logit-stamp "Stopping NFS server...")
  (when *nlm-process* (ignore-errors (mp:process-kill *nlm-process*)))
  (when *nsm-process* (ignore-errors (mp:process-kill *nsm-process*)))
  (when *nfsd-process* (ignore-errors (mp:process-kill *nfsd-process*)))
  (when *mountd-process* (ignore-errors (mp:process-kill *mountd-process*)))
  (when *pmap-process* (ignore-errors (mp:process-kill *pmap-process*)))
  
  (flet ((kill-by-name (name)
	   (let ((proc (find name sys:*all-processes* :key #'mp:process-name :test #'string=)))
	     (when proc
	       (ignore-errors (mp:process-kill proc))))))
    (kill-by-name "open file reaper")
    (kill-by-name "attr cache reaper")
    (kill-by-name "nsm callback retry loop")
    (kill-by-name "nlm retry loop")
    (kill-by-name "nlm notify loop")
    )
	     
  (mp:open-gate *shutting-down*)
  ;; Allow `mainloop' process to see the open gate.
  (sleep 1))

(defun mainloop ()
  (console-control :close :hide)
  (mp:process-wait "waiting for shutdown"
		   #'mp:gate-open-p *shutting-down*)
  (logit-stamp "done."))

(defun debugmain (&optional (config "nfs.cfg"))
  (setf *configfile* config)
  (setf *exit-on-bailout* nil)
  (read-nfs-cfg *configfile*)
  (setf mount:*mountd-debug* t)
  (setf *nfs-debug* t)
  (setf portmap:*portmap-debug* t)
  ;;(setf *rpc-debug* t)
  (setf nsm:*nsm-debug* t)
  (setf nlm:*nlm-debug* t)
  (startem))

(defvar *service-name* "nfs")

(defun main (&rest args)
  (setf *global-gc-behavior* :auto)
  
  (flet ((tnserver ()
	   #+nfs-telnet-server
	   (progn
	     (logit-stamp "Starting telnet server on port 1234~%")
	     (start-telnet-server :port 1234))))
    (let ((exepath (if (first args) (first args) "nfs.exe"))
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
	  (setf *program-mode* :service)	  
	  (read-nfs-cfg *configfile*)
	  (tnserver)
	  (ntservice:execute-service *service-name*
				     #'mainloop 
				     :init #'startem
				     :stop #'stopem)
	  ;; just in case
	  (exit 0))
	 ((string= arg "/console")
	  (console quiet))
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
       *service-name* 
       "Allegro NFS Server" 
       (format nil "~A /service" path)
       :description "Allows NFS clients to access exported directories on this computer"
       :start :auto
       :interact-with-desktop nil)
    (if* success
       then (format t "NFS service successfully installed.~%")
       else (format t "NFS service installation failed: ~A"
		    (ntservice:winstrerror code)))))

(defun delete-service ()
  (multiple-value-bind (success err place)
      (ntservice:delete-service *service-name*)
    (if* success
       then (format t "NFS service successfully uninstalled.~%")
       else (format t "NFS service deinstallation failed.~%(~A) ~A"
		    place (ntservice:winstrerror err)))))



(defun start-service ()
  (multiple-value-bind (success err place)
      (ntservice:start-service *service-name*)
    (if* success
       then (format t "NFS service started.~%")
       else (start-stop-service-err "start" err place))))

(defun stop-service ()
  (multiple-value-bind (success err place)
      (ntservice:stop-service *service-name*)
    (if* success
       then (format t "NFS service stopped.~%")
       else (start-stop-service-err "stop" err place))))

(defun start-stop-service-err (op err place)
  (format t "Failed to ~a NFS service: ~@[(~a): ~]~a~%" 
	  op place (if* (numberp err)
		      then (ntservice:winstrerror err)
		      else err))
  (finish-output))


;;; XXXX FIXME Temporary until building on 8.1
;;; use console-control :title then.
(eval-when (compile load eval)
  (require :winapi))

(defun get-console-hwnd ()
  (let ((where (ff:allocate-fobject '(:array :nat 4) :foreign-static-gc)))
    (win:GetWinMainArgs where)
    (ff:fslot-value where 3)))

(defun set-window-title (title)
  (with-native-string (title title)
    (win:SetWindowText (get-console-hwnd) title)))
