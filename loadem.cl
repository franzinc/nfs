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

;; $Id: loadem.cl,v 1.16 2003/01/20 23:47:25 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (defparameter *ntservice.fasl* "ntservice/ntservice.fasl")
  (load *ntservice.fasl*))

(defparameter *filelist*
    '("util" "fixes" "mpsocketfix" "xdr" "sunrpc" 
      "ipaddr" "access" "portmap" "fhandle" "mountd" "nfs"))

(defun loadem ()
  (dolist (file *filelist*)
    (compile-file-if-needed (concatenate 'string file ".cl"))
    (load file)))

(defun startem (&rest args)
  (declare (ignore args))
  (mp:process-run-function "portmapper" #'portmapper)
  (mp:process-run-function "mountd" #'mountd)
  (mp:process-run-function "nfsd" #'nfsd))

(defun mainloop ()
  (loop (sleep most-positive-fixnum)))      

(defun main (&rest args)
  (let* ((exepath (pop args))
	 (configfile (merge-pathnames "nfs.cfg" exepath)))
    (if (and args (string= (first args) "/install"))
	(create-service exepath))
    (if (and args (string= (first args) "/remove"))
	(delete-service))
    (read-nfs-cfg configfile)
    (if* (and args (string= (first args) "/service"))
       then (ntservice:start-service #'mainloop :init #'startem)
       else (startem)
	    (mainloop))))

(defun read-nfs-cfg (configfile)
  (declare (special *hosts-allow* *hosts-allow-parsed* 
		    *hosts-deny* *hosts-deny-parsed*))
  (with-open-file (s configfile)
    (dolist (pair (read s))
      (set (first pair) (second pair))))
  (setf *hosts-allow-parsed* (mapcar #'parse-addr *hosts-allow*))
  (setf *hosts-deny-parsed* (mapcar #'parse-addr *hosts-deny*)))
  

(defun buildit ()
  (compile-file "loadem.cl")
  (loadem) 
  (let (filelist)
    (dolist (file (reverse (cons "loadem" *filelist*)))
      (push (concatenate 'string file ".fasl") filelist))
    (generate-executable
     "nfs" 
     (append '(:sock :acldns :seq2 :foreign #.*ntservice.fasl*)
	     filelist)
     #+(version>= 6 2 :pre-beta 13) :icon-file
     #+(version>= 6 2 :pre-beta 13) "nfs.ico")
    (run-shell-command
     (format nil "~a -o nfs/nfs.exe +t ~s ~a"
	     (truename "sys:bin;setcmd.exe")
	     "Allegro NFSd"
	     ;; In ACL 6.2, the "show the icon in the tray" bug has been
	     ;; fixed, so don't show the console by default.  Before 6.2,
	     ;; show it minimized.
	     #+(version>= 6 2 :pre-beta 13) "+cx"
	     #-(version>= 6 2 :pre-beta 13) "+cm")
     :show-window :hide)))

(defun create-service (path)
  (multiple-value-bind (success code)
      (ntservice:create-service 
       "nfs" 
       "NFS Server" 
       (format nil "~A /service" path))
    (if* success
       then
	    (format t "NFS service successfully installed.~%")
       else
	    (format t "NFS service installation failed: ~A"
		    (ntservice:winstrerror code))))
  (exit 1)) ;; so the user will see the message

    

(defun delete-service ()
  (multiple-value-bind (success err place)
      (ntservice:delete-service "nfs")
    (if* success
       then
	    (format t "NFS service successfully uninstalled.~%")
       else
	    (format t "NFS service deinstallation failed.~%(~A) ~A"
		    place (ntservice:winstrerror err))))
  (exit 1)) ;; so the user will see the message

