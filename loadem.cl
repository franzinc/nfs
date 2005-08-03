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

;; $Id: loadem.cl,v 1.38 2005/08/03 20:56:34 dancy Exp $

(in-package :user)

;;;;;; NONE OF THESE SHOULD BE ON IN AN PRODUCTION BUILD
;;(pushnew :nfs-debug *features* :test #'eq)
;;(pushnew :nfs-profiling *features* :test #'eq)
;;(pushnew :nfs-telnet-server *features* :test #'eq)

(eval-when (compile load eval)
(defparameter *filelist*
    '("nfs-common" 
      "nfs-shared" 
      "sunrpc-common"
      "xdr" 
      "sunrpc" 
      "portmap" 
      "sunrpc-service"
      "ipaddr" 
      "export"
      "configure"
      "fhandle" 
      "mountd" 
      ;;"nsm"
      ;;"nlm"
      "attr" 
      "dir" 
      "openfile"
      "main" ;; needs to be before "nfs"
      "nfs"
      #+nfs-telnet-server "telnet"
      ))
)

(eval-when (compile load eval)
  (require :osi)
  (use-package :excl.osi)
  (with-compilation-unit ()
    (dolist (file *filelist*)
      (compile-file-if-needed (concatenate 'string file ".cl"))
      (load file))))

(defun buildit (&key demo)
  ;; This will be fixed before 7.0 is released.  Remove then.
  #+(version>= 7)(progn
		   (require :winapi)
		   (require :res))
  (let (filelist)
    (dolist (file (reverse *filelist*))
      (push (concatenate 'string file ".fasl") filelist))
    
    (generate-executable
     "nfs" 
     (append '(:sock :acldns :seq2 :foreign
	       #+nfs-profiling :prof
	       #+nfs-profiling :pe ;; needed for prof:show-flat-profile
	       #+nfs-debug :trace)
	     filelist)
     :runtime
     #+nfs-profiling :partners
     #-nfs-profiling :standard
     :icon-file "nfs.ico"
     :demo demo)

    ;; Set the command line flags.
    (run-shell-command
     ;; +cx hide console
     ;; +Ti remove "interrupt lisp" from system tray menu
     ;; +Cx disable console window exit.
     ;; +N sets program name used in system tray menu     
     (format nil 
	     "~a -o nfs/nfs.exe +t ~s +cx +Ti +Cx +N \"Allegro NFS\""
	     (truename "sys:bin;setcmd.exe")
	     (if demo
		 "Allegro NFS Server demo"
	       "Allegro NFS Server"))
	     
     :show-window :hide)))


