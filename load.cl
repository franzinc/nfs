;; -*- mode: common-lisp -*-
;; See the file LICENSE for the full license governing this code.

(in-package :user)

;;;;;; NONE OF THESE SHOULD BE ON IN AN PRODUCTION BUILD
;;(pushnew :nfs-debug *features* :test #'eq)
;;(pushnew :nfs-profiling *features* :test #'eq)
;;(pushnew :nfs-telnet-server *features* :test #'eq)

(format t "~&~% *features*=~S~%" *features*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RPC build

(load (compile-file-if-needed "rpcgen"))

(dolist (file (list "sunrpc.x" "portmap.x" "mount.x" "nlm.x" "nsm.x"))
  (write-line file)
  (rpcgen file))

(rpcgen "nfs.x" :out-base "gen-nfs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
(defparameter *filelist*
    '("config-defs"
      #-nfs-lisp-bsw "bswap"
      "utf8"
      "utils"
      "xdr" 
      "unicode-file"
      "sunrpc-common"
      "gen-nfs-common"
      "portmap-common"
      "mount-common"
      "nsm-common"
      "nfs-common" 
      "nfs-shared" 
      "nlm-common"
      "sunrpc"
      "portmap-client"
      "sunrpc-service"
      "portmap" 
      "ipaddr" 
      "directory-tree"
      "export"
      "configure"
      "nfs-log"
      "fhandle" 
      "mountd" 
      "nsm-client"
      "nsm"
      "attr" 
      "dir" 
      "openfile"
      "interval"
      "nlm-client"
      "locking"
      "nlm"
      "gen-nfs-client"
      "main" ;; needs to be before "nfs"
      "nfs"
      #+nfs-telnet-server "telnet"
      #+nfs-demo "demoware/demoware2"
      ))
)

(eval-when (compile load eval)
  (setq excl::*warn-smp-usage* nil)
  
  (require :osi)
  (use-package :excl.osi)
  (with-compilation-unit ()
    (dolist (file *filelist*)
      (load (compile-file-if-needed file)))))

(defun buildit ()
  ;; This will be fixed before 7.0 is released.  Remove then.
  #+(version>= 7)(progn
		   (require :winapi)
		   (require :res))
  (let (filelist)
    (dolist (file (reverse *filelist*))
      (push (concatenate 'string file ".fasl") filelist))
    
    (generate-executable
     "nfs" 
     (append '(:sock :acldns :seq2 :foreign :efmacs :autozoom :disasm
	       #+nfs-profiling :prof
	       #+nfs-profiling :pe ;; needed for prof:show-flat-profile
	       #+nfs-debug :trace)
	     filelist)
     :runtime #+nfs-profiling :partners #-nfs-profiling :standard
     :runtime-bundle t
     :icon-file "nfs.ico")

    ;; Set the command line flags.
    (run-shell-command
     ;; +cx hide console
     ;; +Ti remove "interrupt lisp" from system tray menu
     ;; +Cx disable console window exit.
     ;; +N sets program name used in system tray menu     
     (format nil 
	     "~a -o nfs/nfs.exe +t ~s +cx +Ti +Cx +N \"Allegro NFS ~a\""
	     (truename "sys:bin;setcmd.exe")
	     #+nfs-demo (format nil "Allegro NFS ~a Server demo" *nfsd-version*)
	     #-nfs-demo (format nil "Allegro NFS ~a Server" *nfsd-version*)
	     *nfsd-version*)
     :show-window :hide)))


