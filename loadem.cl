;; $Id: loadem.cl,v 1.2 2001/05/23 15:59:02 layer Exp $

(in-package :user)

(defun loadem ()
    (dolist (file '("extra" "util" "mpsocketfix" "xdr" 
		    "sunrpc" "portmap" "fhandle"
		    "mountd" "nfs"))
      (compile-file-if-needed (concatenate 'string file ".cl"))
      (load file)))

(defun startem ()
  (mp:process-run-function "portmapper" #'portmapper)
  (mp:process-run-function "mountd" #'mountd)
  ;;(mp:process-run-function "nfsd" #'nfsd))
  (nfsd))
