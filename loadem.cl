;; $Id: loadem.cl,v 1.3 2001/06/07 19:09:38 dancy Exp $

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
  (mp:process-run-function "nfsd" #'nfsd))

