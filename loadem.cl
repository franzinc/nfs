;; $Id: loadem.cl,v 1.4 2001/06/14 21:55:45 dancy Exp $

(in-package :user)

(defun loadem ()
    (dolist (file '("extra" "util" "fixes" "mpsocketfix" "xdr" 
		    "sunrpc" "portmap" "fhandle"
		    "mountd" "nfs"))
      (compile-file-if-needed (concatenate 'string file ".cl"))
      (load file)))

(defun startem ()
  (mp:process-run-function "portmapper" #'portmapper)
  (mp:process-run-function "mountd" #'mountd)
  (mp:process-run-function "nfsd" #'nfsd))

