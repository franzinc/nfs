;; $Id: loadem.cl,v 1.5 2001/08/03 21:53:29 dancy Exp $

(in-package :user)

(defparameter *filelist*
    '("extra" "util" "fixes" "mpsocketfix" "xdr" 
		    "sunrpc" "portmap" "fhandle"
		    "mountd" "nfs"))

(defun loadem ()
    (dolist (file *filelist*)
      (compile-file-if-needed (concatenate 'string file ".cl"))
      (load file)))

(defun startem ()
  (mp:process-run-function "portmapper" #'portmapper)
  (mp:process-run-function "mountd" #'mountd)
  (mp:process-run-function "nfsd" #'nfsd))


(defun main (&rest args)
  (declare (ignore args))
  (startem)
  (loop (sleep most-positive-fixnum)))

(defun buildit ()
  (compile-file "loadem.cl")
  (let (filelist)
    (dolist (file (reverse (cons "loadem" *filelist*)))
      (push (concatenate 'string file ".fasl") filelist))
    (generate-executable "nfs" (append '(:sock :acldns :seq2) filelist)
			 :application-files '("nfs.cfg"))))
