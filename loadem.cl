;; $Id: loadem.cl,v 1.7 2001/08/11 00:27:42 layer Exp $

(in-package :user)

(eval-when (compile load eval)
  (load "ntservice/ntservice.fasl"))

(defparameter *filelist*
    '("extra" "util" "fixes" "mpsocketfix" "xdr" "sunrpc" "portmap"
      "fhandle" "mountd" "nfs"))

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
  (let ((configfile (merge-pathnames "nfs.cfg" (pop args))))
    (read-nfs-cfg configfile)
    (if* (and args (string= (first args) "/service"))
       then
	    (ntservice:start-service #'mainloop :init #'startem)
       else
	    (startem)
	    (mainloop))))

(defun read-nfs-cfg (configfile)
  (with-open-file (s configfile)
    (dolist (pair (read s))
      (set (first pair) (second pair)))))

(defun buildit ()
  (compile-file "loadem.cl")
  (loadem) 
  (let (filelist)
    (dolist (file (reverse (cons "loadem" *filelist*)))
      (push (concatenate 'string file ".fasl") filelist))
    (generate-executable "nfs" 
			 (append '(:sock :acldns :seq2 "c:/devel/service/service.fasl") 
				 filelist)
			 :application-files '("nfs.cfg"))))

(defun create-service ()
  (ntservice:create-service "nfs" "NFS Server" "c:\\devel\\nfs\\nfs\\nfs.exe /service"))
