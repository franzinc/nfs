;; $Id: loadem.cl,v 1.10 2001/08/13 17:49:00 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (defparameter *ntservice.fasl* "ntservice/ntservice.fasl")
  (load *ntservice.fasl*))

(defparameter *filelist*
    '("util" "fixes" "mpsocketfix" "xdr" "sunrpc" "portmap"
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
			 (append '(:sock :acldns 
				   :seq2 :foreign
				   #.*ntservice.fasl*) 
				 filelist)
			 :application-files '("nfs.cfg"))))

(defun create-service (path)
  (ntservice:create-service 
   "nfs" 
   "NFS Server" 
   (format nil "~A /service" path)))

(defun delete-service ()
  (ntservice:delete-service "nfs"))
