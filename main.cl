(in-package :user)

(defparameter *pmap-process* nil)
(defparameter *mountd-process* nil)
(defparameter *nfsd-process* nil)

(defun startem (&rest args)
  (declare (ignore args))
  (setf *pmap-process* (mp:process-run-function "portmapper" #'portmapper))
  (mp:process-wait "Waiting for portmapper to start" 
		   #'mp:gate-open-p *pmap-gate*)
  (setf *mountd-process* (mp:process-run-function "mountd" #'mountd))
  (setf *nfsd-process* (mp:process-run-function "nfsd" #'nfsd)))

(defun stopem ()
  (if *nfsd-process*
      (ignore-errors (mp:process-kill *nfsd-process*)))
  (if *mountd-process*
      (ignore-errors (mp:process-kill *mountd-process*)))
  (if *pmap-process*
      (ignore-errors (mp:process-kill *pmap-process*))))

(defun mainloop ()
  #+(version>= 7) (console-control :close :hide)
  (loop (sleep most-positive-fixnum)))      

(defun main (&rest args)
  (let ((exepath (if (first args) (first args) "nfs.exe"))
	(*global-gc-behavior* nil)
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
	(read-nfs-cfg *configfile*)
	(ntservice:execute-service #'mainloop 
				   :init #'startem
				   :stop #'stopem)
	;; just in case
	(exit 0))
       (t
	(format t "Ignoring unrecognized command line argument: ~A~%" arg))))
    
    ;; If there were any switches, exit now.
    (when args
      (exit (if quiet 0 1)))

    ;; standalone execution.
    (read-nfs-cfg *configfile*)
    (startem)
    (mainloop)))

(defun create-service (path)
  (multiple-value-bind (success code)
      (ntservice:create-service 
       "nfs" 
       "NFS Server" 
       (format nil "~A /service" path)
       :start :auto)
    (if* success
       then (format t "NFS service successfully installed.~%")
       else (format t "NFS service installation failed: ~A"
		    (ntservice:winstrerror code)))))

(defun delete-service ()
  (multiple-value-bind (success err place)
      (ntservice:delete-service "nfs")
    (if* success
       then
	    (format t "NFS service successfully uninstalled.~%")
       else
	    (format t "NFS service deinstallation failed.~%(~A) ~A"
		    place (ntservice:winstrerror err)))))



(defun start-service ()
  (multiple-value-bind (success err place)
      (ntservice:start-service "nfs")
    (if* success
       then
	    (format t "NFS service started.~%")
       else
	    (start-stop-service-err "start" err place))))

(defun stop-service ()
  (multiple-value-bind (success err place)
      (ntservice:stop-service "nfs")
    (if* success
       then
	    (format t "NFS service stopped.~%")
       else
	    (start-stop-service-err "stop" err place))))

(defun start-stop-service-err (type err place)
  (format t "NFS service ~a failed: " type)
  (if place
      (format t "(~A): " place))
  (format t "~A" 
	  (if (numberp err)
	      (ntservice:winstrerror err)
	    err))
  (format t "~%")
  (finish-output))
  


