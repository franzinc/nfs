;; $Id: nfs-log.cl,v 1.3 2008/01/04 19:13:36 dancy Exp $

;; Logging support

(in-package :user)

(defvar *log-stream* nil)
(defvar *program-mode* nil) ;; nil or :service
(defvar *console-sockets* nil)
(defvar *console-sockets-lock* (mp:make-process-lock))

(defun logit-1 (string)
  (when *log-stream*
    (write-string string *log-stream*)
    (force-output *log-stream*))
  (when (eq *program-mode* :service)
    (mp:with-process-lock (*console-sockets-lock*)
      (let (ok)
	(dolist (sock *console-sockets*)
	  (if* (null (ignore-errors 
		      (progn
			(write-string string sock)
			(finish-output sock)
			(setf ok t))))
	     then ;; Communication w/ the console socket failed.
		  (close sock :abort t)
		  (setf *console-sockets*
		    (delete sock *console-sockets*))))
	(if (not ok)
	    (log-buffer-add string))))))

(defun logit (format-string &rest format-args)
  (logit-1 (apply #'format nil format-string format-args)))

(defun logit-stamp (format-string &rest format-args)
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (logit "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d| " 
	   year month day hour min sec)
    (apply #'logit format-string format-args)))

(defvar *nfs-debug-stream* nil)

(eval-when (compile eval load)
  (require :streamc) ;; for make-broadcast-stream
  )

(defvar *log-file* "sys:nfsdebug.txt")

(defun setup-logging (&optional reopen)
  (when reopen
    (when *nfs-debug-stream* (close *nfs-debug-stream*))
    (setf *log-stream* nil))
  
  (when (null *log-stream*)
    (setf *nfs-debug-stream*
      (open *log-file* 
	    :direction :output 
	    :external-format :utf8
	    :if-exists :append
	    :if-does-not-exist :create))

    (if* (eq *program-mode* :service)
       then (setf *log-stream* *nfs-debug-stream*)
	    (mp:process-run-function "Console server" #'console-server)
       else (setf *log-stream*(make-broadcast-stream *initial-terminal-io* 
						     *nfs-debug-stream*)))
    (logit-stamp "Log file: ~a~%"
		 (translate-logical-pathname (pathname *log-file*)))))

(eval-when (compile load eval)
  (defconstant *nfs-log-limit* 32768)) ;; # of strings.  Use a power of 2

(defparameter *log-buffer* (make-array *nfs-log-limit*))
(defvar *log-buffer-lock* (mp:make-process-lock))

(defparameter *log-buffer-start* 0) ;; where next retrieve will occur
(defparameter *log-buffer-end* 0) ;; where next store will occur

(defun log-buffer-add (string)
  (declare (optimize (speed 3)))
  (macrolet ((next-pos (val)
	       `(logand (the fixnum (1+ ,val)) (1- *nfs-log-limit*))))
    (mp:with-process-lock (*log-buffer-lock*)
      (let* ((start *log-buffer-start*)
	     (end *log-buffer-end*)
	     (newend (next-pos end)))
	(declare (fixnum start end newend))
      
	(setf *log-buffer-end* newend)
	(setf (aref (the (simple-array t (*)) *log-buffer*) end) string)
      
	(if (eq newend start)
	    (setf *log-buffer-start* (next-pos start)))))))

(defvar *console-listener* nil)

(defun console-server ()
  (unwind-protect
      (let ((sock (socket:make-socket :connect :passive 
				      :local-host "127.0.0.1")))
	(setf *console-listener* sock)
	(loop
	  (let ((cli (ignore-errors (socket:accept-connection sock))))
	    (when cli
	      (mp:with-process-lock (*console-sockets-lock*)
		(push cli *console-sockets*)
		(console-server-1 cli))))))
    (close *console-listener*)
    (setf *console-listener* nil)))

;; Dump any buffers log data
(defun console-server-1 (sock)
  (declare (optimize (speed 3)))
  (mp:with-process-lock (*log-buffer-lock*)
    (let ((start *log-buffer-start*)
	  (end *log-buffer-end*)
	  (buf *log-buffer*))
      (declare (fixnum start end)
	       (type (simple-array t (*)) buf))
      (handler-case 
	  (progn
	    (while (not (eq start end))
	      (write-string (aref buf start) sock)
	      (incf start))
	    (finish-output sock))
	(error (c)
	  (declare (ignore c))
	  ;; Communication failure, presumably.  Close down.
	  (close sock :abort t)
	  (setf *console-sockets* (delete sock *console-sockets*))))
      (setf *log-buffer-start* start))))

(defun get-console-port ()
  (let ((res 
	 (ignore-errors
	  (sunrpc:with-rpc-client (cli "127.0.01" #.gen-nfs:*nfs-program* 2 :udp)
	    (sunrpc:callrpc cli 102 nil nil :outproc #'xdr:xdr-int)))))
    (if (and res (not (zerop res)))
	res)))

(ff:def-foreign-call CreateMutexA () :strings-convert t
		     :error-value :os-specific)

(defun console (hide)
  ;; Make sure only one instance runs in this session.
  (multiple-value-bind (handle err)
      (CreateMutexA 0 0 "Allegro NFS Console")
    (declare (ignore handle))
    (if (not (zerop err))
	(exit 0)))
  
  (console-control :tray-exit t :close :hide 
		   :show (if hide nil t))
  
  (set-window-title "Allegro NFS Console")
  
  (let (port sock)
    (tagbody
     top
      (setf port (get-console-port))
      (if* (null port)
	 then ;;(format t "no console port~%")
	      (sleep 5)
	      (go top))
      
      ;;(format t "console port is ~d~%" port)
      
      (setf sock (ignore-errors (socket:make-socket :remote-host "127.0.0.1"
						    :remote-port port)))
      (if* (null sock)
	 then ;;(format t "make-socket to port ~d failed.~%" port)
	      (sleep 5)
	      (go top))
      
      (format t "Allegro NFS v~a running.~%" *nfsd-version*)
      
      (unwind-protect
	  (let ((buf (make-string 65536))
		got)
	    (loop
	      (setf got (ignore-errors (read-vector buf sock)))
	      (if* (or (null got) (zerop got))
		 then ;;(format t "EOF or socket error.~%")
		      #+ignore
		      (go top)
		      ;; Terminate when service exits
		      (exit 0) 
		      )
	      (write-vector buf *initial-terminal-io* :end got)
	      (finish-output *initial-terminal-io*)))
	;; cleanup
	(format t "Allegro NFS stopped.~%")
	(close sock :abort t)))))
