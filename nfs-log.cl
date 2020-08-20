;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2014 Franz Inc, Oakland, CA.  All rights reserved.
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

;; Logging support

(in-package :user)

(defvar *log-stream* nil)
(defvar *log-stream-lock* (mp:make-process-lock)
  "A gate for use in logging.")
(defvar *program-mode* nil) ;; nil or :service
(defvar *console-sockets* nil)
(defvar *console-sockets-lock* (mp:make-process-lock))
;;(defvar *log-rotation-current-count* 0) ; moved to config-defs
;;(defvar *log-file* "sys:nfsdebug-~D.txt") ; moved to config-defs
(defvar *nfs-debug-stream* nil)

(defun log-rotateable (string)
  "Returns true if the *log-stream* will need rotation to write
a given string."
  (and (not (= 0 *log-rotation-file-size*))
       (< (* *log-rotation-file-size*
	     *log-rotation-file-size-magnitude*)
         (+ (file-length *log-stream*)
	    (length string)))))
;;defun make-log-rotation-name ; moved to config-defs

(defun rotate-log ()
  "Rotates *log-stream*"
  ;; Setup the next log file count.
  (write-string "Rotating away from this logfile." *log-stream*)
  (incf *log-rotation-current-count*)
  (when (<= *log-rotation-file-count*
	    *log-rotation-current-count*)
    (setf *log-rotation-current-count* 0))
  
  ;; Open up the new log file
  (let ((new-log (open (make-log-rotation-name
			*log-rotation-current-count*)
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :supersede
		       :external-format :utf8)))
      (when (and new-log
		 (open-stream-p new-log))
	;; If streams are open, flush and close.
	(mp:with-process-lock (*log-stream-lock*)
	  (flet ((close-stream (stream)
		   (when (open-stream-p stream)
		     (finish-output stream)
		     (close stream :abort nil))))
	    ;; bug22497:
	    ;; Close *log-stream* first, in case we're in non-service
	    ;; mode, because otherwise an error results.
	    ;; Reported by John Peterson.
	    ;; https://github.com/franzinc/nfs/pull/7
	    (close-stream *log-stream*)
	    (close-stream *nfs-debug-stream*))
	  ;; If we are running as a service then use the new file in both places.
	  (if* (eq *program-mode* :service)
	     then (setf *log-stream* new-log
			*nfs-debug-stream* new-log)
		  (logit-stamp "~&Rotated logfile successfully to ~A~%"
			       (file-namestring new-log))
		  ;; Otherwise we setup the log-stream to be debug plus terminal io.
	     else (setf *nfs-debug-stream* new-log
			*log-stream* (make-broadcast-stream *initial-terminal-io* 
							    *nfs-debug-stream*)))))))

(defun logit-1 (string)
  (mp:with-process-lock (*log-stream-lock*)
    (when *log-stream*
      (when (log-rotateable string)
	(rotate-log))
      (write-string string *log-stream*)
      (force-output *log-stream*)))
 
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
  (multiple-value-bind (sec min hour day month year ignore usec)
      (get-decoded-time)
    (declare (ignore ignore))
    (multiple-value-setq (ignore usec) (excl::acl-internal-real-time))
    (logit "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d.~3,'0d| " 
	   year month day hour min sec (truncate usec 1000))
    (apply #'logit format-string format-args)))

(eval-when (compile eval load)
  (require :streamc) ;; for make-broadcast-stream
  )

;; defun find-latest-log-file   ; moved to config-defs

(defun setup-logging (&optional reopen)
  (mp:with-process-lock (*log-stream-lock*)
    (when reopen
      (when *nfs-debug-stream* (close *nfs-debug-stream*))
      (setf *log-stream* nil))

    (let ((latest (find-latest-log-file)))
      (when (null *log-stream*)
	(setf *nfs-debug-stream*
	  (open latest
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
		     (translate-logical-pathname latest))))))

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
