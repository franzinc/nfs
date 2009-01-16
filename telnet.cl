;; $Id: telnet.cl,v 1.4 2009/01/16 20:41:46 layer Exp $

;; Source code for telnet-server example
(in-package :user)
;; A telnet server for Allegro CL.
;;
;; This source code is in the public domain.

(defun start-telnet-server (&key (port 1234))
  (mp:process-run-function "telnet server" 'start-telnet-server-1 port))

(defun start-telnet-server-1 (port)
  (loop
    (let ((socket (socket:make-socket :connect :passive :local-port port
                                      :reuse-address t)))
      (unwind-protect
          (loop
            (let ((connection
                    (ignore-errors (socket:accept-connection socket)))
                   ;;  The ignore-errors protects against the rare
                   ;;  occurrence of accept-connection signaling
                   ;;  an error (usually Connection Reset by Peer)
                  from)
              (when connection
                 (handler-case
                     (progn
                       (setq from (or (socket:ipaddr-to-hostname
                                       (socket:remote-host connection))
                                      (socket:ipaddr-to-dotted
                                       (socket:remote-host connection))))
                       (logit-stamp "telnet server: new connection from ~a~%"
				    from)
		       (setf (eol-convention connection) :dos)
                       (format connection "
WARNING: do not use :exit or (exit).  Use ~s to quit."
                                    '(quit))
                       (force-output connection)
                       (mp:process-run-function
                           "telnet session"
                         'start-telnet-session connection from))
                   (error ()
                     (ignore-errors (close connection)))))))
        (ignore-errors (close socket))))))


(defvar *in-telnet-session* nil)

(defun start-telnet-session (s from)
  (unwind-protect
      (catch 'end-telnet-session
        (let ((*in-telnet-session* t))
	  (setq excl::*set-acl-running-mutex* nil)
          (tpl:start-interactive-top-level
           s 'tpl:top-level-read-eval-print-loop nil)))
    (ignore-errors (close s)))
  (logit-stamp "telnet server: closing connection from ~a~%" from))

(defun quit ()
  (throw 'end-telnet-session nil))

(defvar *exit-wrapped* nil)

(when (not *exit-wrapped*)
  (flet ((msg ()
           (format t "Use ~s instead of exit.~%" '(quit))))
    (def-fwrapper exit-wrapper (&optional status &rest args)
      (declare (ignore args))
      (if* *in-telnet-session*
         then (msg)
         else (call-next-fwrapper)))

    (fwrap 'excl:exit :telnet-server 'exit-wrapper)
    (fwrap 'tpl::exit-command :telnet-server 'exit-wrapper))) 
