
(in-package :cl-user)

#+(version>= 9 0)
(eval-when (compile eval load) (require :ide))

(defun buildit ()
  #+(version>= 9 0)
  (setq *print-readably* nil)
  
  (ide.project:build-project 
   (ide.project:load-project "configure.lpr")
   :distribution-directory "configure/"
   :replace-if-exists t
   :increment-build-number nil)
  
  #+(version= 8 2)
  (dolist (p mp:*all-processes*)
    (format t "; killing ~a...~%" p)
    (when (not (eq mp:*current-process* p))
      (mp:process-kill p)))

  (exit 0))

#+(version= 8 2)
(push 'buildit ide:*ide-startup-hook*)

#+(version>= 9 0)
(buildit)
