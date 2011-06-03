
(in-package :cg-user)

(defun buildit ()
  (ide.project:build-project 
   (ide.project:load-project "configure.lpr")
   :distribution-directory "configure/"
   :replace-if-exists t
   :increment-build-number nil)
  (dolist (p mp:*all-processes*)
    (format t "; killing ~a...~%" p)
    (when (not (eq mp:*current-process* p))
      (mp:process-kill p)))
  (exit))

(push 'buildit ide:*ide-startup-hook*)

