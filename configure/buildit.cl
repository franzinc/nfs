
(in-package :cg-user)

(defun buildit ()
  (ide.project:build-project 
   (ide.project:load-project "configure.lpr")
   :distribution-directory "configure/"
   :replace-if-exists t
   :increment-build-number nil)
  (exit))

(push 'buildit ide:*ide-startup-hook*)

