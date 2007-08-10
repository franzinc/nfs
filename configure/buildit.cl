;; $Id: buildit.cl,v 1.2 2007/08/10 16:14:16 dancy Exp $

(in-package :cg-user)

(defun buildit ()
  (ide.project:build-project 
   (ide.project:load-project "configure.lpr")
   :distribution-directory "configure/"
   :replace-if-exists t)
  (exit))

(push 'buildit ide:*ide-startup-hook*)

