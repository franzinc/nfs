(in-package :cg-user)

(defun buildit ()
  (build-project (load-project "configure.lpr")
		 :distribution-directory "configure/"
		 :replace-if-exists t)
  (exit))

(push 'buildit *ide-startup-hook*)

