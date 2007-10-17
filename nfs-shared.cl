(in-package :user)

;; $Id: nfs-shared.cl,v 1.8 2007/10/17 17:40:09 dancy Exp $

;; This file contains stuff that is shared between the nfs server code
;; and the configuration app code.

(eval-when (compile eval load)
  (require :regexp2)
  (require :shell)
  (use-package :excl.shell))

(defun cleanup-dir (dir)
  ;; n: => n:\
  ;; n:\src\ => n:\src
  ;; \\foo\bar => \\foo\bar\
  
  ;; convert forward slashes to back slashes
  (setq dir (namestring (pathname dir)))
  
  (if* (=~ "^[A-Za-z]:$" dir)
     then (+= dir "\\")
   elseif (=~ "([A-Za-z]:.+)\\\\$" dir)
     then $1
   elseif (=~ "(\\\\\\\\[^\\\\]+\\\\[^\\\\]+)$" dir)
     then (+= $1 "\\")
     else dir))

#+ignore
(defun test-cleanup-dir ()
  (let (val)
    (dolist (x (list (cons "n:\\" "n:")
		     (cons "n:\\" "n:/")
		     (cons "n:\\" "n:\\")
		     (cons "n:\\src" "n:/src")
		     (cons "n:\\src" "n:\\src")
		     (cons "n:\\src" "n:\\src\\")
		     (cons "n:\\src" "n:/src/")))
      (when (not (string= (car x)
			  (setq val (cleanup-dir (cdr x)))))
	(error "(cleanup-dir ~s): expected ~s, got ~s."
	       (cdr x)
	       (car x)
	       val)))))

