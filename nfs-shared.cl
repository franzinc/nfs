;; -*- mode: common-lisp -*-
;; See the file LICENSE for the full license governing this code.

(in-package :user)

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
  
  ;; convert forward slashes to backslashes
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
  (let ((cases '(
		 ;; (expected-result input)
		 ("n:\\" "n:")
		 ("n:\\" "n:/")
		 ("n:\\" "n:\\")
		 
		 ("n:\\src" "n:/src")
		 ("n:\\src" "n:\\src")
		 ("n:\\src" "n:\\src\\")
		 ("n:\\src" "n:/src/")
		 
		 ("\\\\server\\share\\" "\\\\server\\share")
		 ("\\\\server\\share\\" "\\\\server\\share\\")
		 ("\\\\server\\share\\" "//server/share")
		 ("\\\\server\\share\\" "//server/share/")
		 )))
    (loop for (expected input) in cases
	do (let ((got (cleanup-dir input)))
	     (when (string/= got expected)
	       (error "(cleanup-dir ~s): expected ~s, got ~s."
		      input expected got))))))
