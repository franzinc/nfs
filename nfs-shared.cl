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
