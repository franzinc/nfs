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

(defvar *canonical-name-regexp*
    (compile-re "^/(?:.*[^/])?$")
  "A regexp to match canonical names.  The rules of which are the following:

  1) must begin with a slash (therefore cannot be a blank string)
  2) may or may not have more path.
  3) must not end with a slash, unless the string consists only of a single
     slash")

(defun canonical-name-p (name)
  "Returns true if a name is canonical."
  (when (match-re *canonical-name-regexp* name)
    t))

(defun eat-trailing-slashes (name)
  "Returns a version of NAME without trailing slashes
   unless NAME is a single slash, in which case NAME is returned.

   The returned string may or may not be eq to NAME."
  
  ;; This code is simple and easy to understand.  Don't bother
  ;; optimizing it.  It's not currently used in performance-critical
  ;; code.
  
  (let ((len (length name)))
    (if* (<= len 1)
       then ;; Base case.  Blank or single-character string.
	    ;; "/" technically has a trailing slash, but it must not be removed.
	    ;; We're done.
	    name
       else (let ((last-char (char name (1- len))))
	      (if* (char= last-char #\/)
		 then ;; Multiple-character string with trailing
		      ;; slash.  Drop the trailing slash
		      ;; and recurse.
		      (eat-trailing-slashes (subseq name 0 (1- len)))
		 else ;; No trailing slash.  done.
		      name)))))
		      
;; Called by define-export, :operator
(defun canonicalize-name (name)
  "Returns a canonicalized version of NAME if possible.

  * If NAME does not begin with a slash, an error is thrown.    

  * If NAME consists of a single slash, NAME is returned.

  * Otherwise, a version of NAME with all trailing slashes removed is
    returned.  The returned string may or may not be eq to NAME."
  
  (when (not (prefixp "/" name))
    (error "~s: NAME must begin with a slash" name))
  
  (eat-trailing-slashes name))

(defun trailing-slashify (string)
  "If STRING has a trailing slash, returns STRING.
   If STRING is a single slash, returns STRING.
   Otherwise returns a copy of STRING with a trailing slash.
   Note that this means if STRING is blank, a string consisting
   of a single slash will be returned."
  (if* (or (match-re "/$" string)
	   (string= string "/"))
     then string
     else (concatenate 'string string "/")))
