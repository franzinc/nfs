;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
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
;;
;; $Id: mpsocketfix.cl,v 1.5 2001/08/15 23:35:14 dancy Exp $

;; mp socket fix
;; patch for bug10759


(in-package :mp)

;; this was modified
#-(version>= 6 1)
(eval-when (compile)
  (defmacro stream-or-fd-fd (s)
    (unless (symbolp s)
      (error "stream-or-fd requires a symbol"))
    `(if* (fixnump ,s)
	then ,s
      elseif (streamp ,s)
	then (stream-input-fn ,s)
	else (let ((fd (wait-for-input-fd ,s)))
	       (if* (and fd (< fd 0))
		  then nil
		elseif (and fd (>= fd 0))
		  then fd
		  else (excl::.type-error ,s
					  '(or stream fixnum)))))))






; these are unmodified, but use the above:

#-(version>= 6 1)
(defun wait-for-input-available (streams
				 &key (wait-function #'stream::stream-listen)
				      (whostate "waiting for input")
				      timeout)
  (declare (optimize (speed 3)))
  (if (not (listp streams))
      (setq streams (list streams)))
  (let ((fds sys::*stack-group-watchfor-fds*)
	(ret-list (list nil)))		; Return value set in real wait fun
    (dolist (stream-or-fd streams)
      (let ((fd (stream-or-fd-fd stream-or-fd)))
	(when fd
	  (push fd fds))))
    (let ((sys::*stack-group-watchfor-fds* fds))
      (if* timeout
	 then (process-wait-with-timeout
	       whostate timeout #'multi-stream-wait-for-input-wait-function
	       streams wait-function ret-list)
	 else (process-wait whostate
			    #'multi-stream-wait-for-input-wait-function
			    streams wait-function ret-list)))
    (cdr ret-list)))


#-(version>= 6 1)
(defun wait-for-input-available-one (fd stream wait-function)
  (declare (optimize (speed 3)))
  (let ((sys::*stack-group-watchfor-fds*
	  (cons (stream-or-fd-fd fd) sys::*stack-group-watchfor-fds*)))
    (process-wait "waiting for input" wait-function stream)))
