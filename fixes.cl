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
;; $Id: fixes.cl,v 1.2 2001/08/15 23:35:14 dancy Exp $

(in-package :excl)

#-(version>= 6 1)
(eval-when (compile load eval)
  (defconstant excl::sc-rmdir 75))

#-(version>= 6 1)
(defun rmdir (directory)
  (let ((dir (truename directory)))
    (multiple-value-bind (res errcode)
	(with-native-string (x (namestring dir))
	  (excl::.primcall 'sys::lisp-syscall #.sc-rmdir x))
      (if* errcode
	 then (error 'file-error
		     :pathname dir
		     :errno errcode
		     :format-control "Could not remove directory: ~a."
		     :format-arguments (list (er-number-to-string errcode)))
	 else res))))
