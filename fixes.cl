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
;; $Id: fixes.cl,v 1.3 2002/02/12 17:20:26 layer Exp $

(in-package :excl)

;; patch needed for delete-directory, so it knows the errno of why the
;; delete failed.

#+(and (version>= 6 0) (not (version>= 6 1)))
(assert (and sys:*patches*
	     (let ((lisp-patches
		    (cdr (assoc :lisp sys:*patches* :test #'eq))))
	       (assoc "4a015" lisp-patches :test #'string=))))

#+(and (version>= 5 0 1) (not (version>= 6 0)))
(assert (and sys:*patches*
	     (let ((lisp-patches
		    (cdr (assoc :lisp sys:*patches* :test #'eq))))
	       (assoc "....." lisp-patches :test #'string=))))
