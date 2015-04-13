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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USER

(in-package :user)

(defvar *nfs-debug* nil)
(defvar *nfs-gc-debug* nil)
(defvar *nfs-debug-timings* nil)
(defvar *nfs-set-mtime-on-write* nil)
(defvar *nfs-debug-filter* #x0fffffff)

;; Needs UI [rfe8202]
(defvar *executable-types* '("exe" "com" "bat"))

(defvar *log-rotation-file-size* 0)
(defvar *log-rotation-file-count* 1)

(defvar *kilobyte* 1024)
(defvar *megabyte* (* *kilobyte* *kilobyte*))
(defvar *gigabyte* (* *megabyte* *kilobyte*))
(defvar *terabyte* (* *gigabyte* *kilobyte*))

(defvar *log-rotation-file-size-magnitude* *megabyte*)

(defvar *nfs-dircache-update-interval* 2)

;; should always be larger than *openfilereaptime*.  FIXME: Why?
(defvar *attr-cache-reap-time* 5) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PORTMAP

(defpackage :xdr
  (:use :lisp :excl))

(defpackage :portmap
  (:use :lisp :excl :xdr)
  (:export #:*portmap-debug*
	   #:*use-system-portmapper*))

(in-package :portmap)

(defvar *portmap-debug* nil)
(defvar *use-system-portmapper* :auto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOUNT

(defpackage :mount
  (:use :lisp :excl :xdr)
  (:export #:*mountd-debug* 
	   #:*mountd-port-number*
	   #:*showmount-disabled*))

(in-package :mount)

(defvar *mountd-debug* nil)
(defvar *mountd-port-number* nil)
(defvar *showmount-disabled* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NSM

(defpackage :nsm
  (:use :lisp :excl :xdr)
  (:export #:*nsm-debug*
	   #:*nsm-port*))

(in-package :nsm)

(defvar *nsm-debug* nil)
(defvar *nsm-port* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NSM

(defpackage :nlm
  (:use :lisp :excl :xdr)
  (:export #:*nlm-debug*
	   #:*nlm-port*))

(in-package :nlm)

(defvar *nlm-debug* nil)
(defvar *nlm-port* nil)
