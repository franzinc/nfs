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
(defvar *log-file* "sys:nfsdebug-~D.txt")

;; Needs UI [rfe8202]
(defvar *executable-types* '("exe" "com" "bat"))

(defvar *log-rotation-file-size* 0)
(defvar *log-rotation-file-count* 1)
(defvar *log-rotation-current-count* 0)

(defvar *kilobyte* 1024)
(defvar *megabyte* (* *kilobyte* *kilobyte*))
(defvar *gigabyte* (* *megabyte* *kilobyte*))
(defvar *terabyte* (* *gigabyte* *kilobyte*))

(defvar *log-rotation-file-size-magnitude* *megabyte*)

(defvar *nfs-dircache-update-interval* 2)

(defvar *open-file-reap-time* 2)

;; Value should always be larger than *open-file-reap-time*.  This is
;; because the open file reaper needs the cached stat information when
;; closing a file that was opened for writing, so that it can update the
;; atime/mtime of the file.
(defvar *attr-cache-reap-time* 5) 

(defvar *disable-persistent-fhandles* nil)

(defvar *enable-32bit-file-id-truncate* nil)

(defun make-log-rotation-name (index)
  "Appends a version onto the logfile name."
  (format nil *log-file* index))

(defun find-latest-log-file ()
  (let ((latest (make-log-rotation-name 0)))
    ;; Ensure the file exists.
    (unless (probe-file latest)
      (with-open-file (f latest 
			 :direction :output
			 :if-does-not-exist :create)))
    (loop for i from 0 to (1- *log-rotation-file-count*)
	  when (probe-file (make-log-rotation-name i))
	  do (let ((newest (make-log-rotation-name i)))
	       (when (< (file-write-date latest)
			(file-write-date newest))
		 (setf latest newest
		       *log-rotation-current-count* i))))
    latest))

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
