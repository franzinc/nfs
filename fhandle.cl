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
;; $Id: fhandle.cl,v 1.10 2002/09/20 21:03:31 dancy Exp $

;; file handle stuff


(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

(defconstant *fhsize* 32)

(defparameter *fhandles* (make-hash-table))
(defparameter *pathnames* (make-hash-table :test #'equalp))

(defun get-unused-fhandle-id ()
  (let ((id 1))
    (while (gethash id *fhandles*)
      (setf id (random (expt 2 20))))
    id))

(defun pathname-to-fhandle-id (p)
  (declare
   (type pathname p))
  (let ((res (gethash p *pathnames*)))
    (if res
        res
      (let ((id (get-unused-fhandle-id)))
        (setf (gethash id *fhandles*) p)
        (setf (gethash p *pathnames*) id)
        id))))

(defun pathname-to-fhandle-with-xdr (xdr p)
  (declare
   (type pathname p)
   (type xdr xdr))
  (let ((id (pathname-to-fhandle-id p)))
    (dotimes (i (/ *fhsize* 4))
      (xdr-unsigned-int xdr id))
    id))


(defmacro fhandle-id-to-pathname (id)
  `(gethash ,id *fhandles*))

;;; most of the time...  I want to take an xdr and get an pathname
;;; out of it
(defun xdr-fhandle-to-pathname (xdr)
  (declare 
   (type xdr xdr))
  (let ((id (xdr-int xdr)))
    (xdr-advance xdr (- *fhsize* 4))
    (values (fhandle-id-to-pathname id) id)))

(defun opaque-fhandle-to-pathname (of)
  (let ((xdr (first of))
	(offset (second of)))
    (declare
     (type xdr xdr)
     (type fixnum offset))
    (xdr-with-seek (xdr offset :absolute t)
		   (xdr-fhandle-to-pathname xdr))))

;; To be used with file deletion (or deletion as a side effect
;; of renaming onto an existing file).
(defun remove-fhandle-by-pathname (p)
  (let ((id (pathname-to-fhandle-id p)))
    (remhash p *pathnames*)
    (remhash id *fhandles*)))
  
  

;;; from and to are pathnames.
;;; need to update *fhandles* and *pathnames*
(defun swap-fhandles (from to)
  (let ((fromid (pathname-to-fhandle-id from))
	(toid (pathname-to-fhandle-id to)))
    (setf (gethash to *pathnames*) fromid) 
    (setf (gethash from *pathnames*) toid)
    (setf (gethash fromid *fhandles*) to)
    (setf (gethash toid *fhandles*) from)))



;;; for debugging
(defun dump-fhandles ()
  (maphash #'(lambda (x y) (format t "~S -> ~S~%" x y))
	   *fhandles*))
