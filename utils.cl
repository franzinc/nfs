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

(eval-when (compile load eval)
  (require :defsubst))

(deftype usb8 () '(unsigned-byte 8))
(deftype sb32 () '(signed-byte 32))
(deftype usb64 () '(unsigned-byte 64))
(deftype ausb8 () '(simple-array usb8 (*)))
(deftype asb32 () '(simple-array sb32 (*)))

(defmacro make-ausb8 (size &rest rest)
  `(make-array ,size :element-type 'usb8 ,@rest))

(defconstant *sizeof-fixnum* (ff:sizeof-fobject :nat))

;; Only works if compiled
(excl::defsubst set-sb32-in-vec (value vec offset)
  "OFFSET must be a multiple of 4"
  (declare (optimize speed (safety 0))
	   (sb32 value)
	   (asb32 vec)
	   (fixnum offset))
  ;; Change the division back to ash when bug22150 is fixed
  (setf (aref vec (/ offset 4)) value)
  vec)

(excl::defsubst get-sb32-in-vec (vec offset)
  "OFFSET must be a multiple of 4"
  (declare (optimize speed (safety 0))
	   (asb32 vec)
	   (fixnum offset))
  (aref vec (ash offset -2)))

;; FIXME: Optimize
(defun put-uint64-into-vec (value vec offset)
  (declare (optimize speed (safety 0))
	   (usb64 value)
	   (ausb8 vec)
	   (fixnum offset))
  (let ((shift -64))
    (declare ((integer -64 0) shift))
    (dotimes (n 8)
      (incf shift 8)
      (setf (aref vec offset) (ash value shift))
      (incf offset))))

;; FIXME: Optimize
(defun get-uint64-from-vec (vec offset)
  (declare (optimize speed (safety 0))
	   (ausb8 vec)
	   (fixnum offset))
  (let ((res 0))
    (declare (usb64 res))
    
    (dotimes (n 8)
      (setf res (logior (ash res 8) (aref vec offset)))
      (incf offset))
    
    res))


;; FIXME: Make a more efficient version if we detect that 
;; dest-offset, src-offset and len are each a multiple of 4.
(excl::defsubst copy-ausb8-into (dest dest-offset src src-offset len)
  (declare (optimize speed (safety 0))
	   (ausb8 dest src)
	   (fixnum dest-offset src-offset len))
  (dotimes (n len)
    (setf (aref dest dest-offset) (aref src src-offset))
    (incf dest-offset)
    (incf src-offset))
  dest)

;; Generates a hexdump of the first MAX-BYTES of FILENAME out to
;; STREAM.  The hexdump is terminated with a newline if TERPRI is true.
;; The return value is undefined.
(defun hexdump-file-to-stream (filename max-bytes stream terpri)
  (with-open-file (f filename)
    (let* ((buf (make-ausb8 max-bytes))
	   (got (read-sequence buf f)))
      (dotimes (n got)
	(format stream "~2,'0x " (aref buf n)))
      (when terpri
	(terpri stream)))))

;; Generates a hexdump of the first MAX-BYTES of FILENAME out to
;; STREAM, which defaults to *standard-output*.  If STREAM is nil, a
;; string containing the hexdump will be returned.  If STREAM is not
;; nil, the returned value is undefined.  TERPRI is used to determine
;; whether or not a newline is added to the end of the hexdump.
;; TERPRI defaults to true unless STREAM is nil, in which case it
;; defaults to false.
(defun hexdump-file (filename max-bytes &key (stream *standard-output*) 
					     (terpri t terpri-supplied-p))
  (if* stream
     then (hexdump-file-to-stream filename max-bytes stream terpri)
     else (with-output-to-string (stream)
	    (hexdump-file-to-stream filename max-bytes stream
				    (if* terpri-supplied-p
				       then terpri
				       else ;; Default to no newline
					    ;; for string output.
					    nil)))))
