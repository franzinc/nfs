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
;; $Id: util.cl,v 1.13 2003/06/06 16:56:46 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (require 'winapi)
  (require 'winapi-dev))

(ff:def-foreign-type stat
    (:struct
     (st_dev :unsigned-int)  
     (st_ino :unsigned-short)  
     (st_mode :unsigned-short)
     (st_nlink :short)
     (st_uid :short)
     (st_gid :short)
     (st_rdev :unsigned-int) 
     (st_size :int) 
     (st_atime :int) 
     (st_mtime :int)
     (st_ctime :int)))     

(ff:def-foreign-call (stat "_stat") ((filename (* :char) string)
                                     (statbuf (* stat)))
  :strings-convert t
  )

(ff:def-foreign-type large-integer 
    (:struct
     (lowpart :unsigned-int)
     (highpart :unsigned-int)))

(ff:def-foreign-call (GetDiskFreeSpaceExA "GetDiskFreeSpaceExA")
    ((filename :foreign-address)
     (freebytes :foreign-address)
     (totalbytes :foreign-address)
     (realfreebytes :foreign-address)))

(ff:def-foreign-call (SetFilePointer "SetFilePointer")
    (hFile lDistanceToMove lpDistanceToMoveHigh dwMoveMethod))

(ff:def-foreign-call (SetEndOfFile "SetEndOfFile") (hFile))

(ff:def-foreign-type utimbuf 
    (:struct
     (actime :unsigned-int)
     (modtime :unsigned-int)))

(ff:def-foreign-call (utime "_utime") ((filename (* :char))
				       (times (* utimbuf)))
  :strings-convert t)

(defconstant FILE_BEGIN           0)
(defconstant FILE_CURRENT         1)
(defconstant FILE_END             2)

(defconstant INVALID_SET_FILE_POINTER -1)

(defun diskfree (root)
  (with-native-string (nativestring (namestring root))
    (let ((freebytes (ff:allocate-fobject 'large-integer :foreign-static-gc))
	  (totalbytes (ff:allocate-fobject 'large-integer :foreign-static-gc))
	  res)
      (setf res (GetDiskFreeSpaceExA nativestring freebytes totalbytes 0))
      (when (= 0 res)
	(error "Call to GetDiskFreeSpaceExA failed"))
      (values 
       (+ (ash (ff:fslot-value freebytes :highpart) 32)
	  (ff:fslot-value freebytes :lowpart))
       (+ (ash (ff:fslot-value totalbytes :highpart) 32)
	  (ff:fslot-value totalbytes :lowpart))))))

(defun basename (p)
  (setf p (namestring p))
  (if (char= #\\ (schar p (1- (length p))))
      (setf p (subseq p 0 (1- (length p)))))
  (let ((pos (position #\\ p :from-end t)))
    (if (null pos)
        p
      (subseq p (1+ pos)))))

(defun roundup (value multiple)
  (let ((mod (mod value multiple)))
    (+ value (if (> mod 0) (- multiple mod) 0))))

;;; return how many blocks are required to contain 'value' items
;;; given a particular blocksize
(defun howmany (value blocksize)
  (/ (roundup value blocksize) blocksize))

(defun createfile (filename)
  (setf filename (namestring filename))
  (with-native-string (filename-native filename)
    (win:CreateFile 
     filename-native
     win:GENERIC_WRITE 
     0
     0
     win:OPEN_EXISTING
     win:FILE_ATTRIBUTE_NORMAL
     0)))

(defun truncate-file (filename size)
  (let (hFile res err)
    (without-interrupts
      (setf hFile (createfile filename))
      (setf err (win:GetLastError)))
    (if* (= hFile win:INVALID_HANDLE_VALUE)
       then
	    (format t "truncate-file: Failed to open file ~A~%" filename)
	    (return-from truncate-file err))
    
    (without-interrupts
      (setf res (SetFilePointer hFile size 0 FILE_BEGIN))
      (setf err (win:GetLastError)))
    (if* (= res INVALID_SET_FILE_POINTER)
       then 
	    (win:CloseHandle hFile)
	    (format t "truncate-file: SetFilePointer failed w/ err code ~D~%"
		    err)
	    (return-from truncate-file err))
    
    (without-interrupts
      (setf res (SetEndOfFile hFile))
      (setf err (win:GetLastError)))
    (if* (= res 0)
       then 
	    (win:CloseHandle hFile)
	    (format t "truncate-file: SetEndofFile failed w/ err code ~D~%"
		    err)
	    (return-from truncate-file err))
    
    (win:CloseHandle hFile)			
    0))

(defun set-file-time (filename atime mtime)
  (setf filename (namestring filename))
  (let ((times (ff:allocate-fobject 'utimbuf :foreign-static-gc)))
    (setf (ff:fslot-value times 'actime) atime)
    (setf (ff:fslot-value times 'modtime) mtime)
    (utime filename times)))
