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
  (require :osi)
  (use-package :excl.osi)
  (use-package :gen-nfs))

;; NOTE: the form of the version *must* be a.b.c.  If you're starting
;;       a new release, say 6.0, then use 6.0.0.
(defvar *nfsd-version* "6.1.beta.4")
(defvar *nfsd-long-version*
    (format nil "~a (NFSv2/NFSv3)" *nfsd-version*))
(load (merge-pathnames "commit-id.cl" *load-pathname*))

;; Filesystem allocation unit size.  Only used by statfs procedure.
;;   See discussion in spr39245 for why this was changed from 8192.
(defconstant *blocksize* 512)

(defconstant *nfs-debug-read*        #x00000001)
(defconstant *nfs-debug-write*       #x00000002)
(defconstant *nfs-debug-readdir*     #x00000004) ;; includes readdirplus
(defconstant *nfs-debug-getattr*     #x00000008)
(defconstant *nfs-debug-setattr*     #x00000010)
(defconstant *nfs-debug-lookup*      #x00000020)
(defconstant *nfs-debug-access*      #x00000040)
(defconstant *nfs-debug-create*      #x00000080)
(defconstant *nfs-debug-mkdir*       #x00000100)
(defconstant *nfs-debug-rmdir*       #x00000200)
(defconstant *nfs-debug-remove*      #x00000400)
(defconstant *nfs-debug-rename*      #x00000800)
(defconstant *nfs-debug-fsstat*      #x00001000)
(defconstant *nfs-debug-fsinfo*      #x00002000)
(defconstant *nfs-debug-pathconf*    #x00004000)
(defconstant *nfs-debug-commit*      #x00008000)
(defconstant *nfs-debug-null*        #x00010000)
(defconstant *nfs-debug-statfs*      #x00020000)
(defconstant *nfs-debug-link*        #x00040000)
(defconstant *nfs-debug-symlink*     #x00080000)
(defconstant *nfs-debug-readlink*    #x00100000)
(defconstant *nfs-debug-mknod*       #x00200000)

(defmacro nfs-debug-filter-on (type)
  (if (eq type 'readdirplus)
      (setf type 'readdir))
  (let ((constant (intern (format nil "*nfs-debug-~a*" type))))
    `(and *nfs-debug* (/= 0 (logand *nfs-debug-filter* ,constant)))))

(defun map-errno-to-nfs-error-code (errno)
  (case errno
    (#.*enoent* *nfserr-noent*)
    (#.*eio* *nfserr-io*)
    (#.*eacces* *nfserr-acces*)
    (#.*enfile* *nfserr-acces*)
    (#.*enotempty* *nfserr-notempty*) 
    (#.*eexist* *nfserr-exist*)
    (#.*einval* *nfserr-inval*)
    (#.*enospc* *nfserr-nospc*)
    ;; very general... avoid.  For v3, should should be
    ;; *nfserr-serverfault*
    (t *nfserr-io*))) 

;; Needed for proper error reporting.
(eval-when (compile load eval)
  (setf excl::*strict-probe-file* t))
    
(defun roundup (value multiple)
  (let ((mod (mod value multiple)))
    (+ value (if (> mod 0) (- multiple mod) 0))))

;;; return how many blocks are required to contain 'value' items
;;; given a particular blocksize
(defun howmany (value blocksize)
  (/ (roundup value blocksize) blocksize))

(define-compiler-macro howmany (value blocksize &whole whole &environment env)
  (flet ((power-of-two-p (value)
	   (zerop (nth-value 1 (truncate (log value 2))))))
    (let ((cv 
	   (and (constantp blocksize env) (sys:constant-value blocksize env))))
      (if* (and cv (power-of-two-p cv))
	 then (setf blocksize cv)
	      (let ((v (gensym)))
		`(let ((,v ,value))
		   (if* (fixnump ,v)
		      then (let ()
			     (declare (fixnum ,v))
			     (ash (logand (+ ,v ,(1- blocksize))
					  (lognot ,(1- blocksize)))
				  ,(- (truncate (log blocksize 2)))))
		      else (ash (logand (+ ,v ,(1- blocksize))
					(lognot ,(1- blocksize)))
				,(- (truncate (log blocksize 2)))))))
	 else whole))))

(defmacro bailout (format &rest format-args)
  `(progn
     (logit-stamp ,format ,@format-args)
     (console-control :close t :show t)
     (exit 1)))

(ff:def-foreign-call MoveFileExA ((from (* :char)) 
				  (to (* :char))
				  (flags :int))
  :strings-convert t
  :returning :boolean
  :error-value :os-specific)

(ff:def-foreign-call MoveFileExW ((from (* :void)) 
				  (to (* :void))
				  (flags :int))
  :strings-convert nil
  :returning :boolean
  :error-value :os-specific)

(defconstant MOVEFILE_REPLACE_EXISTING 1)

(defun my-rename (from to &key unicode)
  (multiple-value-bind (success winerr)
      (if* unicode
	 then (MoveFileExW from to MOVEFILE_REPLACE_EXISTING)
	 else (MoveFileExA from to MOVEFILE_REPLACE_EXISTING))
    (if* success
       then t
       else (excl.osi:perror (excl.osi::win_err_to_errno winerr)
			     "rename failed"))))
