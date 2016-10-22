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

(defpackage :xdr
  (:use :lisp :excl)
  (:export
   #:xdr-vec
   #:xdr-pos
   #:xdr-size
   #:xdr-direction
   #:create-xdr
   #:xdr-get-vec
   #:xdr-flush
   #:with-xdr-seek
   #:xdr-backspace
   #:with-xdr-compute-bytes-added
   #:xdr-void
   #:xdr-unsigned-int
   #:xdr-unsigned
   #:xdr-uint32
   #:xdr-int
   #:xdr-long
   #:xdr-int32
   #:xdr-short
   #:xdr-char
   #:xdr-bool
   #:xdr-unsigned-hyper
   #:xdr-uint64
   #:xdr-hyper
   #:make-opaque
   #:opaque-vec
   #:opaque-offset
   #:opaque-len
   #:opaque-data
   #:xdr-opaque-fixed
   #:xdr-opaque-variable
   #:xdr-opaque-variable-from-stream
   #:with-opaque-xdr
   #:xdr-array-fixed
   #:xdr-array-variable
   #:xdr-string
   #:xdr-string-utf8
   #:xdr-xdr
   #:xdr-timeval
   #:xdr-optional
   #:defxdrstruct
   #:defxdrunion
   #:xdr-opaque))

(in-package :xdr)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

(eval-when (compile load eval)
  (require :streamp)) ;; for memcpy

(defstruct xdr
  (vec nil :type (simple-array (unsigned-byte 8) (*)))
  ;; build: maximum offset in vec (not necessarily the length of the vec)
  (size 0 :type fixnum)
  direction
  (pos 0 :type fixnum)) ;; current position in vec (extracting and building)

(defmethod print-object ((xdr xdr) stream)
  (format stream "#<xdr for ~a, veclen=~d, size=~d, pos=~d>"
	  (xdr-direction xdr) (length (xdr-vec xdr)) 
	  (xdr-size xdr) (xdr-pos xdr)))

;; This is also the amount to expand by when expansion of the internal
;; vector is required
(defconstant *xdrdefaultsize* 1024)

(defmacro compute-padded-len (len)
  (let ((l (gensym)))
    `(let ((,l ,len))
       (declare (optimize (speed 3) (safety 0) (debug 0))
		(fixnum ,l))
       (logand (+ ,l 3) -4))))

(defmacro with-xdr-seek ((xdr pos &key absolute) &body body)
  (let ((oldpos (gensym)))
    `(let ((,oldpos (xdr-pos ,xdr)))
       (declare (fixnum ,oldpos))
       (if* ,absolute 
	  then (setf (xdr-pos ,xdr) ,pos)
	  else (setf (xdr-pos ,xdr) (the fixnum (+ (xdr-pos ,xdr) 
						   (the fixnum ,pos)))))
       (prog1 ,@body
	 (setf (xdr-pos ,xdr) ,oldpos)))))

(defmacro with-xdr-compute-bytes-added ((xdr) &body body)
  (let ((oldpos (gensym)))
    `(let ((,oldpos (xdr-pos ,xdr)))
       ,@body
       (- (xdr-pos ,xdr) ,oldpos))))

;; used during extraction
(defmacro xdr-advance (xdr size)
  `(incf (the fixnum (xdr-pos ,xdr)) (the fixnum ,size)))

;; used during building... bumps the max size (xdr-size) value if 
;; necessary.  Probably should be called xdr-update-size
(defmacro xdr-update-pos (x amt)
  (let ((xdr (gensym))
	(amount (gensym))
	(newsize (gensym)))
    `(let ((,xdr ,x)
	   (,amount ,amt))
       (declare (optimize (speed 3) (debug 0) (safety 0))
		(type fixnum ,amount)
		(type xdr ,xdr))
       (let ((,newsize (+ (xdr-pos ,xdr) ,amount)))
	 (declare (fixnum ,newsize))
	 (setf (xdr-pos ,xdr) ,newsize)
	 (if (> ,newsize (xdr-size ,xdr))
	     (setf (xdr-size ,xdr) ,newsize))
	 nil))))



;; used during building.  This will not work correctly if used within
;; a with-xdr-seek
(defmacro xdr-backspace (xdr size)
  `(progn
     (decf (the fixnum (xdr-pos ,xdr)) (the fixnum ,size))
     (decf (the fixnum (xdr-size ,xdr)) (the fixnum ,size))))

(defmacro make-vec (size)
  `(make-array ,size :element-type '(unsigned-byte 8)))

(defun create-xdr (&key (direction :extract) vec size)
  (case direction
    (:extract
     (create-xdr-extract-mode vec size))
    (:build
     (create-xdr-build-mode vec size))
    (t 
     (error "create-xdr: Unknown direction: ~A~%" direction))))

(define-compiler-macro create-xdr (&key (direction :extract) vec size &whole whole)
  (case direction
    (:extract
     `(create-xdr-extract-mode ,vec ,size))
    (:build
     `(create-xdr-build-mode ,vec ,size))
    (t 
     whole)))
  

(defun create-xdr-extract-mode (vec size)
  (let ((xdr (make-xdr)))
    (setf (xdr-direction xdr) :extract)
    (unless (vectorp vec)
      (error "~
create-xdr: 'vec' parameter must be specified and must be a vector"))
    (setf (xdr-pos xdr) 0)
    (setf (xdr-vec xdr) vec)
    (unless size
      (setf size (length vec)))
    (setf (xdr-size xdr) size)
    xdr))

(defun create-xdr-build-mode (vec size)
  (let ((xdr (make-xdr)))
    (setf (xdr-direction xdr) :build)
    (unless size 
      (setf size *xdrdefaultsize*))
    (if vec
	(setf (xdr-vec xdr) vec)
      (setf (xdr-vec xdr) (make-vec size)))
    (setf (xdr-size xdr) 0)
    (setf (xdr-pos xdr) 0)
    xdr))


(defun xdr-flush (xdr)
  (setf (xdr-size xdr) 0)
  (setf (xdr-pos xdr) 0))

;;; avoid!
(defun xdr-get-vec (xdr)
  (subseq (xdr-vec xdr) 0 (xdr-size xdr)))

;; Callers need to make sure that they don't have a handle
;; on the 'vec' slot of the xdr before calling this function.  If they
;; do, they may find themselves accessing a vector that is no longer
;; relevant (in case a new vec is created due to expansion).  
;; For convenience, this function returns the most relevant vec.
(defun xdr-expand-check (xdr more)
  (declare
   (optimize (speed 3) (safety 0) (debug 0))
   ;;(:explain :calls :types)
   (type xdr xdr)
   (type fixnum more))
  (when (> (+ (xdr-pos xdr) more) (length (xdr-vec xdr)))
    (setf (xdr-vec xdr)
      (concatenate '(vector (unsigned-byte 8))
	(xdr-vec xdr)
	(make-vec (max *xdrdefaultsize* more)))))
  (xdr-vec xdr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xdr-void (xdr &optional arg)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (ignore xdr arg))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (compile)
  (setf (get 'xdr-get-signed-int 'sys::immed-args-call)
    '((:lisp :lisp) :machine-integer))
  (setf (get 'xdr-store-signed-int 'sys::immed-args-call)
    '((:machine-integer :lisp :lisp) :machine-integer))
  (setq comp::*assemble-function-body*
    '((xdr-get-signed-int . "xdr-get-signed-int.lap")
      (xdr-store-signed-int . "xdr-store-signed-int.lap"))))

;; code vector really defined by xdr-get-signed-int.lap
(defun xdr-get-signed-int (usb8 position)
  (declare (optimize speed (safety 0) (debug 0)))
  (excl::ll :aref-mi
      ;; (aref-mi doesn't support this like aref-nat):
      ;;      #.(sys::mdparam 'comp::md-lvector-data0-norm)
      usb8 position))

;; code vector really defined by xdr-store-signed-int.lap
(defun xdr-store-signed-int (value usb8 position)
  (declare (optimize speed (safety 0) (debug 0)))
  (excl::ll :aset-nat usb8 position value))

(defun xdr-int (xdr &optional int)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if* (eq (xdr-direction xdr) :build)
     then
	  (if (null int)
	      (error "'int' is required when building"))
	  (let ((vec (xdr-expand-check xdr 4)))
	    (declare (type (integer #.(- (expt 2 31)) #.(1- (expt 2 31))) int))
	    (xdr-store-signed-int int vec (ash (xdr-pos xdr) -2))
	    (xdr-update-pos xdr 4))
     else
	  (let ((res 
		 (xdr-get-signed-int (xdr-vec xdr) (ash (xdr-pos xdr) -2))))
	    (declare (type (integer #.(- (expt 2 31)) #.(1- (expt 2 31))) res))
	    (xdr-update-pos xdr 4)
	    res)))

(eval-when (compile)
  (defun byteswap-int (value size)
    (let ((res 0))
      (dotimes (n size)
	(setf res (logior (ash res 8) (logand value #xff)))
	(setf value (ash value -8)))
      res)))

(defun xdr-long (xdr &optional int)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (xdr-int xdr int))

(defun xdr-int32 (xdr &optional int)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (xdr-int xdr int))

(defun xdr-short (xdr &optional short)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (xdr-int xdr short))

(defun xdr-char (xdr &optional char)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (xdr-int xdr char))

(define-compiler-macro xdr-long (xdr &optional int)
  `(xdr-int ,xdr ,int))

(define-compiler-macro xdr-int32 (xdr &optional int)
  `(xdr-int ,xdr ,int))

(define-compiler-macro xdr-short (xdr &optional short)
  `(xdr-int ,xdr ,short))

(define-compiler-macro xdr-char (xdr &optional char)
  `(xdr-int ,xdr ,char))


(defun xdr-unsigned-int (xdr &optional int)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if* (eq (xdr-direction xdr) :build)
     then
	  (xdr-int xdr int)
     else 
	  (let ((res (xdr-int xdr nil)))
	    (if* (< res 0)
	       then (+ res #x100000000)
	       else res))))

(define-compiler-macro xdr-unsigned-int (xdr &optional int &whole whole)
  (if* int
     then `(xdr-int ,xdr ,int)
     else whole))

(defun xdr-unsigned (xdr &optional int)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (xdr-unsigned-int xdr int))

(define-compiler-macro xdr-unsigned (xdr &optional int)
  (if* int
     then `(xdr-int ,xdr ,int)
     else `(xdr-unsigned-int ,xdr)))

(defun xdr-uint32 (xdr &optional int)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (xdr-unsigned-int xdr int))

(define-compiler-macro xdr-uint32 (xdr &optional int)
  (if* int
     then `(xdr-int ,xdr ,int)
     else `(xdr-unsigned-int ,xdr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xdr-bool (xdr &optional true)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if* (eq (xdr-direction xdr) :extract)
     then ;; (not (eq x y)) generates more compact code than (/= x y)
	  (not (eq (xdr-int xdr) 0))
     else (xdr-int xdr (if true 1 0))
	  true))

(defun xdr-hyper (xdr &optional int)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if* (eq (xdr-direction xdr) :build)
     then (if (null int)
	      (error "'int' is required when building"))
	  (xdr-int xdr (logand (ash int -32) #xffffffff))
	  (xdr-int xdr (logand int #xffffffff))
     else (logior (ash (xdr-int xdr) 32) (xdr-unsigned-int xdr))))


(defun xdr-unsigned-hyper (xdr &optional int)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (if* (eq (xdr-direction xdr) :build)
     then (xdr-hyper xdr int)
     else (let ((res (xdr-hyper xdr)))
	    (if* (< res 0)
	       then (logand res #xffffffffffffffff)
	       else res))))

(define-compiler-macro xdr-unsigned-hyper (xdr &optional int &whole whole)
  (if* int
     then `(xdr-hyper ,xdr ,int)
     else whole))
    
(defun xdr-uint64 (xdr &optional int)
  (xdr-unsigned-hyper xdr int))

(define-compiler-macro xdr-uint64 (xdr &optional int)
  (if* int
     then `(xdr-hyper ,xdr ,int)
     else `(xdr-unsigned-hyper ,xdr)))

;;;;;;;;;;;;;;;;;;;;;;

(defstruct opaque
  vec
  (offset 0 :type fixnum)
  (len 0 :type fixnum))

(defun opaque-data (o)
  (subseq (opaque-vec o) (opaque-offset o) (+ (opaque-offset o)
					      (opaque-len o))))

(defmacro with-opaque-xdr ((xdr o) &body body)
  "Binds XDR to an xdr suitable for decoding data
   from O (which must be an xdr-opqaue)."
  (let ((offset (gensym))
	(oo (gensym)))
    `(let* ((,oo ,o)
	    (,offset (opaque-offset ,oo))
	    (,xdr 
	     (create-xdr :direction :extract 
			 :vec (opaque-vec ,oo)
			 :size (+ ,offset (the fixnum (opaque-len ,oo))))))
       (declare (optimize (speed 3))
		(fixnum ,offset))
       (setf (xdr-pos ,xdr) ,offset)
       ,@body)))


;; Build:
;;  arg can be a vector, or an opaque struct (in which case 'len' is ignored)
;; Extract:
;;  returns opaque struct
(defun xdr-opaque-fixed (xdr &optional arg len)
  (ecase (xdr-direction xdr)
    (:build
     (let (offset vec plen)
       (if (null arg)
	   (error "xdr-opaque-fixed: 'arg' must be specified when building"))
       (if* (vectorp arg)
	  then (setf vec arg)
	       (setf offset 0)
	       (if (null len)
		   (setf len (length vec)))
	elseif (opaque-p arg)
	  then (setf vec (opaque-vec arg))
	       (setf offset (opaque-offset arg))
	       (setf len (opaque-len arg))
	  else (error "Unexpected value: ~s" arg))
       
       (setf plen (compute-padded-len len))
       (excl::memcpy (xdr-expand-check xdr plen) (xdr-pos xdr)
		     vec offset len)
       (xdr-update-pos xdr plen)))
    (:extract
     (if (null len)
	 (error "xdr-opaque-fixed: 'len' must be specified when extracting"))
	 
     (prog1 
	 (make-opaque :vec (xdr-vec xdr) :offset (xdr-pos xdr) :len len)
       (xdr-advance xdr (compute-padded-len len))))))


;; Build: arg can be a vector, or an opaque struct
;; Extract: returns an opaque struct
(defun xdr-opaque-variable (xdr &optional arg)
  (ecase (xdr-direction xdr)
    (:build
     (if (null arg)
	 (error "xdr-opaque-variable: 'arg' must be specified when building"))
     
     (if* (vectorp arg)
	then (xdr-unsigned-int xdr (length arg))
	     (xdr-opaque-fixed xdr arg)
      elseif (opaque-p arg)
	then (xdr-unsigned-int xdr (opaque-len arg))
	     (xdr-opaque-fixed xdr arg)
	else (error "Unexpected value: ~s" arg)))
    (:extract
     (xdr-opaque-fixed xdr nil (xdr-unsigned-int xdr)))))

;; Returns number of bytes actually read.
(defun xdr-opaque-variable-from-stream (xdr stream count)
  (declare (optimize (speed 3))
	   (xdr xdr)
	   (stream stream)
	   (fixnum count))
  (let (bytesread newpos)
    (declare (fixnum bytesread newpos))
    (with-xdr-seek (xdr 4)
      (xdr-expand-check xdr (compute-padded-len count))
      (let ((pos (xdr-pos xdr)))
	(declare (fixnum pos))
	(setf newpos (read-vector (xdr-vec xdr) stream 
				  :start pos
				  :end (the fixnum (+ pos count))))
	(setf bytesread (the fixnum (- newpos pos)))))
    (xdr-unsigned-int xdr bytesread)
    (xdr-update-pos xdr (compute-padded-len bytesread))
    bytesread))


(defun xdr-array-fixed (xdr typefunc &key len things)
  (declare
   (type xdr xdr)
   (type function typefunc))
  (let (res)
    (ecase (xdr-direction xdr)
      (:extract
       (unless len
	 (error "xdr-array-fixed: 'len' parameter is required"))
       (dotimes (i len)
	 (push (funcall typefunc xdr) res))
       (nreverse res))
      (:build
       (if (not (listp things))
	   (error "xdr-array-fixed: 'things' parameter must be a list"))
       (dolist (thing things)
	 (funcall typefunc xdr thing))))))

(defun xdr-array-variable (xdr typefunc &optional things)
  (declare 
   (type xdr xdr)
   (type function typefunc))
  (let ((direction (xdr-direction xdr))
        len)
    (cond
     ((eq direction :extract)
      (setf len (xdr-int xdr))
      (xdr-array-fixed xdr typefunc :len len))
     ((eq direction :build)
      (setf len (length things))
      (xdr-int xdr len)
      (xdr-array-fixed xdr typefunc :len len :things things)))))

(defun compute-variable-bytes-used (xdr)
  (declare (type xdr xdr))
  ;; variable bytes used is the padded length, plus 4 for the
  ;; size of the length integer.
  (with-xdr-seek (xdr 0)
    (+ 4 (compute-padded-len (xdr-int xdr)))))

(defun xdr-string (xdr &optional string)
  (declare (optimize speed)
	   (type xdr xdr)
	   (simple-string string))

  (let (len plen newstring)
    (ecase (xdr-direction xdr)
      (:extract
       (let ((vec (xdr-vec xdr))
	     pos)
	 (setf len (xdr-unsigned-int xdr))
	 (setf pos (xdr-pos xdr))
	 ;; XXX -- should we impose some limits on the string
	 ;; length?  This is a pretty easy DoS point.
	 (setf newstring (make-string len))
	 ;; FIXME: This could be sped up w/ some ll code.
	 (dotimes (i len)
	   (setf (schar newstring i)
	     (code-char (aref vec (+ i pos)))))
	 (xdr-advance xdr (compute-padded-len len))
	 newstring))
      (:build
       (setf len (length string))
       (setf plen (compute-padded-len len))
       (xdr-unsigned-int xdr len)
       ;; FIXME:  This could be sped up w/ some ll code.
       (let ((vec (xdr-expand-check xdr plen))
	     (pos (xdr-pos xdr)))
	 (declare (type fixnum pos len)
		  (type (simple-array (unsigned-byte 8) (*)) vec)
		  (type simple-string string))
	 (dotimes (i len)
	   (setf (aref vec (+ pos i))
	     (logand #xff (the fixnum (char-code (schar string i))))))
	 (xdr-update-pos xdr plen))))))

(defun xdr-string-utf8 (xdr &optional string)
  (declare (optimize speed)
	   (simple-string string))
  (ecase (xdr-direction xdr)
    (:extract
     (let ((bytes (xdr-unsigned-int xdr))
	   (stringbuf (make-string 512)))
       (declare (dynamic-extent stringbuf))
       (if (> bytes 512)
	   (setf stringbuf (make-string bytes)))
       (let ((chars (user::utf8-to-string (xdr-vec xdr) (xdr-pos xdr) 
					  bytes stringbuf)))
	 (xdr-advance xdr (compute-padded-len bytes))
	 (subseq stringbuf 0 chars))))
    (:build 
     (let* ((strlen (length string))
	    (maxbytes (+ 4 (compute-padded-len (* strlen 3))))
	    (vec (xdr-expand-check xdr maxbytes))
	    bytes)
       (with-xdr-seek (xdr 4)
	 (setf bytes (user::string-to-utf8 string vec (xdr-pos xdr))))
       (xdr-unsigned-int xdr bytes)
       (xdr-update-pos xdr (compute-padded-len bytes))))))

(defun xdr-xdr (xdr &optional xdr2)
  (declare (type xdr xdr))
  (ecase (xdr-direction xdr)
    (:build
     (unless xdr2
       (error "xdr-xdr: 'xdr2' parameter is required"))
     (let* ((size (xdr-size xdr2))
	    (destvec (xdr-expand-check xdr size))
	    (srcvec (xdr-vec xdr2))
	    (pos (xdr-pos xdr)))
       (declare (optimize (speed 3) (safety 0))
		(type (simple-array (unsigned-byte 8) (*)) destvec srcvec)
		(type fixnum pos))
       ;; do the copy....
       (excl::memcpy destvec pos srcvec 0 size)
       (xdr-update-pos xdr size)))
    (:extract
     (cons xdr (xdr-pos xdr)))))
      

(defun xdr-timeval (xdr &optional timeval)
  (declare (type xdr xdr))
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (list (xdr-int xdr) (xdr-int xdr)))
     ((eq direction :build)
      (unless timeval
        (error "xdr-timeval: 'timeval' parameter required"))
      (xdr-int xdr (first timeval))
      (xdr-int xdr (second timeval))))))

;; deprecated.  Only portmap.cl uses this.. and that program
;; needs to be rewritten using rpcgen.
(defun xdr-list (xdr typefunc &key things)
  (ecase (xdr-direction xdr)
    (:extract
     (let (res)
       (while (= (xdr-unsigned-int xdr) 1)
	 (push (funcall typefunc xdr) res))
       (reverse res)))
    (:build
     (dolist (thing things)
       (xdr-unsigned-int xdr 1)
       (funcall typefunc xdr thing))
     (xdr-unsigned-int xdr 0))))

(defun xdr-optional (xdr typefunc &optional arg)
  (ecase (xdr-direction xdr)
    (:build
     (if* arg
	then (xdr-unsigned-int xdr 1)
	     (funcall typefunc xdr arg)
	else (xdr-unsigned-int xdr 0)))
    (:extract
     (when (= 1 (xdr-unsigned-int xdr))
       (funcall typefunc xdr)))))

;;;;;;;;;;;;;;;;;

(defun xdr-prepend-xdr (sym)
  (intern (concatenate 'string (symbol-name 'xdr) "-" (symbol-name sym))))

(defun xdr-struct-accessor (structname slot)
  (intern (concatenate 'string (symbol-name structname) "-"
		       (symbol-name slot))))

(defun xdr-predicate (structname)
  (intern (concatenate 'string (symbol-name structname) "-"
		       (symbol-name 'p))))

(defun xdr-varfixed-extract (type varfixed len)
  (if* (string= (symbol-name type) "opaque")
     then (ecase varfixed
	    (:fixed `(xdr-opaque-fixed xdr nil ,len))
	    (:variable `(xdr-opaque-variable xdr)))
     else (let ((decoder (xdr-prepend-xdr type)))
	    (ecase varfixed
	      (:fixed `(xdr-array-fixed xdr #',decoder :len ,len))
	      (:variable `(xdr-array-variable xdr #',decoder))))))


(defun xdr-varfixed-build (structname obj type slot varfixed)
  (let ((accessor (xdr-struct-accessor structname slot))
	(encoder (xdr-prepend-xdr type)))
    (if* (string= (symbol-name type) "opaque")
       then `(,(ecase varfixed
		 (:fixed 'xdr-opaque-fixed)
		 (:variable 'xdr-opaque-variable))
		 xdr (,accessor ,obj))
       else (ecase varfixed
	      (:fixed
	       `(xdr-array-fixed xdr #',encoder :things (,accessor ,obj)))
	      (:variable
	       `(xdr-array-variable xdr #',encoder (,accessor ,obj)))))))

(defun xdr-extractor-form (type varfixed len)
  (if* varfixed
     then  (xdr-varfixed-extract type varfixed len)
     else `(,(xdr-prepend-xdr type) xdr)))

(defun xdr-builder-form (structname obj type slot varfixed)
  (if varfixed
      (xdr-varfixed-build structname obj type slot varfixed)
    `(,(xdr-prepend-xdr type) xdr (,(xdr-struct-accessor structname slot)
				   ,obj))))

(defmacro defxdrstruct (structname members)
  (let (optional maker extractions builds accessors)
    (when (listp structname)
      (setf optional (second structname))
      (setf structname (first structname)))
    
    (setf maker (intern (concatenate 'string (symbol-name 'make) "-"
				     (symbol-name structname))))
    
    (dolist (member members)
      (let* ((type (first member))
	     (name (second member))
	     (varfixed (third member))
	     (len (fourth member))
	     (slotkeyword (intern name :keyword))
	     (form (xdr-extractor-form type varfixed len)))

	(push (xdr-struct-accessor structname name) accessors)
	(setf extractions (append extractions `(,slotkeyword ,form)))
	(push (xdr-builder-form structname 'arg type name varfixed) builds)))
    
    (setf builds (nreverse builds))
    (setf accessors (nreverse accessors))
    
    `(eval-when (compile load eval)
       (defstruct ,structname
	 ,@(mapcar #'second members))
       
       (defun ,(xdr-prepend-xdr structname) (xdr &optional arg)
	 (ecase (xdr-direction xdr)
	   (:build
	    (if* arg
	       then ,(if* optional
			then '(xdr-int xdr 1))
		    ,@builds
	       else ,(if* optional
			then '(xdr-int xdr 0)
			else '(error "arg must be non-null"))))
	   (:extract
	    (when ,(if* optional
		      then '(= 1 (xdr-int xdr))
		      else t)
	      (,maker 
	       ,@extractions)))))
       
       (export '(,(xdr-prepend-xdr structname) ,maker 
		 ,(xdr-predicate structname)
		 ,@accessors)))))

	       
	       
			   
(defmacro defxdrunion (type discrim arms)
  (let* ((maker (intern (concatenate 'string (symbol-name 'make) "-"
				     (symbol-name type))))
	 (discrim-type (first discrim))
	 (discrim-slot (second discrim))
	 (discrim-accessor (xdr-struct-accessor type discrim-slot))
	 (accessors (list discrim-accessor))
	 (boolean (string= (symbol-name discrim-type) "bool"))
	 slots
	 builds
	 extractions)
    
    (dolist (arm arms)
      (let* ((cases (first arm))
	     (default (eq cases :default))
	     (slot-type (second arm))
	     (slot-name (third arm))
	     (varfixed (fourth arm))
	     (len (fifth arm)))
	
	(when (string/= (symbol-name slot-type) "void")
	  (if (not (listp cases))
	      (setf cases (list cases)))
	  
	  (push slot-name slots)
	  (let ((conditional 
		 (if* default
		    then t
		    else (let (res)
			   (dolist (case cases)
			     (if* boolean
				then (case case
				       (0 
					(push '(null discrim) res))
				       (1 
					(push 'discrim res))
				       (t
					(error "cases must be either #.*true*, #.*false*, or :default when the discriminant type is 'bool")))
				else (push `(= discrim ,case) res)))
			   `(or ,@res)))))

	    (push (xdr-struct-accessor type slot-name) accessors)
	    
	    (push `(,conditional
		    ,(xdr-builder-form type 'arg slot-type slot-name varfixed))
		  builds)
	    (push `(,conditional
		    (setf (,(xdr-struct-accessor type slot-name) res) 
		      ,(xdr-extractor-form slot-type varfixed len)))
		  extractions)))))

    (setf slots (nreverse slots))
    (setf builds (nreverse builds))
    (setf extractions (nreverse extractions))
    
    `(eval-when (compile load eval)
       (defstruct ,type 
	 ,(second discrim)
	 ,@slots)
       
       (defun ,(xdr-prepend-xdr type) (xdr &optional arg)
	 (ecase (xdr-direction xdr)
	   (:build
	    (let ((discrim (,discrim-accessor arg)))
	      (,(xdr-prepend-xdr discrim-type) xdr discrim)
	      (cond
	       ,@builds)))
	   (:extract
	    (let ((discrim (,(xdr-prepend-xdr discrim-type) xdr))
		   (res (,maker)))
	      (setf (,discrim-accessor res) discrim)
	      (cond
	       ,@extractions)
	      res))))

       (export '(,(xdr-prepend-xdr type) ,maker 
		 ,(xdr-predicate type)
		 ,@accessors)))))

(defun xdr-opaque (foo)
  (declare (ignore foo))
  (error "xdr-opaque called."))
