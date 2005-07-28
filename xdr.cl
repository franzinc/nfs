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
;; $Id: xdr.cl,v 1.19 2005/07/28 16:41:41 dancy Exp $

(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

(defstruct xdr
  (vec nil :type (simple-array (unsigned-byte 8) (*)))
  ;; build: maximum offset in vec (not necessarily the length of the vec)
  (size 0 :type fixnum)
  direction
  (pos 0 :type fixnum)) ;; current position in vec (extracting and building)

(defparameter *xdrdefaultsize* 1024)

(eval-when (compile eval load)
  (defmacro with-xdr-seek ((xdr pos &key absolute) &body body)
    (let ((oldpos (gensym)))
      `(let ((,oldpos (xdr-pos ,xdr)))
	 (if ,absolute 
	     (setf (xdr-pos ,xdr) ,pos)
	   (incf (xdr-pos ,xdr) ,pos))
	 (multiple-value-prog1 
	     (progn ,@body)
	   (setf (xdr-pos ,xdr) ,oldpos))))))

(eval-when (compile eval load)
  (defmacro with-xdr-compute-bytes-added ((xdr) &body body)
    (let ((oldpos (gensym)))
      `(let ((,oldpos (xdr-pos ,xdr)))
	 ,@body
	 (- (xdr-pos ,xdr) ,oldpos)))))

(eval-when (compile eval load)
  (defmacro with-xdr-xdr ((pair &key name) &body body)
    (let ((thexdr (gensym))
	  (offset (gensym))
	  (pairval (gensym)))
      (if (null name)
	  (setf name pair))
      `(let* ((,pairval ,pair)
	      (,thexdr (car ,pairval))
	      (,offset (cdr ,pairval))
	      (,name ,thexdr))
	 ;;(logit "seeking to offset ~D~%" ,offset)
	 (with-xdr-seek (,thexdr ,offset :absolute t)
	   ;;(logit "offset is currently ~D~%" (xdr-pos ,thexdr))
	   ,@body)))))

;; used during extraction
(defmacro xdr-advance (xdr size)
  (let ((sizeval (gensym)))
  `(let ((,sizeval ,size))
     (incf (the fixnum (xdr-pos ,xdr)) (the fixnum ,sizeval)))))

;; used during building... bumps the max size (xdr-size) value if 
;; necessary.  Probably should be called xdr-update-size
(defmacro xdr-update-pos (x amt)
  (let ((xdr (gensym))
	(amount (gensym)))
    `(let ((,xdr ,x)
	   (,amount ,amt))
       (declare (optimize (speed 3) (safety 0))
		(type fixnum ,amount)
		(type xdr ,xdr))
       (incf (xdr-pos ,xdr) ,amount)
       (if (> (xdr-pos ,xdr) (xdr-size ,xdr))
	   (setf (xdr-size ,xdr) (xdr-pos ,xdr)))
       nil)))

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
  (declare 
   (type xdr xdr))
  (setf (xdr-size xdr) 0)
  (setf (xdr-pos xdr) 0))

;;; avoid!
(defun xdr-get-vec (xdr)
  (declare
   (type xdr xdr))
  (subseq (xdr-vec xdr) 0 (xdr-size xdr)))

;; Callers need to make sure that they don't have a handle
;; on the 'vec' slot of the xdr before calling this function.  If they
;; do, they may find themselves accessing a vector that is no longer
;; relevant (in case a new vec is created due to expansion).  
;; For convenience, this function returns the most relevant vec.
(defun xdr-expand-check (xdr more)
  (declare
   (optimize (speed 3) (safety 0))
   ;;(:explain :calls :types)
   (type xdr xdr)
   (type fixnum more))
  (when (> (+ (xdr-pos xdr) more) (length (xdr-vec xdr)))
    (setf (xdr-vec xdr)
      (concatenate '(vector (unsigned-byte 8))
	(xdr-vec xdr)
	(make-vec (max *xdrdefaultsize* more)))))
  (xdr-vec xdr))

(defmacro xdr-unroll-extract-from-array (size array offset)
  (let ((shift (* 8 size))
	(arraysym (gensym))
	(offsetsym (gensym))
	body)
    (dotimes (n size)
      (decf shift 8)
      (push `(ash (aref ,arraysym (+ ,offsetsym ,n)) ,shift)
	    body))
    `(let ((,arraysym ,array)
	   (,offsetsym ,offset))
       (declare (optimize (speed 3) (safety 0))
		;;(:explain :types :calls)
		(type fixnum ,offsetsym)
		(type (simple-array (unsigned-byte 8) (*)) ,arraysym))
       (logior ,@body))))

(defmacro xdr-unroll-set-in-array (size value array offset)
  (let ((shift (* -8 size))
	(valuesym (gensym))
	(arraysym (gensym))
	(offsetsym (gensym))
	body)
    (dotimes (n size)
      (incf shift 8)
      (push (if (= shift 0)
		`(setf (aref ,arraysym (+ ,offsetsym ,n)) 
		   (logand #xff ,valuesym))
	      `(setf (aref ,arraysym (+ ,offsetsym ,n)) 
		 (logand #xff (ash ,valuesym ,shift))))
	    body))
    `(let ((,valuesym ,value)
	   (,arraysym ,array)
	   (,offsetsym ,offset))
       (declare (optimize (speed 3) (safety 0))
		;;(:explain :types :calls)
		(type fixnum ,offsetsym)
		(type (simple-array (unsigned-byte 8) (*)) ,arraysym))
       (if (fixnump ,valuesym)
	   (prog ()
	     (declare (type fixnum ,valuesym))
	     ,@body)
	 (progn 
	   ,@body))
       nil)))
	
(defun xdr-unsigned-int (xdr &optional int)
  (declare 
   (type xdr xdr))
  (ecase (xdr-direction xdr)
    (:extract
     (prog1
	 (xdr-unroll-extract-from-array 4 (xdr-vec xdr) (xdr-pos xdr))
       (xdr-advance xdr 4)))
    (:build
     (let ((vec (xdr-expand-check xdr 4)))
       (xdr-unroll-set-in-array 4 int vec (xdr-pos xdr))
       (xdr-update-pos xdr 4)))))

(defun xdr-int (xdr &optional int)
  (declare
   (type xdr xdr))
  (ecase (xdr-direction xdr)
    (:extract
     (let ((res (xdr-unsigned-int xdr)))
       (if (/= 0 (logand res #.(ash 1 31)))
	   (- res #.(ash 1 32))
	 res)))
    (:build
     (let ((vec (xdr-expand-check xdr 4)))
       (xdr-unroll-set-in-array 4 int vec (xdr-pos xdr))
       (xdr-update-pos xdr 4)))))

(eval-when (compile load eval)
(defun xdr-signed-unsigned-int-compiler-macro (xdr int env whole)
  ;; If 'int' is provided, must be building
  (if (and int (constantp int))
      (let ((value #-(version>= 7 1)(excl::constant-value int env)
		   #+(version>= 7 1)(sys:constant-value int env)))
	(if (numberp value)
	    (let ((b0 (logand #xff (ash value -24)))
		  (b1 (logand #xff (ash value -16)))
		  (b2 (logand #xff (ash value -8)))
		  (b3 (logand #xff value))
		  (xdrsym (gensym))
		  (pos (gensym))
		  (vec (gensym)))
	      `(let* ((,xdrsym ,xdr)
		      (,vec (xdr-expand-check ,xdrsym 4))
		      (,pos (xdr-pos ,xdrsym)))
		 (declare (optimize (speed 3) (safety 0))
			  (type fixnum ,pos)
			  (type (simple-array (unsigned-byte 8) (*)) ,vec))
		 (setf (aref ,vec ,pos) ,b0)
		 (setf (aref ,vec (+ 1 ,pos)) ,b1)
		 (setf (aref ,vec (+ 2 ,pos)) ,b2)
		 (setf (aref ,vec (+ 3 ,pos)) ,b3)
		 (xdr-update-pos ,xdrsym 4)))
	  whole))
    whole)))

(define-compiler-macro xdr-unsigned-int 
    (xdr &optional int &environment env &whole whole)
  (xdr-signed-unsigned-int-compiler-macro xdr int env whole))

(define-compiler-macro xdr-int 
    (xdr &optional int &environment env &whole whole)
  (xdr-signed-unsigned-int-compiler-macro xdr int env whole))

(defun xdr-bool (xdr &optional true)
  (ecase (xdr-direction xdr)
    (:extract
     (/= (xdr-unsigned-int xdr) 0))
    (:build
     (xdr-unsigned-int xdr (if true 1 0))
     true)))

(defun xdr-unsigned-hyper (xdr &optional int)
  (declare 
   (type xdr xdr))
  (ecase (xdr-direction xdr)
    (:extract
     (prog1
	 (xdr-unroll-extract-from-array 8 (xdr-vec xdr) (xdr-pos xdr))
       (xdr-advance xdr 8)))
    (:build
     (let ((vec (xdr-expand-check xdr 8)))
       (xdr-unroll-set-in-array 8 int vec (xdr-pos xdr))
       (xdr-update-pos xdr 8)))))

(defun xdr-hyper (xdr &optional int)
  (declare
   (type xdr xdr))
  (ecase (xdr-direction xdr)
    (:extract
     (let ((res (xdr-unsigned-hyper xdr)))
       (if (/= 0 (logand res #.(ash 1 63)))
	   (- res #.(ash 1 64))
	 res)))
    (:build
     (let ((vec (xdr-expand-check xdr 8)))
       (xdr-unroll-set-in-array 8 int vec (xdr-pos xdr))
       (pprint vec)
       (xdr-update-pos xdr 8)))))



;; If make-vec is true, return a new vector.
;; Otherwise, return enough information to efficiently
;; get that the data from the xdr vector later.
(defun xdr-opaque-fixed (xdr &key vec len make-vec)
  (declare
   (type xdr xdr))
  (ecase (xdr-direction xdr)
    (:extract
     (unless len
       (error "xdr-opaque-fixed: 'len' parameter is required"))
     (prog1 
	 (if make-vec
	     (subseq (xdr-vec xdr) (xdr-pos xdr) (+ (xdr-pos xdr) len))
	   (list xdr (xdr-pos xdr) len))
       (xdr-advance xdr (compute-padded-len len))))
    (:build
     (unless vec
       (error "xdr-opaque-fixed: 'vec' parameter is required"))
     (unless len
       (setf len (length vec)))
     (let* ((plen (compute-padded-len len))
	    (destvec (xdr-expand-check xdr plen))
	    (pos (xdr-pos xdr)))
       (declare ;;(:explain :calls :types)
		(optimize (speed 3) (safety 0))
		(type (simple-array (unsigned-byte 8) (*)) vec destvec))
       (dotimes (i len)
	 (declare (fixnum i))
	 (setf (aref destvec (+ i pos)) (aref vec i)))
       (xdr-update-pos xdr plen)))))


 ;;; extract:  returns (xdr offset length)
(defun xdr-opaque-variable (xdr &key vec len make-vec)
  (declare 
   (type xdr xdr))
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (xdr-opaque-fixed xdr :len (xdr-int xdr) :make-vec make-vec))
     ((eq direction :build)
      (unless vec
        (error "xdr-opaque-variable: 'vec' parameter is required"))
      (unless len
	(setf len (length vec)))
      (xdr-int xdr len)
      (xdr-opaque-fixed xdr :vec vec :len len)))))

;; Returns number of bytes actually read.
(defun xdr-opaque-variable-from-stream (xdr stream count)
  (declare
   (type xdr xdr)
   (type stream stream)
   (type fixnum count))
  (unless (eq (xdr-direction xdr) :build)
    (error "xdr-opaque-variable-from-stream is only good for building"))
  (let (bytesread newpos)
    (with-xdr-seek (xdr 4)
      (xdr-expand-check xdr (compute-padded-len count))
      (setf newpos (read-vector (xdr-vec xdr) stream 
				:start (xdr-pos xdr)
				:end (+ (xdr-pos xdr) count)))
      (setf bytesread (- newpos (xdr-pos xdr))))
    (xdr-unsigned-int xdr bytesread)
    (xdr-update-pos xdr (compute-padded-len bytesread))
    bytesread))


(defun xdr-array-fixed (xdr typefunc &key len things)
  (declare
   (type xdr xdr)
   (type function typefunc))
  (let ((direction (xdr-direction xdr))
        res)
    (cond
     ((eq direction :extract)
      (unless len
        (error "xdr-array-fixed: 'len' parameter is required"))
      (dotimes (i len)
        (push (funcall typefunc xdr) res))
      (reverse res))
     ((eq direction :build)
      (unless (consp things)
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


;; auth_flavor: AUTH_NULL = 0 AUTH_UNIX = 1 AUTH_SHORT = 2 AUTH_DES =
;; 3

(defstruct opaque-auth
  (flavor :type fixnum) ;; int
  body-xdr
  (body-offset :type fixnum))

(defun xdr-opaque-auth (xdr &optional flavor body)
  (declare
   (type xdr xdr))
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (let ((oa (make-opaque-auth)))
	;;(logit "xdr-opaque-auth:  current xdr pos is ~D~%" (xdr-pos xdr))
        (setf (opaque-auth-flavor oa) (xdr-int xdr))
	;;(logit "xdr-opaque-auth: auth-flavor is ~D~%" (opaque-auth-flavor oa))
	(setf (opaque-auth-body-xdr oa) xdr)
	(setf (opaque-auth-body-offset oa) (+ 4 (xdr-pos xdr))) ;; add 4 to skip the integer which contains the size of the body
	(xdr-advance xdr (compute-variable-bytes-used xdr))
	;;(logit "xdr-opaque-auth: new xdr pos is ~D~%" (xdr-pos xdr))
        oa))
     ((eq direction :build)
      (if (null body)
	  (error "xdr-opaque-auth: 'body' keyword arg required when building"))
      (xdr-int xdr flavor)
      (xdr-opaque-variable xdr :vec body)))))

(defun xdr-auth-null (xdr)
  (declare 
   (type xdr xdr))
  (unless (eq (xdr-direction xdr) :build)
    (error "xdr-auth-null: Not ready for extraction yet"))
  (xdr-opaque-auth xdr 0 #()))

(defstruct auth-unix 
  stamp
  machinename ;; string machinename<255>
  uid
  gid
  gids ;; unsigned int gids<16>
  ) 

(defun xdr-auth-unix (xdr &optional au)
  (declare 
   (type xdr xdr))
  (let ((direction (xdr-direction xdr)))
    (cond 
     ((eq direction :extract)
      (let* ((stamp (xdr-int xdr))
             (machinename (xdr-string xdr))
             (uid (xdr-int xdr))
             (gid (xdr-int xdr))
             (gids (xdr-array-variable xdr #'xdr-int))
             (newau (make-auth-unix
                     :stamp stamp
                     :machinename machinename
                     :uid uid
                     :gid gid
                     :gids gids)))
        newau))
     ((eq direction :build)
      (if (null au)
	  (error "xdr-auth-unix: 'au' keyword arg required when building"))
      (xdr-int xdr (auth-unix-stamp au))
      (xdr-string xdr (auth-unix-machinename au))
      (xdr-int xdr (auth-unix-uid au))
      (xdr-int xdr (auth-unix-gid au))
      (xdr-array-variable xdr #'xdr-int (auth-unix-gids au))))))

(defun xdr-opaque-auth-struct-to-auth-unix-struct (oa)
  (let ((xdr (opaque-auth-body-xdr oa))
	(au (make-auth-unix))
	(pos (opaque-auth-body-offset oa)))
    (declare
     (type xdr xdr)
     (type auth-unix au)
     (type fixnum pos))
    (with-xdr-seek (xdr pos :absolute t)
      ;;(logit "seeked to position ~D within xdr~%" pos)
      (setf (auth-unix-stamp au) (xdr-int xdr))
      ;;(logit "stamp is ~D~%" (auth-unix-stamp au))
      (setf (auth-unix-machinename au) (xdr-string xdr))
      ;;(logit "machine name is ~A~%" (auth-unix-machinename au))
      (setf (auth-unix-uid au) (xdr-int xdr))
      (setf (auth-unix-gid au) (xdr-int xdr))
      (setf (auth-unix-gids au)
	(xdr-array-variable xdr #'xdr-int)))
    au))



(defun compute-padded-len (len)
  (declare
   ;;(:explain :calls :types)
   (type fixnum len))
  (let ((remainder (mod len 4)))
    (declare 
     (type (integer 0 3) remainder))
    (+ len (if (> remainder 0) (- 4 remainder) 0))))

(defun compute-variable-bytes-used (xdr)
  (declare (type xdr xdr))
  ;; variable bytes used is the padded length, plus 4 for the
  ;; size of the length integer.
  (with-xdr-seek (xdr 0)
    (+ 4 (compute-padded-len (xdr-int xdr)))))

(defun xdr-string (xdr &optional string)
  (declare (type xdr xdr)
	   (optimize (speed 3)))
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
	 (dotimes (i len)
	   (setf (schar newstring i)
	     (code-char (aref vec (+ i pos)))))
	 (xdr-advance xdr (compute-padded-len len))
	 newstring))
      (:build
       (setf len (length string))
       (setf plen (compute-padded-len len))
       (xdr-unsigned-int xdr len)
       (let ((vec (xdr-expand-check xdr plen))
	     (pos (xdr-pos xdr)))
	 (declare (type fixnum pos len)
		  (type (simple-array (unsigned-byte 8) (*)) vec)
		  (type simple-string string))
	 (dotimes (i len)
	   (setf (aref vec (+ pos i))
	     (logand #xff (the fixnum (char-code (schar string i))))))
	 (xdr-update-pos xdr plen))))))

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
       (dotimes (i size)
	 (setf (aref destvec (+ i pos))
	   (aref srcvec i)))
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

;;;;;;;;;;;;;;;;;

(defun xdr-prepend-xdr (sym)
  (intern (concatenate 'string (symbol-name 'xdr) "-" (symbol-name sym))))

(defun xdr-struct-accessor (structname slot)
  (intern (concatenate 'string (symbol-name structname) "-"
		       (symbol-name slot))))

(defmacro defxdrstruct (structname members)
  (let* ((maker (intern (concatenate 'string (symbol-name 'make) "-"
				     (symbol-name structname))))
	 extractions)
    (dolist (member members)
      (let ((type (first member))
	    (member (second member)))
	(setf extractions (append extractions
				  (list (intern member :keyword) 
					`(,(xdr-prepend-xdr type) xdr))))))
      
    `(progn
       (defstruct ,structname
	 ,@(mapcar #'second members))
       
       (defun ,(xdr-prepend-xdr structname) (xdr &optional arg)
	 (ecase (xdr-direction xdr)
	   (:build
	    ,@(mapcar #'(lambda (member)
			  (let* ((type (first member))
				 (member (second member))
				 (accessor (xdr-struct-accessor structname member)))
			    `(,(xdr-prepend-xdr type) xdr (,accessor arg))))
		      members))
	   (:extract
	    (,maker 
	     ,@extractions)))))))
			   
(defmacro defxdrunion (type discrim arms)
  (let* ((maker (intern (concatenate 'string (symbol-name 'make) "-"
				     (symbol-name type))))
	 (discrim-type (first discrim))
	 (discrim-slot (second discrim))
	 (discrim-accessor (xdr-struct-accessor type discrim-slot))
	 slots
	 builds
	 extractions)
    (dolist (arm arms)
      (let ((case (first arm))
	    (slot-type (second arm))
	    (slot-name (third arm)))
	(push slot-name slots)
	(push `((eql discrim ,case)
		(,(xdr-prepend-xdr slot-type) 
		 xdr (,(xdr-struct-accessor type slot-name) arg)))
	      builds)
	(push `((eql discrim ,case)
		(setf (,(xdr-struct-accessor type slot-name) res) 
		  (,(xdr-prepend-xdr slot-type) xdr)))
	      extractions)))
    
    (setf slots (nreverse slots))
    (setf builds (nreverse builds))
    (setf extractions (nreverse extractions))
    
    `(progn
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
	    (let* ((discrim (,(xdr-prepend-xdr discrim-type) xdr))
		   (res (,maker)))
	      (setf (,discrim-accessor res) discrim)
	      (cond
	       ,@extractions)
	      res)))))))
	    
