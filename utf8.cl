;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2010 Franz Inc, Oakland, CA.  All rights reserved.
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

(defmacro mi-to-fixnum (value)
  `(comp::ll :mi-to-fixnum ,value))

(defmacro mi (value)
  `(comp::ll :fixnum-to-mi ,value))

(defmacro mi-incf (var &optional (amt 1))
  `(setf ,var (comp::ll :+ ,var (mi ,amt))))

(defmacro mi-= (var const)
  `(comp::ll := ,var (mi ,const)))

(defmacro mi-<= (var const)
  `(comp::ll :<= ,var (mi ,const)))

(defmacro mi-sub (expr1 expr2)
  `(comp::ll :- ,expr1 ,expr2))

(defmacro mi-or (expr1 expr2 &rest exprs)
  (if (constantp expr1)
      (setf expr1 `(mi ,expr1)))
  (if (constantp expr2)
      (setf expr2 `(mi ,expr2)))
  (if* (zerop (length exprs))
     then `(comp::ll :logior ,expr1 ,expr2)
     else `(mi-or (comp::ll :logior ,expr1 ,expr2) ,@exprs)))

(defmacro mi-and (expr1 expr2)
  (if (constantp expr1)
      (setf expr1 `(mi ,expr1)))
  (if (constantp expr2)
      (setf expr2 `(mi ,expr2)))
  `(comp::ll :logand ,expr1 ,expr2))

(defmacro mi-lsr (value amount)
  `(comp::ll :lsr ,value (mi ,amount)))

(defmacro mi-lsl (value amount)
  `(comp::ll :lsl ,value (mi ,amount)))

(defmacro aref-ubyte-vec (vec)
  `(comp::ll :aref-ubyte ,vec (mi #.(sys::mdparam 'comp::md-lvector-data0-norm))))

(defmacro aref-uword-vec (vec)
  `(comp::ll :aref-uword ,vec (mi #.(sys::mdparam 'comp::md-lvector-data0-norm))))

(defmacro aset-byte-vec (vec value)
  `(comp::ll :aset-byte ,vec (mi #.(sys::mdparam 'comp::md-lvector-data0-norm)) ,value))

(defmacro aset-word-vec (vec value)
  `(comp::ll :aset-word ,vec (mi #.(sys::mdparam 'comp::md-lvector-data0-norm)) ,value))

;; Expects 16-bit chars
;; Returns number of bytes encoded.
(defun string-to-utf8 (string vec pos)
  (declare (optimize (speed 3) (safety 0))
	   (simple-string string)
	   ((simple-array (unsigned-byte 8) (*)) vec))
  
  (let ((remaining (length string)))
    (declare (fixnum remaining))

    (mi-incf vec pos)
    
    (let ((orig-vec vec))
    
      (while (not (zerop remaining))
	(macrolet ((put (value)
		     `(progn (aset-byte-vec vec ,value)
			     (mi-incf vec))))
	  (let ((code (aref-uword-vec string)))
	    (mi-incf string 2)
	    (decf remaining)
	    (if* (mi-<= code #x7f)
	       then ;; simple-ascii
		    (put code)
	     elseif (mi-<= code #x7ff)
	       then ;; two byte encoding
		    (put (mi-or #xc0 (mi-lsr code 6)))
		    (put (mi-or #x80 (mi-and #x3f code)))
	       else ;; three byte encoding
		    (put (mi-or #xe0 (mi-lsr code 12)))
		    (put (mi-or #x80 (mi-and #x3f (mi-lsr code 6))))
		    (put (mi-or #x80 (mi-and #x3f code)))))))
      
      (mi-to-fixnum (mi-sub vec orig-vec)))))

;; Returns # of characters decoded.
(defun utf8-to-string (vec start len out)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum len))
  
  (mi-incf vec start)
  
  (let ((orig-out out))
  
    (while (not (zerop len))
      (macrolet ((nextbyte ()
		   `(prog1 (aref-ubyte-vec vec)
		      (mi-incf vec)
		      (decf len)))
		 (outchar (code)
		   `(progn
		      (aset-word-vec out ,code)
		      (mi-incf out 2)))
		 (lowsix (value)
		   `(mi-and #x3f ,value)))
	(let ((b (nextbyte)))
	  (if* (mi-<= b #x7f)
	     then (outchar b)
	   elseif (mi-= (mi-and #xe0 b) #xc0)
	     then ;; 2 byte encoding
		  (outchar (mi-or (mi-lsl (mi-and #b11111 b) 6)
				  (lowsix (nextbyte))))
	     else ;; 3 byte encoding
		  (outchar (mi-or
			    (mi-lsl (mi-and b #xf) 12)
			    (mi-lsl (lowsix (nextbyte)) 6)
			    (lowsix (nextbyte))))))))
    
    (mi-to-fixnum (mi-lsr (mi-sub out orig-out) 1))))
    

  
  


