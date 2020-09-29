;; -*- mode: common-lisp -*-
;; See the file LICENSE for the full license governing this code.

(defpackage :interval
  (:use :lisp :excl)
  (:export 
   #:begins-before-p
   #:begins-after-p
   #:ends-after-p
   #:begins-within-p
   #:ends-within-p
   #:overlaps-p
   #:interval-subtract
   #:interval-subtract-pairs))
  	
(in-package :interval)

(defmacro begins-before-p (start1 end1 start2 end2)
  (declare (ignore end1 end2))
  `(< ,start2 ,start1))

(defmacro begins-after-p (start1 end1 start2 end2)
  (declare (ignore start1 end2))
  `(>= ,start2 ,end1))

;; interval2 ends after interval 1 ends
(defmacro ends-after-p (start1 end1 start2 end2)
  (declare (ignore start1 start2))
  `(> ,end2 ,end1))

(defmacro begins-within-p (start1 end1 start2 end2)
  (declare (ignore end2))
  `(and (>= ,start2 ,start1) (< ,start2 ,end1)))
  
(defmacro ends-within-p (start1 end1 start2 end2)
  (declare (ignore start2))
  `(and (> ,end2 ,start1) (<= ,end2 ,end1)))

(defmacro overlaps-p (start1 end1 start2 end2)
  `(or (begins-within-p ,start1 ,end1 ,start2 ,end2)
       (ends-within-p ,start1 ,end1 ,start2 ,end2)
       (and (begins-before-p ,start1 ,end1 ,start2 ,end2)
	    (ends-after-p ,start1 ,end1 ,start2 ,end2))))

(defun interval-subtract-1 (start1 end1 start2 end2)
  (cond   
   ((not (overlaps-p start1 end1 start2 end2))
    (values start1 end1))
   
   ((begins-before-p start1 end1 start2 end2)
    (if* (ends-within-p start1 end1 start2 end2)
       then (values end2 end1)
       else nil))
   
   (t ;; begins within
    (if* (ends-within-p start1 end1 start2 end2)
       then (values start1 start2 end2 end1)
       else (values start1 start2)))))

(defun interval-subtract (start1 end1 start2 end2)
  (declare (optimize (speed 3)))
  (multiple-value-bind (a b c d)
      (interval-subtract-1 start1 end1 start2 end2)
    (if (null a)
	(return-from interval-subtract))
    (if* (and c (= c d))
       then (setf c nil)
	    (setf d nil))
    (if* (= a b)
       then (setf a c)
	    (setf b d)
	    (setf c nil)
	    (setf d nil))
    (values a b c d)))

;; Return a list of the remaining intervals (in cons form)
(defun interval-subtract-pairs (start end pairs)
  (dolist (pair pairs)
    (multiple-value-bind (nstart1 nend1 nstart2 nend2)
	(interval-subtract start end (car pair) (cdr pair))
      (if* nstart2
	 then (return-from interval-subtract-pairs 
		(nconc (interval-subtract-pairs nstart1 nend1 pairs)
		       (interval-subtract-pairs nstart2 nend2 pairs)))
       elseif nstart1
	 then (setf start nstart1)
					(setf end nend1)
	 else (return-from interval-subtract-pairs nil))))
  (list (cons start end)))

  
