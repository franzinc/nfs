;; This software is Copyright (c) Franz Inc., 2001-2010.
;; Franz Inc. grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3))))

(defstruct network-address
  network
  mask)

;; Acceptable formats:
;; a.b.c.d
;; a.b.c.d/x
;; a.b.c.d/x.y.z.w
;; t  (shortcut for 0.0.0.0/0)
(defun parse-addr (addr)
  ;; convenience
  (if (eq addr t)
      (setf addr "0.0.0.0/0"))
  (setf addr (string-trim '(#\space) addr))
  (let* ((slashpos (position #\/ addr))
	 (mask #xffffffff)
	 (network (socket:dotted-to-ipaddr 
		   (subseq addr 0 (or slashpos (length addr))))))
    (if* slashpos
       then
	    (setf addr (subseq addr (1+ slashpos)))
	    (setf mask 
	      (if (position #\. addr)
		  (socket:dotted-to-ipaddr addr)
		(masklength-to-mask addr)))
	    (setf network (logand network mask)))
    (make-network-address
     :network network
     :mask mask)))

(defun masklength-to-mask (value)
  (if (stringp value)
      (setf value (parse-integer value)))
  (if (or (< value 0) (> value 32))
      (error "Invalid mask length: ~A" value))
  (- #xffffffff (1- (expt 2 (- 32 value)))))

(defun test (net)
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((addr (socket:dotted-to-ipaddr "1.2.3.4")))
    (dotimes (n 10000000)
      (addr-in-network-p addr net))))

#+ignore
(defun addr-in-network-p (addr net)
  (declare (optimize speed (safety 0) (debug 0)))
  (if (stringp addr)
      (setf addr (socket:dotted-to-ipaddr addr)))
  (= (logand addr (network-address-mask net))
     (network-address-network net)))

;; Optimized version
(defun addr-in-network-p (addr net)
  (declare (optimize speed (safety 0) (debug 0)))
  (if (stringp addr)
      (setf addr (socket:dotted-to-ipaddr addr)))
  (let ((addr (comp::ll :integer-to-mi addr))
	(mask (comp::ll :integer-to-mi (network-address-mask net)))
	(network (comp::ll :integer-to-mi (network-address-network net))))
    (eq (comp::ll :logand addr mask) network)))

