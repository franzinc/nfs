;; This software is Copyright (c) Franz Inc., 2001-2014.
;; Franz Inc. grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3))))

(eval-when (compile load eval)
  (require :acldns)
  (require :regexp2)
  (require :sock))

(defstruct network-address
  network
  mask)

(eval-when (compile eval load)
  (defvar *ipaddr-re* "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+"))

(defun valid-ipaddr-p (thing &key full)
  (if* full
     then (match-re
	   #.(concatenate 'simple-string
	       "^" *ipaddr-re* "/"
	       "(" *ipaddr-re* "|\\d+)"
	       "$")
	   thing)
     else (match-re
	   #.(concatenate 'simple-string
	       "^" *ipaddr-re* "$")
	   thing)))

(defun my-dotted-to-ipaddr (addr)
  (if* (not (valid-ipaddr-p addr))
     then (error "Invalid address specification")
     else (socket:dotted-to-ipaddr addr)))

;; Acceptable formats:
;; a.b.c.d
;; a.b.c.d/x
;; a.b.c.d/x.y.z.w
;; t  (shortcut for 0.0.0.0/0)

(defun parse-addr (addr &aux (mask #xffffffff))
  ;; convenience
  (if (eq addr t)
      (setf addr "0.0.0.0/0"))
  (setf addr (string-trim '(#\space) addr))
  (if (string= addr "")
      (error "blank string passed to parse-addr"))
  (if* (valid-ipaddr-p addr :full t)
     then (let* ((slashpos (position #\/ addr))
		 (network (my-dotted-to-ipaddr 
			   (subseq addr 0 (or slashpos (length addr))))))
	    (if* slashpos
	       then (setf addr (subseq addr (1+ slashpos)))
		    (setf mask 
		      (if (position #\. addr)
			  (my-dotted-to-ipaddr addr)
			(masklength-to-mask addr)))
		    (setf network (logand network mask)))
	    (make-network-address
	     :network network
	     :mask mask))
     else ;; Assume it's a host name and try to resolve it
	  (let ((ip (ignore-errors (socket:lookup-hostname addr))))
	    (when (null ip)
	      (error "Could not resolve host name ~s." addr))
	    (make-network-address
	     :network ip
	     :mask mask))))

(defun masklength-to-mask (value)
  (if (stringp value)
      (setf value (parse-integer value)))
  (if (or (< value 0) (> value 32))
      (error "Invalid mask length: ~A" value))
  (- #xffffffff (1- (expt 2 (- 32 value)))))

#+ignore
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

(defun network-address-to-printable-string (network-address)
  (let ((mask (network-address-mask network-address))
        (addr (socket:ipaddr-to-dotted 
                          (network-address-network network-address))))
    (cond ((and (integerp mask)(= mask 0)
		(stringp addr) (string= addr "0.0.0.0"))
           (setf mask nil
		 addr "*"))
	  ((= #xffffffff mask) 
           (setf mask nil)
           (let ((name (socket:ipaddr-to-hostname 
			(network-address-network network-address))))
             (when name 
               (setf addr name))))
          (t (setf mask (socket:ipaddr-to-dotted mask))))
    (format nil "~A~@[/~A~]" addr mask)))	
