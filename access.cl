;; $Id: access.cl,v 1.1 2003/01/20 23:47:25 dancy Exp $

(in-package :user)

(defparameter *hosts-allow* nil)
(defparameter *hosts-deny* nil)
(defparameter *hosts-allow-parsed* nil)
(defparameter *hosts-deny-parsed* nil)

(defun access-allowed-p (addr)
  (dolist (allow *hosts-allow-parsed*)
    (if (addr-in-network-p addr allow)
	(return-from access-allowed-p t)))
  (dolist (deny *hosts-deny-parsed*)
    (if (addr-in-network-p addr deny)
	(return-from access-allowed-p nil)))
  t)
    
    