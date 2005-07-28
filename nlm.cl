;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2005 Franz Inc, Oakland, CA.  All rights reserved.
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
;; $Id: nlm.cl,v 1.1 2005/07/28 16:41:41 dancy Exp $

(in-package :user)

;; This file implements the Network Lock Monitor (NLM) protocol. 

;; Ref: http://www.opengroup.org/onlinepubs/009629799/chap9.htm

(eval-when (compile load eval)
  (defconstant *nlm-vers* 3)
  (defconstant *nlm-prog* 100021)
  
  (defconstant *nlm-maxnetobj-sz 1024)

  ;; enum nlm_stats 
  (defconstant *lck-granted* 0)
  (defconstant *lck-denied* 1)
  (defconstant *lck-denied-nolocks* 2)
  (defconstant *lck-blocked* 3)
  (defconstant *lck-denied-grace-period* 4)
  
  ;;procedures
  (defconstant *nlm-null* 0)
  (defconstant *nlm-test* 1)
  (defconstant *nlm-lock* 2)
  (defconstant *nlm-cancel* 3)
  (defconstant *nlm-unlock* 4)
  (defconstant *nlm-granted* 5)
  (defconstant *nlm-test-msg* 6)
  (defconstant *nlm-lock-msg* 7)
  (defconstant *nlm-cancel-msg* 8)
  (defconstant *nlm-unlock-msg* 9)
  (defconstant *nlm-granted-msg* 10)
  (defconstant *nlm-test-res* 11)
  (defconstant *nlm-lock-res* 12)
  (defconstant *nlm-cancel-res* 13)
  (defconstant *nlm-unlock-res* 14)
  (defconstant *nlm-granted-res* 15)
  (defconstant *nlm-share* 20)
  (defconstant *nlm-unshare* 21)
  (defconstant *nlm-nm-lock* 22)
  (defconstant *nlm-free-all* 23)
  )

(defun xdr-nlm-netobj (xdr &optional vec)
  (ecase (xdr-direction xdr)
    (:build
     (xdr-opaque-variable xdr :vec vec))
    (:extract
     (xdr-opaque-variable xdr :make-vec t))))

(defmacro xdr-nlm-stat (&rest rest) 
  `(xdr-int ,@rest))

(defxdrstruct nlm-res ((nlm-netobj cookie) 
		       (nlm-stat stat)))

(defxdrstruct nlm-holder ((bool exclusive)
			  (int uppid)
			  (nlm-netobj oh)
			  (unsigned-int offset)
			  (unsigned-int len)))

(defxdrunion nlm-testrply (nlm-stat stat)
  ((*lck-denied* nlm-holder holder)))

(defxdrstruct nlm-testres ((nlm-netobj cookie)
			   (nlm-testrply test-stat)))

(defxdrstruct nlm-lock  ((string caller-name)
			 (nlm-netobj fh)
			 (nlm-netobj oh)
			 (int uppid)
			 (unsigned-int offset)
			 (unsigned-int len)))

(defxdrstruct nlm-lockargs ((nlm-netobj cookie)
			    (bool block)
			    (bool exclusive)
			    (nlm-lock alock)
			    (bool reclaim)
			    (int state)))

(defxdrstruct nlm-cancargs ((nlm-netobj cookie)
			    (bool block)
			    (bool exclusive)
			    (nlm-lock alock)))

(defxdrstruct nlm-testargs ((nlm-netobj cookie)
			    (bool exclusive)
			    (nlm-lock alock)))

(defxdrstruct nlm-unlockargs ((nlm-netobj cookie)
			      (nlm-lock alock)))





      


  
