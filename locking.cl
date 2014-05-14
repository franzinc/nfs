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

;; Better locking interface than the MSVCRT _locking

(ff:def-foreign-call _get_osfhandle ((fd :int)))

(ff:def-foreign-call LockFile ((handle :int) 
			       (offsetlow :unsigned-int) 
			       (offsethigh :unsigned-int) 
			       (countlow :unsigned-int) 
			       (counthigh :unsigned-int)) 
  :error-value :os-specific)

(ff:def-foreign-call UnlockFile ((handle :int) 
				 (offsetlow :unsigned-int) 
				 (offsethigh :unsigned-int) 
				 (countlow :unsigned-int) 
				 (counthigh :unsigned-int)) 
  :error-value :os-specific)

(defconstant ERROR_LOCK_VIOLATION 33)
(defconstant ERROR_NOT_LOCKED 158)

;; Returns 't' if successful, nil if already locked, and 
;; signals an error otherwise.
(defun my-lock-file (stream offset length)
  (multiple-value-bind (status err)
      (LockFile (_get_osfhandle (excl.osi::stream-to-fd stream))
		(logand offset #xffffffff)
		(logand (ash offset -32) #xffffffff)
		(logand length #xffffffff)
		(logand (ash length -32) #xffffffff))
    (if* (zerop status)
       then ;; Something didn't work out
	    (if* (= err ERROR_LOCK_VIOLATION)
	       then nil
	       else (error "Lockfile failed with code ~d" err))
       else t)))

;; Returns 't' if successful, 'nil' if Windows claims that the
;; region wasn't lock before. 
;; signals an error otherwise
(defun my-unlock-file (stream offset length)
  (multiple-value-bind (status err)
      (UnlockFile (_get_osfhandle (excl.osi::stream-to-fd stream))
		  (logand offset #xffffffff)
		  (logand (ash offset -32) #xffffffff)
		  (logand length #xffffffff)
		  (logand (ash length -32) #xffffffff))
    (if* (zerop status)
       then ;; Something didn't work out
	    (if* (= err ERROR_NOT_LOCKED)
	       then nil
	       else (error "Unlockfile failed with code ~d" err))
       else t)))
