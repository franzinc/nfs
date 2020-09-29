;; -*- mode: common-lisp -*-
;; See the file LICENSE for the full license governing this code.

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
