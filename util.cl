;; $Id: util.cl,v 1.5 2001/06/07 17:14:05 dancy Exp $

(in-package :user)

(ff:def-foreign-type stat
    (:struct
     (st_dev :unsigned-int)  
     (st_ino :unsigned-short)  
     (st_mode :unsigned-short)
     (st_nlink :short)
     (st_uid :short)
     (st_gid :short)
     (st_rdev :unsigned-int) 
     (st_size :int) 
     (st_atime :int) 
     (st_mtime :int)
     (st_ctime :int)))     

(ff:def-foreign-call (stat "_stat") ((filename (* :char) string)
                                     (statbuf (* stat)))
  :strings-convert t
  )

(ff:def-foreign-type intholder
    (:struct
     (value :int)))

(ff:def-foreign-type large-integer 
    (:struct
     (lowpart :int)
     (highpart :int)))

(ff:def-foreign-call GetDiskFreeSpaceExA ((filename :foreign-address)
					  (freebytes :foreign-address)
					  (totalbytes :foreign-address)
					  (realfreebytes :foreign-address)))

  
(defun diskfree (root)
  (with-native-string (nativestring (namestring root))
    (let ((freebytes (ff:allocate-fobject 'large-integer))
	  (totalbytes (ff:allocate-fobject 'large-integer))
	  res)
      (setf res (GetDiskFreeSpaceExA nativestring freebytes totalbytes 0))
      (when (= 0 res)
	(error "Call to GetDiskFreeSpaceExA failed"))
      (values 
       (+ (ash (ff:fslot-value freebytes :highpart) 32)
	  (ff:fslot-value freebytes :lowpart))
       (+ (ash (ff:fslot-value totalbytes :highpart) 32)
	  (ff:fslot-value totalbytes :lowpart))))))

(defun basename (p)
  (setf p (namestring p))
  (if (char= #\\ (schar p (1- (length p))))
      (setf p (subseq p 0 (1- (length p)))))
  (let ((pos (position #\\ p :from-end t)))
    (if (null pos)
        p
      (subseq p (1+ pos)))))

(defun roundup (value multiple)
  (let ((mod (mod value multiple)))
    (+ value (if (> mod 0) (- multiple mod) 0))))

;;; return how many blocks are required to contain 'value' items
;;; given a particular blocksize
(defun howmany (value blocksize)
  (/ (roundup value blocksize) blocksize))
  
