(defun directoryp (p)
  (excl::probe-directory p))

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
                                     (statbuf (* stat))))

(ff:def-foreign-type intholder
    (:struct
     (value :int)))

(ff:def-foreign-call GetDiskFreeSpaceA ((root (* :char) string)
                                       (spc (* :int) intholder)
                                       (bps (* :int) intholder)
                                       (fc (* :int) intholder)
                                       (tc (* :int) intholder)))

(defun diskfree (root)
  (let ((spc (ff:allocate-fobject 'intholder))
        (bps (ff:allocate-fobject 'intholder))
        (fc (ff:allocate-fobject 'intholder))
        (tc (ff:allocate-fobject 'intholder))
        bpc)
    (GetDiskFreeSpaceA (concatenate 'string (pathname-device root) ":")
                       spc bps fc tc)
    (setf spc (ff:fslot-value spc 'value))
    (setf bps (ff:fslot-value bps 'value))
    (setf fc (ff:fslot-value fc 'value))
    (setf tc (ff:fslot-value tc 'value)) 
    (setf bpc (* bps spc))
    (values bpc fc tc)))


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
  