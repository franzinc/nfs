;; $Id: util.cl,v 1.7 2001/08/11 22:20:41 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (require 'winapi)
  (require 'winapi-dev))

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

(ff:def-foreign-call SetFilePointer (hFile lDistanceToMove lpDistanceToMoveHigh dwMoveMethod))

(ff:def-foreign-call SetEndOfFile (hFile))

(ff:def-foreign-type utimbuf 
    (:struct
     (actime :unsigned-int)
     (modtime :unsigned-int)))

(ff:def-foreign-call (utime "_utime") ((filename (* :char)) (times (* utimbuf)))
  :strings-convert t)

(defconstant FILE_BEGIN           0)
(defconstant FILE_CURRENT         1)
(defconstant FILE_END             2)

(defconstant INVALID_SET_FILE_POINTER -1)

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

(defun createfile (filename)
  (setf filename (namestring filename))
  (win:CreateFile 
   filename 
   win:GENERIC_WRITE 
   0
   0
   win:OPEN_EXISTING
   win:FILE_ATTRIBUTE_NORMAL
   0))

(defun truncate-file (filename size)
  (let ((hFile (createfile filename))
	res)
    (if (= hFile win:INVALID_HANDLE_VALUE)
	(return-from truncate-file (win:GetLastError)))
    
    (if* (= (SetFilePointer hFile size 0 FILE_BEGIN) INVALID_SET_FILE_POINTER -1)
	    then
	    (setf res (win:GetLastError))
	    (win:CloseHandle hFile)
	    (return-from truncate-file res))

    (if* (= 0 (SetEndOfFile hFile))
       then
	    (setf res (win:GetLastError))
	    (win:CloseHandle hFile)
	    (return-from truncate-file res))
    
    (win:CloseHandle hFile)			
    0))

(defun set-file-time (filename atime mtime)
  (setf filename (namestring filename))
  (let ((times (ff:allocate-fobject 'utimbuf)))
    (setf (ff:fslot-value times 'actime) atime)
    (setf (ff:fslot-value times 'modtime) mtime)
    (utime filename times)))
