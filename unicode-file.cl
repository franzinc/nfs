;; $Id: unicode-file.cl,v 1.3 2008/06/05 16:11:15 dancy Exp $

(in-package :user)

(eval-when (compile load eval)
  (require :winapi)
  (require :ef-fat-le))

;; Functions to allow unicode filenames.  Needed until Allegro CL
;; has built-in support.

(ff:def-foreign-type win32-find-data-w
    (:struct
     (dwFileAttributes win:dword)
     (ftCreationTime win:filetime)
     (ftLastAccessTime win:filetime)
     (ftLastWriteTime win:filetime)
     (nFileSizeHigh win:dword)
     (nFileSizeLow win:dword)
     (dwReserved win:dword)
     (dwReserved1 win:dword)
     (cFileName (:array :unsigned-short 260))
     (cAlternateFileName (:array :unsigned-short 14))))

(ff:def-foreign-call FindFirstFileW ((filename (* :void))
				     (finddata (* win32-find-data-w)))
  :error-value :os-specific
  :strings-convert nil
  :returning :foreign-address)

(ff:def-foreign-call FindNextFileW ((handle (* :void))
				    (finddata (* win32-find-data-w)))
  :error-value :os-specific
  :strings-convert nil
  :returning :int)

(defconstant *invalid-handle*
    (1- (expt 2 #+64bit 64 #-64bit 32)))

(defun fast-native-to-string (ptr string)
  (declare (optimize speed (safety 0)))
  (setf ptr (comp::ll :integer-to-mi ptr))
  (let ((len 0)
	char)
    (declare ((integer 0 260) len))
    (while (not (comp::ll := (setf char (comp::ll :aref-uword ptr 0)) 0))
      (comp::ll :aset-word string (comp::ll :fixnum-to-mi #.(sys::mdparam 'comp::md-lvector-data0-norm)) char)
      (incf len)
      (setf ptr (comp::ll :+ ptr (comp::ll :fixnum-to-mi 2)))
      (setf string (comp::ll :+ string (comp::ll :fixnum-to-mi 2))))
    len))

(defconstant *error-no-more-files* 18)

;; Not the same as the CL directory call.  Only returns basenames, and it
;; includes the . and .. names.
(defun unicode-directory (dir)
  (declare (optimize speed))
  (setf dir (concatenate 'string dir "\\*"))
  (let ((stringbuf (make-string 260))
	res)
    (declare (dynamic-extent stringbuf))
    (ff:with-static-fobject (data 'win32-find-data-w :allocation :foreign-static-gc)
      (multiple-value-bind (handle err)
	  (FindFirstFileW dir data)
	(if (= handle *invalid-handle*)
	    (excl.osi:perror (excl.osi::win_err_to_errno err) "FindFirstFile"))
	(unwind-protect 
	    (loop
	      (push 
	       (subseq stringbuf 0 (fast-native-to-string (ff:fslot-value-typed 'win32-find-data-w :foreign-static-gc data 'cFileName) stringbuf))
	       res)
	      (multiple-value-bind (status err)
		  (FindNextFileW handle data)
		(when (zerop status)
		  (if (eq err *error-no-more-files*)
		      (return))
		  (excl.osi:perror (excl.osi::win_err_to_errno err) "FindNextFile"))))
	  ;; cleanup
	  (win:FindClose handle))
	res))))

;; Workaround for bug17857
(ff:def-foreign-call (excl.osi::sys-allocate-pseudofd "allocate_pseudofd")
    ((filedes :unsigned-int)
     (kind :unsigned-short))
  :strings-convert nil
  :returning :int)

(ff:def-foreign-call (syscall-wopen "_wopen")
    ((file (* :void))
     (flags :int)
     (mode :int))
  :strings-convert nil
  :returning :int
  :error-value :errno)

;; Only supports argument combinations that are used in Allegro NFS.
;; Also doesn't line up w/ common lisp open in some respects.  For
;; example, :direction :io :if-exists :error will not signal an error
;; if the file exists since there's no way to do this check w/ a
;; single call to _wopen.  But Allegro NFS doesn't need this behavior
;; anyway.
(defun unicode-open (filename &key (direction :input) if-exists
				   if-does-not-exist)
  (let (create exclusive)
    (ecase direction
      (:input)
      (:output
       (if (null if-exists)
	   (setf if-exists :error))
       (if (null if-does-not-exist)
	   (setf if-does-not-exist :create)))
      (:io
       (if (null if-does-not-exist)
	   (setf if-does-not-exist :create))))
    
    (if (eq if-does-not-exist :create)
	(setf create t))
    (if (eq if-exists :error)
	(setf exclusive t))

    (multiple-value-bind (fd err)
	(syscall-wopen filename 
		       ;; flags
		       (logior excl::*o-binary*
			       (case direction
				 (:input excl::*o-rdonly*)
				 (:output excl::*o-wronly*)
				 (:io excl::*o-rdwr*))
			       (if create excl::*o-creat* 0)
			       (if exclusive excl::*o-excl* 0))
		       #o666)
      (if* (>= fd 0)
	 then (let ((handle (excl.osi::allocate-pseudofd fd)))
		(if* (< handle 0)
		   then (error "allocate-pseudofd returned ~d for fd ~d" handle fd))
		(make-instance 'file-simple-stream
		  :filename filename
		  :input-handle handle
		  :output-handle handle))
	 else (excl.osi:perror err "_wopen")))))

(defmacro with-unicode-open ((var &rest rest) &body body)
  (let ((abort (gensym)))
    `(let ((,var (unicode-open ,@rest))
	   (,abort t))
       (unwind-protect (multiple-value-prog1 (progn ,@body)
			 (setq ,abort nil))
	 ;; cleanup
	 (when (streamp ,var)
	   (close ,var :abort ,abort))))))

(ff:def-foreign-type dev_t :unsigned-int)
(ff:def-foreign-type ino_t :unsigned-short)
(ff:def-foreign-type int64 
    (:struct
     (low :unsigned-int)
     (high :unsigned-int)))
(ff:def-foreign-type time64_t int64)
     
#|
struct __stat64 {
        _dev_t st_dev;
        _ino_t st_ino;
        unsigned short st_mode;
        short st_nlink;
        short st_uid;
        short st_gid;
        _dev_t st_rdev;
        __int64 st_size;
        __time64_t st_atime;
        __time64_t st_mtime;
        __time64_t st_ctime;
        };
|#

(ff:def-foreign-type stat64
    (:struct
     (dev dev_t)			; 0 4
     (ino ino_t)			; 4 2
     (mode :unsigned-short)		; 6 2
     (nlink :short)			; 8 2
     (uid :short)			; 10 2
     (gid :short)			; 12 2
					; 2 bytes of alignment pad
     (rdev dev_t)			; 16 4
     (pad1 :int)			; 20 4
     (size int64)			; 24
     (atime time64_t)
     (mtime time64_t)
     (ctime time64_t)))

(ff:def-foreign-call (wstat64 "_wstat64") 
    ((path (* :void)) 
     (buffer (* stat64)))
  :strings-convert nil
  :error-value :errno)

;; Returns values used by Allegro NFS:
:: mode nlink uid gid size atime mtime ctime
(defun unicode-stat (filename) 
  (declare (optimize speed))
  (ff:with-static-fobject (sb 'stat64 :allocation :foreign-static-gc)
    (multiple-value-bind (status errno)
	(wstat64 filename sb)
      (if (not (zerop status))
	  (excl.osi:perror errno "stat64"))
      (macrolet ((slot (&rest rest)
		   `(ff:fslot-value-typed 'stat64 :foreign-static-gc sb ,@rest)))
	(macrolet ((timeslot (name)
		     `(excl.osi:unix-to-universal-time (slot ,name :low))))
	  (let ((mode (slot 'mode)))
	    (if (symlink-p filename)
		(setf mode #o0120777))
	    (values mode (slot 'nlink) (slot 'uid) (slot 'gid) 
		    (logior (slot 'size 'low) (ash (slot 'size 'high) 32))
		    (timeslot 'atime) (timeslot 'mtime) (timeslot 'ctime))))))))

(ff:def-foreign-call (GetFileAttributes "GetFileAttributesW")
    ((lpFileName (* :void)))
  :returning :int
  :error-value :os-specific)

(defconstant INVALID_FILE_ATTRIBUTES -1)
(defconstant FILE_ATTRIBUTE_SYSTEM 4)
(defconstant FILE_ATTRIBUTE_DIRECTORY 16)

(defconstant *symlink-header* 
    #.(make-array 8 :element-type '(unsigned-byte 8)
		  :initial-contents '(#x49 #x6e #x74 #x78 #x4c #x4e #x4b #x01)))

(defun symlink-header-p (buf)
  (declare (optimize speed (safety 0) (debug 0))
	   ((simple-array (unsigned-byte 8) (*)) buf))
  (dotimes (n 8 t)
    (if (not (eq (aref buf n)
		 (aref *symlink-header* n)))
	(return))))

(defun symlink-p (filename)
  (declare (optimize speed))
  (let ((res (GetFileAttributes filename)))
    (when (and (not (eq res INVALID_FILE_ATTRIBUTES))
	       (not (zerop (logand res FILE_ATTRIBUTE_SYSTEM)))
	       (zerop (logand res FILE_ATTRIBUTE_DIRECTORY))
	       (evenp (file-length filename)))
      (let ((buf (make-array 8 :element-type '(unsigned-byte 8))))
	(declare (optimize (safety 0))
		 (dynamic-extent buf))
	(with-open-file (f filename)
	  (if (and (eq 8 (read-vector buf f))
		   (symlink-header-p buf))
	      t))))))

(defun unicode-readlink (filename)
  (declare (optimize speed))
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8))))
    (declare (optimize (safety 0))
	     (dynamic-extent buf))
    (with-open-file (f filename :external-format :fat-le)
      (let ((flen (file-length f)))
	(if* (and (evenp flen)
		  (eq 8 (read-vector buf f))
		  (symlink-header-p buf))
	   then (let ((string (make-string (ash (- flen 8) -1))))
		  (read-vector string f)
		  string)
	   else (excl::.syscall-error "readlink" *einval*))))))

(ff:def-foreign-call (SetFileAttributes "SetFileAttributesW")
    ((lpFileName (* :void))
     (dwFileAttributes win:dword))
  :returning :boolean
  :error-value :os-specific)

;; XXX This is not atomic link it is supposed to be.  One possible
;; workaround would be to create a temporary file, then rename it.
(defun unicode-symlink (oldpath newpath)
  (with-open-file (f newpath :direction :output
		   :external-format :fat-le)
    (write-vector *symlink-header* f)
    (write-string oldpath f))
  (multiple-value-bind (ok err)
      (SetFileAttributes newpath FILE_ATTRIBUTE_SYSTEM)
    (or ok (excl::.winapi-error "SetFileAttributesW" err))))

(defun unicode-truncate (filename len)
  (with-unicode-open (s filename :direction :output :if-exists :overwrite)
    (os-ftruncate s len)))

(ff:def-foreign-call (syscall-wutime "_wutime")
    ((file (* :void))
     (times (* excl.osi::utimbuf)))
  :strings-convert nil
  :returning :int
  :error-value :errno)

(defun unicode-utime (filespec atime mtime)
  (if (and (null atime) (null mtime))
      (error "One of atime or mtime must be supplied"))
  (multiple-value-bind (mode nlink uid gid size file-atime file-mtime)
      (unicode-stat filespec)
    (declare (ignore mode nlink uid gid size))
    (ff:with-static-fobject (utimbuf 'excl.osi::utimbuf 
				     :allocation :foreign-static-gc)
      (setf (ff:fslot-value utimbuf 'excl.osi::actime)
	(universal-to-unix-time (or atime file-atime)))
      (setf (ff:fslot-value utimbuf 'excl.osi::modtime)
	(universal-to-unix-time (or mtime file-mtime)))
      
      (multiple-value-bind (res errno) (syscall-wutime filespec utimbuf)
        (when (/= 0 res)
          (perror errno "utime of ~a failed" filespec))
        t))))

(ff:def-foreign-call (syscall-CreateHardLinkW "CreateHardLinkW")
    ((newpath (* :void))
     (pathpath (* :void))
     (attributes :int))
  :strings-convert nil
  :returning :int
  :error-value :os-specific)

(defun unicode-link (old-filespec new-filespec)
  (multiple-value-bind (res err)
      (syscall-CreateHardLinkW new-filespec old-filespec 0)
    (if* (= res 0)
       then (perror (excl.osi::win_err_to_errno err) "link failed")
       else t)))

(ff:def-foreign-call (wunlink "_wunlink") ((filename (* :void)))
  :strings-convert nil
  :error-value :errno)

(defun unicode-unlink (filename)
  (multiple-value-bind (res errno)
      (wunlink filename)
    (if* (zerop res)
       then t
       else (perror errno "wunlink"))))

(ff:def-foreign-call (wmkdir "_wmkdir") ((dirname (* :void)))
  :strings-convert nil
  :error-value :errno)

(ff:def-foreign-call (wrmdir "_wrmdir") ((dirname (* :void)))
  :strings-convert nil
  :error-value :errno)

(defun unicode-mkdir (filename)
  (multiple-value-bind (res errno)
      (wmkdir filename)
    (if* (zerop res)
       then t
       else (perror errno "wmkdir"))))

(defun unicode-rmdir (filename)
  (multiple-value-bind (res errno)
      (wrmdir filename)
    (if* (zerop res)
       then t
       else (perror errno "wrmdir"))))
