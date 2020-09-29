;; -*- mode: common-lisp -*-
;; See the file LICENSE for the full license governing this code.

(in-package :user)

(eval-when (compile load eval)
  (require :winapi)
  (require :winapi-dev)
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

(defun dword-pair-to-integer (high low)
  ;; Takes high and low 32-bit dwords and returns the corresponding integer
  (declare (optimize speed (safety 0))
	   ((unsigned-byte 32) high low))
  (logior (ash high 32) low))

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
	(nreverse res)))))

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
		       (logior excl.osi:*o-binary*
			       (ecase direction
				 (:input excl.osi:*o-rdonly*)
				 (:output excl.osi:*o-wronly*)
				 (:io excl.osi:*o-rdwr*))
			       (if create excl.osi:*o-creat* 0)
			       (if exclusive excl.osi:*o-excl* 0))
		       #o666)
      (if* (>= fd 0)
	 then (let ((handle (excl.osi::allocate-pseudofd fd)))
		(if* (< handle 0)
		   then (error "allocate-pseudofd returned ~d for fd ~d" handle fd))
		(make-instance 'file-simple-stream
		  :filename filename
		  :input-handle handle
		  :output-handle handle))
	 else (excl.osi:perror err "_wopen(~a)" filename)))))

(defmacro with-unicode-open ((var &rest rest) &body body)
  (let ((abort (gensym)))
    `(let ((,var (unicode-open ,@rest))
	   (,abort t))
       (unwind-protect (multiple-value-prog1 (progn ,@body)
			 (setq ,abort nil))
	 ;; cleanup
	 (when (streamp ,var)
	   (close ,var :abort ,abort))))))

(ff:def-foreign-call (GetFileAttributes "GetFileAttributesW")
    ((lpFileName (* :void)))
  :returning :int
  :error-value :os-specific)

(defconstant INVALID_FILE_ATTRIBUTES -1)
(defconstant FILE_ATTRIBUTE_SYSTEM 4)
(defconstant FILE_ATTRIBUTE_DIRECTORY 16)

(defconstant *symlink-header* 
    #.(make-ausb8 8 :initial-contents '(#x49 #x6e #x74 #x78 #x4c #x4e #x4b #x01)))

(defun symlink-header-p (buf)
  (declare (optimize speed (safety 0) (debug 0))
	   (ausb8 buf))
  (dotimes (n 8 t)
    (if (not (eq (aref buf n)
		 (aref *symlink-header* n)))
	(return))))

;; Called by unix-mode-from-file-attributes, :operator
(defun symlink-p (filename attrs file-length)
  (declare (optimize speed))
  (let ((attrs (or attrs (GetFileAttributes filename)))
	(file-length (or file-length (file-length filename))))
    (when (and (not (eq attrs INVALID_FILE_ATTRIBUTES))
	       (not (zerop (logand attrs FILE_ATTRIBUTE_SYSTEM)))
	       (zerop (logand attrs FILE_ATTRIBUTE_DIRECTORY))
	       (evenp file-length))
      (let ((buf (make-ausb8 8)))
	(declare (optimize (safety 0))
		 (dynamic-extent buf))
	;; We may be able to read the file attributes but not open the
	;; file, so permission denied errors are caught here.
	(handler-bind 
	    ((syscall-error
	      (lambda (e)
		(let ((errno (syscall-error-errno e)))
		  (when (eq errno *eacces*)
		    ;;(format t "symlink-p returning nil due to errno ~a~%" errno)
		    (return-from symlink-p nil))))))
	  (with-unicode-open (f filename)
	    (if (and (eq 8 (read-vector buf f))
		     (symlink-header-p buf))
		t)))))))

(defun unicode-readlink (filename)
  (declare (optimize speed))
  (let ((buf (make-ausb8 8)))
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

;; XXX This is not atomic like it is supposed to be.  One possible
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
      (error "At least one of atime or mtime must be supplied"))
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

(ff:def-foreign-type by-handle-file-information 
    (:struct
     (dwFileAttributes win:dword)
     (ftCreationTime win:filetime)
     (ftLastAccessTime win:filetime)
     (ftLastWriteTime win:filetime)
     (dwVolumeSerialNumber win:dword)
     (nFileSizeHigh win:dword)
     (nFileSizeLow win:dword)
     (nNumberOfLinks win:dword)
     (nFileIndexHigh win:dword)
     (nFileIndexLow win:dword)))

(ff:def-foreign-call GetFileInformationByHandle
    ((hFile (* :void))
     (lpFileInformation (* by-handle-file-information)))
  :returning (:int boolean)
  :error-value :os-specific)

(ff:def-foreign-call CreateFileW 
    ((lpfileName (* :void))
     (dwDesiredAccess win:dword)
     (dwShareMode win:dword)
     (lpSecurityAttributes (* :void))
     (dwCreationDisposition win:dword)
     (dwFlagsAndAttributes win:dword)
     (hTemplateFile (* :void)))
  :returning :foreign-address
  :error-value :os-specific
  :strings-convert nil)

(defconstant FILE_FLAG_BACKUP_SEMANTICS #x02000000)

(defun filetime-to-unix-time (filetime-ptr)
  "Reads the FILETIME at FILETIME-PTR and
   converts it to Unix time.  Two values
   are returned:
    1) seconds
    2) nanoseconds"
  (declare (optimize speed))
  (let* ((ticks-per-sec 10000000)
	 (secs-from-windows-epoch-to-unix-epoch 11644473600)
	 (ticks (dword-pair-to-integer 
		 (ff:fslot-value-typed 'win:filetime :c filetime-ptr 'dwHighDateTime)
		 (ff:fslot-value-typed 'win:filetime :c filetime-ptr 'dwLowDateTime))))
			
    (multiple-value-bind (secs-since-1601 remaining-ticks)
	(truncate ticks ticks-per-sec)
      (values 
       ;; Unix seconds
       (- secs-since-1601 secs-from-windows-epoch-to-unix-epoch) 
       ;; nanoseconds.  Each tick is 100ns
       (* remaining-ticks 100)))))

(defmacro filetime-to-universal-time (filetime-ptr)
  `(unix-to-universal-time (filetime-to-unix-time ,filetime-ptr)))

(ff:def-foreign-call GetFileAttributesExW
    ((lpFileName (* :void))
     (fInfoLevelId :int)
     (lpFileInformation (* :void)))
  :returning :boolean
  :error-value :os-specific
  :strings-convert nil)

;;;Called by
;;;stat-via-find-first-file, :operator
;;;unicode-stat, :operator
(defun unix-mode-from-file-attributes (filename attrs file-length)
  (declare (optimize speed))
  (if* (symlink-p filename attrs file-length)
     then #o0120777
     else ;; Windows documentation claims:
	  ;; Setting a folder to read-only makes all the files in the
	  ;; folder read-only. It does not affect the folder itself.
	  ;; To work around this nonsense, treat directories as always writeable.
	  (let* ((is-dir (logtest attrs win:FILE_ATTRIBUTE_DIRECTORY))
		 (perms (if* (or is-dir (not (logtest attrs win:FILE_ATTRIBUTE_READONLY)))
			   then #o666
			   else #o444))
		 (type (if* is-dir
			  then *s-ifdir*
			  else *s-ifreg*)))
	    (when (or is-dir (member (pathname-type filename) *executable-types* :test #'equalp))
	      (setf perms (logior perms #o111)))
	    
	    (logior type perms))))

(defun stat-via-find-first-file (filename)
  (ff:with-stack-fobject (data 'win32-find-data-w)
    (multiple-value-bind (handle err)
	(FindFirstFileW filename data)
      (if (= handle *invalid-handle*)
	  (excl.osi:perror (excl.osi::win_err_to_errno err) "FindFirstFile"))
      (win:FindClose handle)
      
      (macrolet ((access-slot (&rest names)
		   `(ff:fslot-value-typed 'win32-find-data-w :foreign data ,@names)))
	
	(let ((file-length (dword-pair-to-integer (access-slot 'nFileSizeHigh)
						  (access-slot 'nFileSizeLow))))
						  
	  (values
	   ;; If we had to resort to using stat-via-find-first-file, then the file wasn't 
	   ;; accessible using normal mechanisms, which means that attempts to read/write it
	   ;; in the future definitely won't work, so we reflect that in the mode bits.
	   (logandc2 (unix-mode-from-file-attributes filename 
						     (access-slot 'dwFileAttributes)
						     file-length)
		     #o777)
	   1 ;; nlinks
	   0 ;; uid
	   0 ;; gid
	   file-length ;; size
	   (filetime-to-universal-time (access-slot 'ftLastAccessTime))  ;; atime
	   (filetime-to-universal-time (access-slot 'ftLastWriteTime)) ;; mtime
	   ;; Return same info as mtime for ctime
	   (filetime-to-universal-time (access-slot 'ftLastWriteTime)) ;; ctime
	   ))))))

(defconstant ERROR_SHARING_VIOLATION 32)

;; Returns values used by Allegro NFS:
;; mode nlink uid gid size atime mtime ctime
(defun unicode-stat (filename) 
  (declare (optimize speed))
  (ff:with-stack-fobject (info 'win:win32_file_attribute_data)
    (multiple-value-bind (success err)
	(GetFileAttributesExW filename win:GetFileExInfoStandard info)
      (if* success
	 then (macrolet ((access-slot (&rest names)
			   `(ff:fslot-value-typed 'win:win32_file_attribute_data :foreign info ,@names)))
		(let ((file-length (dword-pair-to-integer (access-slot 'nFileSizeHigh)
							  (access-slot 'nFileSizeLow))))
		  (values
		   (unix-mode-from-file-attributes filename 
						   (access-slot 'win::dwFileAttributes)
						   file-length)
		   1 ;; nlinks
		   0 ;; uid
		   0 ;; gid
		   file-length ;; size
		   (filetime-to-universal-time (access-slot 'ftLastAccessTime))  ;; atime
		   (filetime-to-universal-time (access-slot 'ftLastWriteTime)) ;; mtime
		   ;; Return same info as mtime for ctime
		   (filetime-to-universal-time (access-slot 'ftLastWriteTime)) ;; ctime
		   )))
       elseif (= err  ERROR_SHARING_VIOLATION)
	 then ;; Try alternate approach
	      (stat-via-find-first-file filename)
	 else (excl.osi:perror (excl.osi::win_err_to_errno err) "GetFileAttributesExW")))))


;; Returns true if FILENAME names an existing stat-able directory.
;; An error will be thrown if FILENAME does not exist (or for other unexpected
;; trouble)
(defun unicode-directory-p (filename)
  (let ((mode (unicode-stat filename)))
    (= (logand mode *s-ifmt*) *s-ifdir*)))

(ff:def-foreign-type large-integer
    (:struct
     (LowPart win:dword) ;; unsigned long
     (HighPart :long)))

(ff:def-foreign-type ularge-integer
    (:struct
     ;; win:dword is unsigned long
     (LowPart win:dword)
     (HighPart win:dword)))

(defun get-ularge-integer (uli)
  (dword-pair-to-integer 
   (ff:fslot-value-typed 'ularge-integer :foreign uli 'HighPart)
   (ff:fslot-value-typed 'ularge-integer :foreign uli 'LowPart)))

(ff:def-foreign-call GetDiskFreeSpaceExW
    ;; Must be a directory! Can't be a file (unlike Unix statfs).
    ((lpDirectoryName (* void)) ;; in
     ;; Free bytes available to the user
     (lpFreeBytesAvailable (* ularge-integer)) ;; out
     ;; Size of the filesystem (possibly constrained by user's quota)
     (lpTotalNumberOfBytes (* ularge-integer)) ;; out
     ;; Free bytes available (regardless of user)
     (lpTotalNumberOfFreeBytes (* ularge-inteegr)) ;; out
     )
  :returning (:int boolean)
  :strings-convert nil
  :error-value :os-specific)

(defun unicode-get-filesystem-free-space-1 (directory)
  "DIRECTORY must be path to a directory.  It can't be a file.
  
   Returns values 
   1) The number of free bytes available for the calling user
   2) The number of free bytes available regardless of user
   3) The size of the filesystem
  "
  (ff:with-stack-fobjects ((user-free 'ularge-integer)
			   (apparent-size 'ularge-integer)
			   (total-free 'ularge-integer))
    (multiple-value-bind (ok err)
	(GetDiskFreeSpaceExW directory user-free apparent-size total-free)
      (if* ok
	 then (values (get-ularge-integer user-free)
		      (get-ularge-integer total-free)
		      (get-ularge-integer apparent-size)
		      )
	 else ;;(warn "unicode-get-filesystem-free-space got windows error code ~a~%" err)
	      (excl.osi:perror (excl.osi::win_err_to_errno err) "GetDiskFreeSpaceExW")))))

(defun unicode-get-filesystem-free-space (filename)
  "Wrapper for unicode-get-filesystem-free-space-1 which handles the
   case where FILENAME is a file, not a directory"
  
  (let ((mode (unicode-stat filename)))
    (when (not (logtest mode *s-ifdir*))
      ;; Not a directory.  Strip the filename portion
      (setf filename (dirname filename))))
  
  (unicode-get-filesystem-free-space-1 filename))

(ff:def-foreign-call GetVolumePathNameW
    ((filename (* :void)) ;; in
     (volume-path (* :void)) ;; out
     (volume-path-chars win:dword)) ;; in
  :returning (:int boolean)
  :strings-convert nil
  :error-value :os-specific)

;; Actual max is 32767 plus null terminator, but 
;; http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx
;; says "the maximum path of 32,767 characters is approximate, because
;; the "\\?\" prefix may be expanded to a longer string by the system
;; at run time, and this expansion applies to the total length".
;; Annoying.  Anyway, we'll choose a value that's more than enough.
(defconstant *windows-max-path* 40000) ;; characters

(defconstant *max-path* 260)

(defmacro with-aclmalloc ((var size) &body body)
  `(let ((,var (aclmalloc ,size)))
     (assert (not (zerop ,var)))
     (unwind-protect (progn ,@body)
       (aclfree ,var))))

(defun get-volume-path-name (filename)
  "Returns the mount point for the filesystem that contains FILENAME"
  (declare (optimize speed (safety 0)))
  (with-aclmalloc (volume-path-buf (* *windows-max-path* 2))
    (multiple-value-bind (ok err)
	(GetVolumePathNameW filename volume-path-buf (1- *windows-max-path*))
      (if* ok
	 then (native-to-string volume-path-buf :external-format :fat-le)
	 else (excl.osi:perror (excl.osi::win_err_to_errno err) "GetVolumePathNameW")))))

(ff:def-foreign-call GetVolumeNameForVolumeMountPointW 
    ((lpszVolumeMountPoint (* :void)) ;; in
     (lpszVolumeName (* :void)) ;; out
     (cchBufferLength win:dword)) ;; in, characters
  :returning (:int boolean)
  :error-value :os-specific
  :strings-convert nil
  )

;; Ref: http://msdn.microsoft.com/en-us/library/windows/desktop/aa364994(v=vs.85).aspx
;; 49 actual characters.  50 if counting the null terminator
(defconstant *max-guid-path-length* 49) ;; characters.

(defun get-volume-guid-path-for-volume-mount-point (mount-point &optional (string (make-string *max-guid-path-length*)))
  "If successful, returns the string.  If we get ERROR_INVALID_PARAMETER, we assume that
   a network path was passed in and we just return nil.  Any other problem throws
   an error"
  (declare (optimize speed (safety 0)))
  (let* ((buf-size-in-chars (1+ *max-guid-path-length*))
	 ;; The compiler isn't smart enough to recognize the constant result if I use
	 ;; (* buf-size-in-chars 2).  Disappointing.
	 (buf-size-in-bytes (* (1+ *max-guid-path-length*) 2)))
    (with-aclmalloc (buf buf-size-in-bytes)
      (multiple-value-bind (ok err)
	  (GetVolumeNameForVolumeMountPointW mount-point buf buf-size-in-chars)
	(if* ok
	   then (native-to-string buf :string string :external-format :fat-le)
	 elseif (= err windows:ERROR_INVALID_PARAMETER)
	   then nil
	   else ;;(format t "err: ~a~%" err)
		(excl.osi:perror (excl.osi::win_err_to_errno err) "GetVolumeNameForVolumeMountPointW"))))))

(defun guid-string-to-vec (string pos vec offset)
  "Returns VEC"
  (declare (optimize speed (safety 0))
	   (simple-string string)
	   (fixnum pos)
	   (ausb8 vec)
	   (fixnum offset))
  (let ((len (length string)))
    (declare (fixnum pos))
    (flet ((grab-digit-1 ()
	     (assert (< pos len))
	     (let* ((char (schar string pos))
		    (code (char-code char)))
	       (incf pos)
	       (if* (<= (char-code #\0) code (char-code #\9))
		  then (- code (char-code #\0))
		elseif (<= (char-code #\a) code (char-code #\f))
		  then (- code (- (char-code #\a) 10))
		  else (error "bogus char: ~s" char))))
	   (assert-dash ()
	     (assert (< pos len))
	     (let ((char (schar string pos)))
	       (assert (char= char #\-))
	       (incf pos))))
      (macrolet ((grab-digit ()
		   `(the (mod 16) (grab-digit-1)))
		 (grab-byte ()
		   `(the usb8
		      (+ (the usb8 (ash (grab-digit) 4)) (grab-digit))))
		 (put-byte (value)
		   `(progn
		      (setf (aref vec offset) ,value)
		      (incf offset))))
	(dotimes (n 4)
	  (put-byte (grab-byte)))
	(assert-dash)
	(dotimes (i 3)
	  (dotimes (n 2)
	    (put-byte (grab-byte)))
	  (assert-dash))
	(dotimes (n 6)
	  (put-byte (grab-byte)))
	
	vec))))

(defconstant *max-guid-string-length* 36)

(defun guid-vec-to-string (vec offset &optional (string (make-string *max-guid-string-length*)))
  (declare (optimize speed (safety 0))
	   (simple-string string)
	   (ausb8 vec)
	   (fixnum offset))
  (let ((pos 0))
    (declare (fixnum pos))
    
    (macrolet ((get-byte ()
		 `(prog1 (aref vec offset)
		    (incf offset)))
	       (byte-to-char (byte)
		 `(schar "0123456789abcdef" ,byte))
	       (byte-to-chars (byte)
		 `(let* ((b ,byte)
			 (high (ash b -4))
			 (low (logand b #xf)))
		    (values (byte-to-char high) (byte-to-char low))))
	       (put-char (char)
		 `(progn
		    (setf (schar string pos) ,char)
		    (incf pos)))
	       (put-chars (first second)
		 `(progn
		    (put-char first)
		    (put-char second)))
	       (do-byte ()
		 `(multiple-value-bind (first second)
		      (byte-to-chars (get-byte))
		    (put-chars first second)))
	       (put-dash ()
		 `(put-char #\-)))
	       
      
      (dotimes (n 4)
	(do-byte))
      (put-dash)
      (dotimes (i 3)
	(dotimes (n 2)
	  (do-byte))
	(put-dash))
      (dotimes (n 6)
	(do-byte))
      
      string)))

(defun extract-guid-from-volume-guid-path (path vec offset)
  "Places the guid into vec, which must be a usb8 array. Returns VEC"
  (declare (optimize speed (safety 0)))
  (let* ((prefix "\\\\?\\Volume{")
	 (prefix-len (length prefix)))
    (assert (prefixp prefix path))
    (guid-string-to-vec path prefix-len vec offset)))

(defun get-volume-guid-from-path (path vec offset)
  "If successful, places the guid into vec (which must be a usb8 array) and returns VEC.
   If we could not get a volume guid (probably due to being passed a network path), return
   NIL"
  (declare (optimize speed (safety 0)))
  (let* ((mount-point (get-volume-path-name path))
	 (guid-path (make-string *max-guid-path-length*)))
    (declare (dynamic-extent guid-path))
    
    (when (get-volume-guid-path-for-volume-mount-point mount-point guid-path)
      (extract-guid-from-volume-guid-path guid-path vec offset))))
  

;; FIXME: Add a cached mapping from volume serial number (which is returned
;; by GetFileInformationByHandle) to volume guids.

;; FIXME: Add a check to ensure that all volumes on the system have a different
;; serial number.

;; If successful, returns a fileid (which may be 0 if the file is on a
;; filesystem that doesn't support fileids).  Note that this will be a
;; 64-bit number, so NFSv2 clients (which use 32-bit file ids) and
;; 32-bit NFSv3 clients may behave badly (EOVERFLOW) or get bogus
;; results (due to truncation).  Ref: spr42796

;; If not successful, returns:
;;    nil
;;    errno (converted from Windows error code)
(defun get-file-id (filename)
  (declare (optimize speed))
  (multiple-value-bind (handle err)
      (CreateFileW filename 
		   win:GENERIC_READ ;; dwDesiredAccess
		   win:FILE_SHARE_READ ;; dwShareMode
		   0 ;; lpSecurityAttributes
		   win:OPEN_EXISTING ;; dwCreationDisposition
		   FILE_FLAG_BACKUP_SEMANTICS  ;; dwFlagsAndAttributes
		   0)  ;; hTemplateFile
    (when (= handle *invalid-handle*)
      (return-from get-file-id
	(values nil (excl.osi::win_err_to_errno err))))
    
    ;; Good to go
    (ff:with-stack-fobject (info 'by-handle-file-information)
      (multiple-value-bind (res err)
	  (GetFileInformationByHandle handle info)
	(when (not res)
	  (win:CloseHandle handle)
	  (return-from get-file-id
	    (values nil (excl.osi::win_err_to_errno err))))
      
	(macrolet ((access-slot (&rest names)
		     `(ff:fslot-value-typed 'by-handle-file-information :foreign info ,@names)))
	  (prog1 (dword-pair-to-integer (access-slot 'nFileIndexHigh) (access-slot 'nFileIndexLow))
	    (win:CloseHandle handle)))))))


(defun open-volume-by-guid-string (guid-string)
  "Returns an open volume handle, or throws an error if unsuccessful.
  guid-string must NOT have the curly braces."
  (let ((volume-guid-path (format nil "\\\\?\\Volume{~a}\\" guid-string)))
    (multiple-value-bind (handle err)
	(CreateFileW volume-guid-path win:GENERIC_READ win:FILE_SHARE_READ 0 win:OPEN_EXISTING
		     FILE_FLAG_BACKUP_SEMANTICS 0)
      (if* (= handle *invalid-handle*)
	 then (excl.osi:perror (excl.osi::win_err_to_errno err) "CreateFileW")
	 else handle))))

(defun open-volume-by-guid-vec (vec offset)
  "Returns an open volume handle, or throws an error if unsuccessful"
  (declare (optimize speed (safety 0)))
  (let ((guid-string (make-string *max-guid-string-length*)))
    (declare (dynamic-extent guid-string))
    (open-volume-by-guid-string (guid-vec-to-string vec offset guid-string))))

(defmacro with-open-volume-by-guid-vec ((handle vec offset) &body body)
  `(let ((,handle (open-volume-by-guid-vec ,vec ,offset)))
     (unwind-protect (progn ,@body)
       (windows:CloseHandle ,handle))))

(ff:def-foreign-type guid
    (:struct
     (Data1 :unsigned-long)
     (Data2 :unsigned-short)
     (Data3 :unsigned-short)
     (Data4 (:array :unsigned-char 8))))

(defconstant *sizeof-guid* (ff:sizeof-fobject 'guid))

(defconstant FileIdType 0)
(defconstant ObjectIdType 1)
(defconstant ExtendedFileIdType 2)
(defconstant MaximumFileIdType 3)

(ff:def-foreign-type file-id-descriptor 
    (:struct
     (dwSize win:dword)
     (type :int)
     (u
      (:union
       (FileId large-integer)
       (ObjectId guid)))
     ))

(assert (= (ff:sizeof-fobject 'file-id-descriptor) 24))

(ff:def-foreign-call OpenFileById
    ((hVolumeHint (* :void))
     (lpFileId (* file-id-descriptor))
     (dwDesiredAccess win:dword)
     (dwShareMode win:dword)
     (lpSecurityAttributes (* :void))
     (dwFlagsAndAttributes win:dword))
  :returning ((* :void))
  :error-value :os-specific)

(defun open-file-by-id (volume-handle id dwDesiredAccess dwShareMode dwFlagsAndAttributes)
  (declare (optimize speed (safety 0)))
  (ff:with-stack-fobject (file-id-descriptor 'file-id-descriptor)
    (macrolet ((slot (&rest args)
		 `(ff:fslot-value-typed 'file-id-descriptor :foreign file-id-descriptor ,@args)))
      (setf (slot 'dwSize) #.(ff:sizeof-fobject 'file-id-descriptor))
      (setf (slot 'type) FileIdType)
      (setf (slot 'u 'FileId 'LowPart) (logand id #xffffffff))
      (setf (slot 'u 'FileId 'HighPart) (ash id -32))
      
      (multiple-value-bind (handle err)
	  (OpenFileById volume-handle file-id-descriptor 
			dwDesiredAccess 
			dwShareMode 
			0
			dwFlagsAndAttributes)
	(when (= handle *invalid-handle*)
	  (excl.osi:perror (excl.osi::win_err_to_errno err) "OpenFileById"))
	
	handle))))

(defmacro with-open-file-by-id ((handle volume-handle id dwDesiredAccess dwShareMode dwFlagsAndAttributes) &body body)
  `(let ((,handle (open-file-by-id ,volume-handle ,id ,dwDesiredAccess ,dwShareMode ,dwFlagsAndAttributes)))
     (unwind-protect (progn ,@body)
       (win:CloseHandle ,handle))))

(ff:def-foreign-call GetFinalPathNameByHandleW
    ((hFile (* :void))
     (lpszFilePath (* :void))
     (cchFilePath win:dword)
     (dwFlags win:dword))
  :returning win:dword
  :strings-convert nil
  :error-value :os-specific)

(defconstant FILE_NAME_NORMALIZED 0)
(defconstant FILE_NAME_OPENED     8)
(defconstant VOLUME_NAME_DOS      0)
(defconstant VOLUME_NAME_GUID     1)
(defconstant VOLUME_NAME_NONE     4)
(defconstant VOLUME_NAME_NT       2)

(defun get-final-path-name-by-handle (handle &optional (style FILE_NAME_NORMALIZED))
  (with-aclmalloc (buf (* *windows-max-path* 2))
    (multiple-value-bind (len err)
	(GetFinalPathNameByHandleW handle buf *windows-max-path* style)
      ;; The documentation for GetFinalPathNameByHandle is unclear but experimentation
      ;; indicates taht len == 0 for real errors.
      (if* (zerop len)
	 then (excl.osi:perror (excl.osi::win_err_to_errno err) "GetFinalPathNameByHandleW")
	 else (native-to-string buf :external-format :fat-le)))))

;; File handle interface
(defun put-file-id-into-vec (filename vec offset)
  "If successful, returns the file id (which is a 64-bit number).
   If we couldn't collect the file id due to permissions, return nil.
   If we couldn't determine the volume guid, return nil.
   All other problems (such as file not found) throw an error"
  (declare (optimize speed (safety 0))
	   (ausb8 vec)
	   (fixnum offset))
  (multiple-value-bind (id errno)
      (get-file-id filename)
    (if* id
       then (when (get-volume-guid-from-path filename vec offset)
	      (incf offset *sizeof-guid*)
	      (put-uint64-into-vec id vec offset)
	      id)
     elseif (eq errno *eacces*)
       then ;; Permission denied.  Highly likely when encountering
	    ;; pagefile.sys or System Volume Information.
	    nil
       else (excl.osi:perror errno "getting file id for ~s" filename))))

(defconstant *sizeof-fileid* 8)

(defconstant FILE_READ_ATTRIBUTES #x80)

;; File handle interface

;; Notes: If a file is renamed on the Windows side, it will retain its
;; file id.  This is in line w/ Unixy behavior so it can be considered
;; desirable.  If a file is deleted such that it ends up in the
;; recycle bin, this function might return something like
;; "C:\\$Recycle.Bin\\S-1-5-21-2517939709-4264412073-2524334547-1000\\$RYNHO31.txt".
;; If the file is subsequently deleted from the recycle bin, OpenFileById will
;; error w/ (translated) errno *einval*.  Higher level code will need to catch this
;; and translate that to a stale file handle error.
;; If the volume in question no longer exists, this function will throw an error with
;; errno *enoent*.
(defun file-id-vec-to-path (vec offset)
  (declare (optimize speed (safety 0))
	   (fixnum offset))
  (with-open-volume-by-guid-vec (volume-handle vec offset)
    (incf offset *sizeof-guid*)
    (let ((id (get-uint64-from-vec vec offset)))
      (with-open-file-by-id (handle volume-handle id FILE_READ_ATTRIBUTES win:FILE_SHARE_READ 
				    (logior win:FILE_ATTRIBUTE_NORMAL FILE_FLAG_BACKUP_SEMANTICS))
	(let ((prefix "\\\\?\\")
	      (path (get-final-path-name-by-handle handle)))
	  (if* (prefixp prefix path)
	     then (subseq path (length prefix))
	     else path))))))
