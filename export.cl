(in-package :user)

(eval-when (compile) (declaim (optimize (speed 3))))

(defparameter *exports* nil)
(defvar *exports-lock* 
    (mp:make-process-lock :name "*exports* lock"))
(defparameter *old-exports* nil)
(defparameter *next-export-id* 1)


;; couldn't use "export" or "exp"
(defstruct nfs-export
  id
  
  ;; Canonicalized name.  This will either be the string "/", or a
  ;; string that begins with a slash and which does not have a
  ;; trailing slash.
  name 

  path
  uid
  gid
  umask
  set-mode-bits
  hosts-allow
  rw-users
  ro-users
  )

(defun prepare-exports ()
  (mp:with-process-lock (*exports-lock*)
    (setf *old-exports* (coerce *exports* 'list))
    (setf *exports* nil)))

;; selections will be a list of strings (or a single string, which
;; will be converted to a list).
(defun expand-access-list (selections table)
  (if (not (listp selections))
      (setf selections (list selections)))
  (let (res)
    (dolist (selection selections)
      (multiple-value-bind (expansion found)
	  (gethash selection table)
	(when (not found)
	  (error "~S is not a defined list" selection))
	(setf res (append res expansion))))
    ;; check for special case.
    (if* (member t res)
       then
	    t
       else
	    (remove-duplicates res :test #'equalp))))

(defun define-export (&key name path (uid 9999) (gid 9999)
			   (umask 0) (set-mode-bits 0)
			   hosts-allow rw-users ro-users 
			   host-lists user-lists)
  (when (null name)
    (error ":name must be specified for define-export"))
  (when (null path)
    (error ":path must be specified for define-export"))
  (let ((canonical-name (canonicalize-name name)))
    (if canonical-name
	(mp:with-process-lock (*exports-lock*)
	  (setf *exports*
	    (append *exports*
		    (list
		     (make-nfs-export
		      :id (select-export-id)
		      :name canonical-name
		      :path (cleanup-dir path)
		      :uid uid
		      :gid gid
		      :umask umask
		      :set-mode-bits set-mode-bits
		      :hosts-allow (expand-access-list hosts-allow host-lists)
		      :rw-users (expand-access-list rw-users user-lists)
		      :ro-users (expand-access-list ro-users user-lists)
		      )))))
      (logit-stamp "Export with name '~A' isn't an acceptable name, flush your configuration and reconfigure!"
		   name))))


;; used as fsid in getattr calls.
(defun select-export-id ()
  (prog1 
      *next-export-id* 
    (incf *next-export-id*)))

(defun exports-match-p (exp1 exp2)
  (and 
   (string= (nfs-export-name exp1) (nfs-export-name exp2))
   (equalp (nfs-export-path exp1) (nfs-export-path exp2))))

(defun finalize-exports ()
  ;; Check for exports that have been removed or changed.
  (let (new-exports)
    (mp:with-process-lock (*exports-lock*)
      (dolist (exp *exports*)
	(let ((oldexp (find exp *old-exports* :test #'exports-match-p)))
	  (if* oldexp
	     then
		  ;; a (possibly) changed export.   Update the existing (old)
		  ;; export structure with the new information.
		  (setf (nfs-export-uid oldexp) (nfs-export-uid exp))
		  (setf (nfs-export-gid oldexp) (nfs-export-gid exp))
		  (setf (nfs-export-umask oldexp) (nfs-export-umask exp))
		  (setf (nfs-export-set-mode-bits oldexp)
		    (nfs-export-set-mode-bits exp))
		  (setf (nfs-export-hosts-allow oldexp)
		    (nfs-export-hosts-allow exp))
		  (setf (nfs-export-rw-users oldexp) (nfs-export-rw-users exp))
		  (setf (nfs-export-ro-users oldexp) (nfs-export-ro-users exp))
		  ;; Remove entry from old exports.
		  (setf *old-exports* (remove oldexp *old-exports* :test #'eq))
		  ;; add to list of good exports
		  (push oldexp new-exports)
	     else
		  ;; new
		  (push exp new-exports))))

      ;; Convert to an array for efficient access and sort by longest
      ;; export first, which allows regexp searching to function properly.
      (setf *exports* (coerce new-exports 'vector))
      (sort *exports* #'string> :key #'nfs-export-name))

    ;; *old-exports* is now the list of exports that existed before but
    ;; which don't now.  The file handles for that export need to be 
    ;; invalidated.
    (dolist (exp *old-exports*)
      (invalidate-export-fhandles exp))))

;; Assumptions:
;; * The caller has verified that trailing-slashified EXPORT-NAME
;;   is a prefix of trailing-slashified REQUESTED-PATH.
(defun compute-tail-path (export-name requested-path)
  "EXPORT-NAME and REQUESTED-PATH must be canonicalized names
 
   If REQUESTED-PATH is equal to EXPORT-NAME, returns NIL.  This
   indicates that there is no attempt to mount a directory within 
   the export.

   If REQUESTED-PATH is a subpath of EXPORT-NAME, returns a 
   string containing the subpath without a leading slash (and,
   since REQUESTED-PATH is a canonical path, without a trailing
   slash."
  (let* ((export-name-len    (length export-name))
	 (requested-path-len (length requested-path)))
    (when (> requested-path-len export-name-len)
      (if* (= export-name-len 1)
	 then ;; export name must be "/".  Sanity check.
	      (assert (string= export-name "/"))
	      (subseq requested-path 1)
	 else
	      (subseq requested-path (1+ export-name-len))))))

;; Called by mount::mountproc-mnt-common, :operator
(defun locate-nearest-export-by-nfs-path (path)
  "PATH is provided by the NFS client when requesting
   a file handle during a MOUNT request. 
 
   If PATH cannot be canonicalized, NIL is returned.

   Returns NIL if PATH does not correspond to any
   defined export.
  "
  (when (setf path (ignore-errors (canonicalize-name path)))
    ;; We scan the sorted exports using a trailing slash so that we know that
    ;; we're making comparisons at path-component boundaries.
    (let ((slashified-path (trailing-slashify path)))
      (mp:with-process-lock (*exports-lock*)
	(if (null *exports*)
	    (return-from locate-nearest-export-by-nfs-path nil))

	;; *exports* is sorted so that the longest export names are first
	(loop for exp in-sequence *exports*
	    do (let* ((export-name            (nfs-export-name   exp))
		      (slashified-export-name (trailing-slashify export-name)))
		 (when (prefixp slashified-export-name slashified-path)
		   ;; Found the best export.  Collect information about any
		   ;; subdirectory of the mount that was requested.
		   (return (values exp (compute-tail-path export-name path))))))))))

(defun extract-path-drive-and-tail (path)
  (multiple-value-bind (matched whole drive tail)
      (match-re "^([A-Za-z]:)(.*)" path)
    (declare (ignore whole))
    (if* matched
       then (values drive tail)
       else (multiple-value-bind (matched whole drive tail)
		(match-re "^(\\\\\\\\[^\\\\]+\\\\[^\\\\]+)(.*)" path)
	      (declare (ignore whole))
	      (when matched
		(values drive tail))))))

(defun real-path-prefix-p-1 (prefix string)
  "If PREFIX is a prefix of STRING, return the tail (excluding leading slash).
   Otherwise returns NIL"
  (declare (optimize speed (safety 0))
	   (simple-string prefix string))
  (let ((pos 0)
	(p-max (length prefix))
	(s-max (length string)))
    (declare (fixnum pos pos p-max s-max))

    ;; Make sure input is in the expected form.
    (assert (plusp p-max))
    (assert (plusp s-max))
    (assert (prefixp "\\" prefix))
    (assert (prefixp "\\" string))
    
    ;; Handle the special case of prefix == "\" 
    (when (string= prefix "\\")
      (return-from real-path-prefix-p-1 (subseq string 1)))

    (when (>= s-max p-max)
      (while (< pos p-max)
	(let ((p-char (schar prefix pos))
	      (s-char (schar string pos)))
	  (incf pos)
	  
	  (when (not (char-equal p-char s-char))
	    (return-from real-path-prefix-p-1 nil))))
      ;; Exhausted the prefix.

      (if* (= pos s-max)
	 then ;; STRING and PREFIX match exactly. 
	      ""
       elseif (eq (schar string pos) #\\)
	 then (subseq string (1+ pos))
	 else nil))))

(defun real-path-prefix-p (path prefix)
  "PATH and PREFIX must be strings.  PREFIX must be in standard form
   (as returned by cleanup-dir) which means it may or may not have
   a trailing slash.  If PREFIX is a prefix of PATH, return the tail 
   (excluding leading slash).  Otherwise returns NIL"
  (multiple-value-bind (path-drive path-tail)
      (extract-path-drive-and-tail path)
    (multiple-value-bind (prefix-drive prefix-tail)
	(extract-path-drive-and-tail prefix)
      (when (equalp path-drive prefix-drive)
	(real-path-prefix-p-1 prefix-tail path-tail)))))

#+ignore
(defun test-real-path-prefix-p ()
  ;;  path, prefix, result 
  (let ((data '(("c:\\" "c:\\" "")
		("c:\\" "d:\\" nil)
		("c:\\temp" "c:\\" "temp")
		("c:\\temp" "c:\\temp" "")
		("c:\\temp\\deeper" "c:\\temp" "deeper")
		("c:\\temper" "c:\\temp" nil)
		
		("\\\\server1\\share\\" "\\\\server1\\share\\" "")
		("\\\\server1\\share\\" "d:\\" nil)
		("\\\\server1\\share\\" "\\\\server2\\share\\" nil)
		("\\\\server1\\share\\" "\\\\server1\\other\\" nil)
		("\\\\server1\\share\\temp" "\\\\server1\\share\\" "temp")
		("\\\\server1\\share\\temp" "\\\\server1\\share\\temp" "")
		("\\\\server1\\share\\temp\\deeper" "\\\\server1\\share\\temp" "deeper")
		("\\\\server1\\share\\temper" "\\\\server1\\share\\temp" nil)

		)))
    (dolist (entry data)
      (destructuring-bind (path prefix expected-result)
	  entry
	(let ((got (real-path-prefix-p path prefix)))
	  (when (not (equal got expected-result))
	    (error "(real-path-prefix-p ~s ~s) returned ~s but expected ~s"
		   path prefix got expected-result)))))))

;; Called by recover-persistent-fh, :operator
(defun locate-nearest-export-by-real-path (path)
  (let (best-export best-tail)
    (mp:with-process-lock (*exports-lock*)
      (when *exports*
	(dotimes (n (length *exports*))
	  (let ((exp (svref *exports* n)))
	    (let* ((prefix (nfs-export-path exp))
		   (tail (real-path-prefix-p path prefix)))
	      (when (and tail
			 (or (null best-export)
			     (> (length prefix) (length (nfs-export-path best-export)))))
		(setf best-export exp)
		(setf best-tail tail)))))))
    (values best-export best-tail)))


(defun export-host-access-allowed-p (exp addr)
  (declare (optimize speed (safety 0) (debug 0)))
  (dolist (allow (nfs-export-hosts-allow exp))
    (if (addr-in-network-p addr allow)
	(return t))))

;; uid is an integer (or nil if auth-unix auth info was
;; not supplied)
(defun export-user-write-access-allowed-p (exp uid)
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((rw-users (nfs-export-rw-users exp)))
    (or (eq rw-users t)
	(eq uid (nfs-export-uid exp))
	(member uid rw-users :test #'eq))))

;; uid is an integer (or nil if auth-unix auth info was
;; not supplied)
(defun export-user-read-access-allowed-p (exp uid)
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((ro-users (nfs-export-ro-users exp)))
    (or (eq ro-users t)
	(export-user-write-access-allowed-p exp uid)
	(member uid ro-users :test #'eq))))

