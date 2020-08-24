(in-package :user)

(eval-when (compile) (declaim (optimize (speed 3))))

;; Reset by prepare-exports
(defparameter *exports* (directory-tree:make-directory-tree))

(defvar *exports-lock* 
    (mp:make-process-lock :name "*exports* lock"))

(defparameter *old-exports* nil)
(defparameter *last-export-id* 0)


(defstruct nfs-export
  id ;; used as fsid in getattr calls.
  
  ;; Canonicalized name.  This will either be the string "/", or a
  ;; string that begins with a slash and which does not have a
  ;; trailing slash.

  ;; The name the user selected for the export.  Though we save that information
  ;; here, this slot is not actually used in operation.
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

;; Called by read-nfs-cfg before making any define-export calls.
(defun prepare-exports ()
  (mp:with-process-lock (*exports-lock*)
    
    ;; *old-exports* is processed by finalize-exports
    (setf *old-exports* *exports*)
    (setf *exports* (directory-tree:make-directory-tree))))

;; Called by define-export (below).
;; SELECTIONS will be a list of strings (or a single string, which
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

;; Called by read-nfs-cfg
(defun define-export (&key name path (uid 9999) (gid 9999)
			   (umask 0) (set-mode-bits 0)
			   hosts-allow rw-users ro-users 
			   host-lists user-lists)
  (when (null name)
    (error ":name must be specified for define-export"))
  (when (null path)
    (error ":path must be specified for define-export"))
  
  (mp:with-process-lock (*exports-lock*)
    (let ((export (make-nfs-export
                   :id (incf *last-export-id*)
                   :name name
                   :path (cleanup-dir path)
                   :uid uid
                   :gid gid
                   :umask umask
                   :set-mode-bits set-mode-bits
                   :hosts-allow (expand-access-list hosts-allow host-lists)
                   :rw-users    (expand-access-list rw-users    user-lists)
                   :ro-users    (expand-access-list ro-users    user-lists)
                   )))
      ;; If a duplicate name is seen, the newer export definition will take precedence.
      (directory-tree:insert-directory-tree *exports* name export))))

;; CALLBACK will be called with 
;; 1) The canonical export name (begins with a slash)
;; 2) The export struct
(defun map-exports (callback &key (exports *exports*))
  (declare (dynamic-extent callback))
  (directory-tree:map-directory-tree exports callback))

(defmacro do-exports ((export-name export &key (exports '*exports*)) &body body)
  `(map-exports 
    (lambda (,export-name ,export)
      (declare (ignorable ,export-name ,export))
      ,@body)
    :exports ,exports))

(defun finalize-exports ()
  ;; Check for exports that have been removed or changed.
  (mp:with-process-lock (*exports-lock*)
    (do-exports (export-name exp)
      (let ((oldexp (directory-tree:find-data *old-exports* export-name)))
        (when (and oldexp 
                   (equalp (nfs-export-path oldexp) (nfs-export-path exp)))
          ;; The current export has a name and directory that matches
          ;; a prior export definition (one in *old-exports*).  This means that
          ;; the configuration of an old export has possibly been updated.
          ;; Update the old export structure with the new
          ;; information so that filehandles which still reference it will see
          ;; the new settings.
          
          ;;(format t "Updating the config of old export ~a~%" export-name)
          (setf (nfs-export-uid           oldexp) (nfs-export-uid           exp))
          (setf (nfs-export-gid           oldexp) (nfs-export-gid           exp))
          (setf (nfs-export-umask         oldexp) (nfs-export-umask         exp))
          (setf (nfs-export-set-mode-bits oldexp) (nfs-export-set-mode-bits exp))
          (setf (nfs-export-hosts-allow   oldexp) (nfs-export-hosts-allow   exp))
          (setf (nfs-export-rw-users      oldexp) (nfs-export-rw-users      exp))
          (setf (nfs-export-ro-users      oldexp) (nfs-export-ro-users      exp))
          ;; Remove entry from old exports (by setting its data to nil).  Any
          ;; entries that remain in *old-exports* will be processes later in
          ;; finalize-exports (see below).
          (directory-tree:insert-directory-tree *old-exports* export-name nil)))))
  ;; end with-process-lock
  
  ;; *old-exports* now only contains exports that existed before but which don't
  ;; now.  The file handles for those exports need to be invalidated.
  (do-exports (export-name export :exports *old-exports*)
    ;;(format t "Invaliding old export ~a~%" export-name)
    (invalidate-export-fhandles export)))


;; Called by mount::mountproc-mnt-common, :operator
(defun locate-nearest-export-by-nfs-path (path)
  ;; PATH is provided by the NFS client when requesting
  ;; a file handle during a MOUNT request. PATH may refer to 
  ;; a file or subdirectory beneath an export.  
  
  ;; This function locates the nearest (i.e. most specific) export that covers
  ;; PATH. If one is found, returns two values:
  ;; 1) The nearest export (struct)
  ;; 2) If PATH contained components beyond the export name, 
  ;;    the scond return value will be a string with the remaining
  ;;    components of the path.  The string will not have a leading
  ;;    or trailing slash.  If there is no tail component (i.e.,
  ;;    PATH matched exactly with an export name) the second return
  ;;    value is nil.
  ;;    
  ;; If no suitable export is found, nil is returned.

  (mp:with-process-lock (*exports-lock*)
    (directory-tree:find-nearest-data *exports* path)))

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

;; Called by locate-nearest-export-by-real-path (this file)
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
      (do-exports (_ exp)
        (let* ((prefix (nfs-export-path exp))
               (tail (real-path-prefix-p path prefix)))
          (when (and tail
                     (or (null best-export)
                         (> (length prefix) (length (nfs-export-path best-export)))))
            (setf best-export exp)
            (setf best-tail tail)))))
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
	(eql uid (nfs-export-uid exp))
	(member uid rw-users :test #'eq))))

;; uid is an integer (or nil if auth-unix auth info was
;; not supplied)
(defun export-user-read-access-allowed-p (exp uid)
  (declare (optimize speed (safety 0) (debug 0)))
  (let ((ro-users (nfs-export-ro-users exp)))
    (or (eq ro-users t)
	(export-user-write-access-allowed-p exp uid)
	(member uid ro-users :test #'eql))))

