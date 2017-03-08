(in-package :user)

;; Directory caching functions.

;; We must keep directory information cached forever because readdir
;; cookies are expected (by NFS clients, particularly Solaris) to
;; be valid forever.

(defparameter *nfs-dircache* (make-hash-table :test #'eq)) ;; Key is fhandle, value is dircache struct
(defparameter *nfs-dircachelock* (mp:make-process-lock))

(defparameter *dir-id* 0)

(defstruct dircache 
  (entries (make-array 0) :type simple-vector)
  (mtime (excl::cl-internal-real-time) :type fixnum)

  ;; This is a list of indices of dircache-entries which are 
  ;; available for reuse.  It is added to by update-dircache and
  ;; nfs-remove-file-from-dir and reduced by add-to-dircache. 
  (free-slots nil :type list)
  
  (id (incf *dir-id*)))

;; Returns a list or array of basenames.
;; called by nfs-lookup-dir.
(defun augmented-directory (dir as-array)
  (declare (optimize (speed 3) (safety 0))
	   (simple-string dir))
  (if (char/= (schar dir (1- (length dir))) #\\)
      (setf dir (concatenate 'string dir "\\")))
  (let ((res (unicode-directory dir)))
    (if* as-array
       then (let ((arr (make-array (length res)))
		  (n 0))
	      (declare (fixnum n))
	      (dolist (entry res)
		(setf (aref arr n) entry)
		(incf n))
	      arr)
       else res)))

(defun add-to-dircache-tail (dc files)
  (declare (optimize (speed 3) (safety 0))
	   (list files))
  (let* ((old (dircache-entries dc))
	 (len (length old))
	 (pos len)
	 (new (make-array (the fixnum (+ len (length files))))))
    (declare (fixnum len pos))
    ;; copy old entries
    (dotimes (n len)
      (setf (aref new n) (aref old n)))
    ;; Add new entries
    (dolist (file files)
      (setf (aref new pos) file)
      (incf pos))
    (setf (dircache-entries dc) new)))

(defun add-to-dircache (dc files)
  (declare (optimize (speed 3) (safety 0))
	   (list files))
  (let ((entries (dircache-entries dc)))
    (loop
      (let ((file (pop files)))
	(if (null file) ;; done
	    (return))
	(let ((slot (pop (dircache-free-slots dc))))
	  (if* slot
	     then (setf (aref entries slot) file)
		  #+ignore
		  (format t " added ~a to slot ~a~%" file slot)
	     else ;; ran out of free slots.  Add remaining files to end
		  #+ignore
		  (format t " adding ~a to end.~%" (cons file files))
		  (add-to-dircache-tail dc (cons file files))
		  (return)))))))


;; Look for entries in the DC which are not in CURRENT-FILENAMES.
;; These are files which disappeared since the last time we looked at
;; this directory.  The return value is undefined.
(defun update-dircache-remove-missing-files (dc current-filenames)
  (declare (optimize speed (safety 0)))
  
  (let ((cached-entries         (dircache-entries dc))
	(current-filenames-hash (make-hash-table 
				 :test #'equalp 
				 :size (length current-filenames)
				 :values nil)))
    ;; Populate the hash table of current filenames
    (dolist (filename current-filenames)
      (puthash-key filename current-filenames-hash))
    
    ;; Iterate over cached entries and look for ones that are not 
    ;; in current-filenames.
    (dotimes (n (length cached-entries))
      (let ((entry (aref cached-entries n)))
	(if* (and entry (not (gethash entry current-filenames-hash)))
	   then (setf (aref cached-entries n) nil)
		#+ignore
		(format t " removed ~a from slot ~a~%"  entry n)
		(push n (dircache-free-slots dc)))))))

;; Look for and cache filenames from CURRENT-FILENAMES which are
;; not cached in DC.  The return value is undefined.
(defun update-dircache-add-missing-files (dc current-filenames)
  (declare (optimize speed (safety 0)))

  (let* ((cached-entries      (dircache-entries dc))
	 (cached-entries-hash (make-hash-table 
			       :test #'equalp 
			       :size (length cached-entries)
			       :values nil))
	 new-filenames)
    ;; Populate the hash table of cached filenames
    (loop for entry in-sequence cached-entries
	do (puthash-key entry cached-entries-hash))
    
    ;; Iterate over current filenames looking for 
    ;; any which have not yet been cached.  
    (dolist (filename current-filenames)
      (when (not (gethash filename cached-entries-hash))
	;; No cache hit.  Add it to the list of files
	;; to add to the cache.
	(push filename new-filenames)))

    (when new-filenames
      ;; Now add the new files to the cache
      (add-to-dircache dc new-filenames))))

;; Updates dircache DC by removing cached entries which no longer
;; exist in the directory and adding new cached entries for files
;; which showed up in the directory since the last update.
(defun update-dircache (path dc)
  (declare (optimize speed (safety 0)))
  #+ignore
   (format t "update-dircache.~%")

   (let ((current-filenames (augmented-directory path nil)))
	  
     (update-dircache-remove-missing-files dc current-filenames)
     (update-dircache-add-missing-files    dc current-filenames)

     ;; Update timestamp
     (setf (dircache-mtime dc) (excl::cl-internal-real-time))))

;; Called by:
;; nfs-add-file-to-dir, :operator
;; nfs-remove-file-from-dir, :operator
;; add-direntries, :operator
(defun nfs-lookup-dir (fh create)
  (declare (optimize (speed 3)))
  (mp:with-process-lock (*nfs-dircachelock*)
    (let ((dc (gethash fh *nfs-dircache*))
	  (debug nil))
      (when debug
	(logit "nfs-lookup-dir for ~a~%" (fh-pathname fh)))
      
      (if* (null dc)
	 then (when (not create)
		(when debug
		  (logit "No cache hit and not in create mode.  Returning nil.~%"))
		(return-from nfs-lookup-dir))
	      (when debug
		(logit "No cache hit.  Preparing a new cache entry.~%"))
	      (let ((path (fh-pathname fh)))
		(setf dc (make-dircache :entries (augmented-directory path t)))
		(setf (gethash fh *nfs-dircache*) dc)
		(values (dircache-entries dc) dc))
	 else (when debug
		(logit "Cache hit.~%"))
	      (when (>= (the fixnum 
			  (- (the fixnum (excl::cl-internal-real-time))
			     (dircache-mtime dc)))
			(the fixnum *nfs-dircache-update-interval*))
		(when debug
		  (logit "Cached information has expired.  Refreshing.~%"))
		(update-dircache (fh-pathname fh) dc))
	      (values (dircache-entries dc) dc)))))

;; Called by link, rename, mkdir, create(3) procs.
;;; doesn't add duplicates
(defun nfs-add-file-to-dir (file dirfh)
  (sanity-check-filename file :create)
  (mp:with-process-lock (*nfs-dircachelock*)
    (multiple-value-bind (entries dc)
	(nfs-lookup-dir dirfh nil)
      ;; Don't add duplicates
      (when (and dc (not (find file entries :test #'equalp)))
	(add-to-dircache dc (list file))))))

;; Called by rename, rmdir, and remove procs.
(defun nfs-remove-file-from-dir (file dirfh)
  (sanity-check-filename file :lookup)
  (mp:with-process-lock (*nfs-dircachelock*)
    (multiple-value-bind (entries dc)
	(nfs-lookup-dir dirfh nil)
      (when dc
	(let ((pos (position file entries :test #'equalp)))
	  (when pos
	    (setf (aref entries pos) nil)
	    (push pos (dircache-free-slots dc))))))))
