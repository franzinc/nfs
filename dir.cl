(in-package :user)

;; Directory caching functions.

;; We must keep directory information cached forever because readdir
;; cookies are expected (by NFS clients, particularly Solaris) to
;; be valid forever.

(defparameter *nfs-dircache* (make-hash-table :test #'eq))
(defparameter *nfs-dircachelock* (mp:make-process-lock))

(defparameter *nfs-dircache-update-interval* 2)

(defparameter *dir-id* 0)

(defstruct dircache 
  (entries (make-array 0) :type simple-vector)
  (mtime (excl::cl-internal-real-time) :type fixnum)
  (free-slots nil :type list)
  (id (incf *dir-id*)))

;; Returns a list or array of basenames.
;; called by nfs-lookup-dir.
#+ignore
(defun augmented-directory (dir as-array)
  (declare (optimize (speed 3) (safety 0))
	   (simple-string dir))
  (if (char/= (schar dir (1- (length dir))) #\\)
      (setf dir (concatenate 'string dir "\\")))
  (let ((res (list ".." ".")))
    (declare (list res))
    (dolist (entry (directory dir))
      (push (namestring (basename entry)) res))
    (setf res (nreverse res))
    (if* as-array
       then (let ((arr (make-array (length res)))
		  (n 0))
	      (declare (fixnum n))
	      (dolist (entry res)
		(setf (aref arr n) entry)
		(incf n))
	      arr)
       else res)))

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

;; Inefficient
(defun update-dircache (path dc)
  (declare (optimize (speed 3) (safety 0)))
  #+ignore
  (format t "update-dircache.~%")
  (let ((entries (dircache-entries dc))
	(new-list (augmented-directory path nil)))
    (declare (list new-list))
    (dotimes (n (length entries))
      (let ((entry (aref entries n)))
	(if* (and entry (not (member entry new-list :test #'equalp)))
	   then (setf (aref entries n) nil)
		#+ignore
		(format t " removed ~a from slot ~a~%"  entry n)
		(push n (dircache-free-slots dc)))))
    ;; We've removed deleted entries.  Now see what we need to add.
    (let (new)
      (dolist (entry new-list)
	(if (not (find entry entries :test #'equalp))
	    (push entry new)))
      (if new
	  (add-to-dircache dc new))))
  (setf (dircache-mtime dc) (excl::cl-internal-real-time)))
  

;; Called by:
;; nfs-add-file-to-dir, :operator
;; nfs-remove-file-from-dir, :operator
;; add-direntries, :operator
(defun nfs-lookup-dir (fh create)
  (declare (optimize (speed 3)))
  (mp:with-process-lock (*nfs-dircachelock*)
    (let ((dc (gethash fh *nfs-dircache*)))
      (if* (null dc)
	 then (if (not create)
		  (return-from nfs-lookup-dir))
	      (let ((path (fh-pathname fh)))
		(setf dc (make-dircache :entries (augmented-directory path t)))
		(setf (gethash fh *nfs-dircache*) dc)
		(values (dircache-entries dc) dc))
	 else (if (>= (the fixnum 
			(- (the fixnum (excl::cl-internal-real-time))
			   (dircache-mtime dc)))
		      (the fixnum *nfs-dircache-update-interval*))
		  (update-dircache (fh-pathname fh) dc))
	      (values (dircache-entries dc) dc)))))

;; Called by link, rename, mkdir, create(3) procs.
;;; doesn't add duplicates
(defun nfs-add-file-to-dir (file dirfh)
  (sanity-check-filename file)
  (mp:with-process-lock (*nfs-dircachelock*)
    (multiple-value-bind (entries dc)
	(nfs-lookup-dir dirfh nil)
      ;; Don't add duplicates
      (when (and dc (not (find file entries :test #'equalp)))
	(add-to-dircache dc (list file))))))

;; Called by rename, rmdir, and remove procs.
(defun nfs-remove-file-from-dir (file dirfh)
  (sanity-check-filename file)
  (mp:with-process-lock (*nfs-dircachelock*)
    (multiple-value-bind (entries dc)
	(nfs-lookup-dir dirfh nil)
      (when dc
	(let ((pos (position file entries :test #'equalp)))
	  (when pos
	    (setf (aref entries pos) nil)
	    (push pos (dircache-free-slots dc))))))))
