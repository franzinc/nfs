(in-package :user)

;; Directory caching functions.

;; Directory caching is mainly here to avoid problems
;; that can crop up in NFSv2 during an operation like
;; rm -r
;; on a large/deep directory tree.  

(defparameter *nfs-dircache* (make-hash-table :test #'equalp))
(defparameter *nfs-dircachelock* (mp:make-process-lock))

;; This can be much lower for nfsv3.  Like 1 or 2 seconds.
(defparameter *nfs-dircachereaptime* 10)

(defstruct dircache 
  entries
  lastaccess
  id)

(defparameter *nfs-dircache-id* 0)

;; Returns a list of basenames.
(defun augmented-directory (dir)
  (if (char/= (schar dir (1- (length dir))) #\\)
      (setf dir (concatenate 'string dir "\\")))
  (let ((res (directory dir)))
    (setf res (mapcar #'basename res))
    (setf res (mapcar #'namestring res))
    (push ".." res)
    (push "." res)))


(defun nfs-lookup-dir (dir)
  (mp:with-process-lock (*nfs-dircachelock*)
    (let ((dc (gethash dir *nfs-dircache*)))
      (when (null dc)
	(setf dc (make-dircache :entries (augmented-directory dir)
				:id (incf *nfs-dircache-id*)))
	(setf (gethash dir *nfs-dircache*) dc))
      (setf (dircache-lastaccess dc) (get-universal-time))
      (values (dircache-entries dc) dc))))

;;; doesn't add duplicates
(defun nfs-add-file-to-dir (file dir)
  (sanity-check-filename file)
  (mp:with-process-lock (*nfs-dircachelock*)
    (multiple-value-bind (entries dc)
	(nfs-lookup-dir dir)
      ;; Don't add duplicates
      (when (not (member file entries :test #'equalp))
	;; see if there's a blank entry we can replace
	(let ((pos (position nil entries)))
	  (when pos
	    ;; replace an empty position
	    (setf (nth pos entries) file)
	    (return-from nfs-add-file-to-dir)))
	
	;; no empty positions, add to the end of the list
	(setf (dircache-entries dc)
	  (append entries (list file)))))))

(defun nfs-remove-file-from-dir (file dir)
  (sanity-check-filename file)
  (mp:with-process-lock (*nfs-dircachelock*)
    (let* ((entries (nfs-lookup-dir dir))
	   (pos (position file entries :test #'equalp)))
      (when pos
	(setf (nth pos entries) nil)))))
	


;; cache control
(defun dircache-reaper ()
  (loop
    (sleep *nfs-dircachereaptime*)
    (reap-dircache)))

(defun reap-dircache ()
  (mp:with-process-lock (*nfs-dircachelock*)
    (let ((now (get-universal-time)))
      (maphash
       #'(lambda (key dc)
	   (if (>= now (+ (dircache-lastaccess dc) *nfs-dircachereaptime*))
	       (remhash key *nfs-dircache*)))
       *nfs-dircache*))))

;; debugging
(defun dump-nfsdircache ()
  (maphash #'(lambda (key value)
	       (format t "~S -> ~S~%" key value))
	   *nfs-dircache*))

