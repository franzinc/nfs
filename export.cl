(in-package :user)

(defparameter *exports* nil)
(defparameter *old-exports* nil)
(defparameter *next-export-id* 1)


;; couldn't use "export" or "exp"
(defstruct nfs-export
  id
  name
  path
  uid
  gid
  umask
  set-mode-bits
  hosts-allow
  rw-users
  ro-users)

(defun prepare-exports ()
  (setf *old-exports* (coerce *exports* 'list))
  (setf *exports* nil))


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
  (setf *exports*
    (append *exports*
	    (list
	     (make-nfs-export
	      :id (select-export-id)
	      :name name
	      :path (cleanup-dir path)
	      :uid uid
	      :gid gid
	      :umask umask
	      :set-mode-bits set-mode-bits
	      :hosts-allow (expand-access-list hosts-allow host-lists)
	      :rw-users (expand-access-list rw-users user-lists)
	      :ro-users (expand-access-list ro-users user-lists))))))

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
    
    ;; *old-exports* is now the list of exports that existed before but
    ;; which don't now.  The file handles for that export need to be 
    ;; invalidated.
    (dolist (exp *old-exports*)
      (invalidate-export-fhandles exp))
    
    ;; Convert to an array for efficient access.  
    (setf *exports* (coerce new-exports 'vector))))

(defun locate-nearest-export (path)
  (if (null *exports*)
      (return-from locate-nearest-export))
  ;; Strip trailing slash if path is not "/"
  (let ((len (length path)))
    (if* (and (> len 1)
	      (char= #\/ (schar path (1- len))))
       then (decf len)
	    (setf path (subseq path 0 len)))
    (locate-nearest-export-1 path len)))
      
(defun locate-nearest-export-1 (path end)
  ;; See if we're there yet.
  (let ((res (locate-nearest-export-2 path end)))
    (if* res
       then (return-from locate-nearest-export-1
	      (values res (if* (string= (nfs-export-name res) "/")
			     then (subseq path end)
			     else (subseq path (1+ end)))))))
  ;; Trim down the path and repeat.
  (let ((slashpos (position #\/ path :from-end t :end end)))
    (if* (null slashpos)
       then nil
     elseif (zerop slashpos)
       then (locate-nearest-export-1 path 1)
       else (locate-nearest-export-1 path slashpos))))

(defun locate-nearest-export-2 (path end)
  (let (exp)
    (dotimes (n (length *exports*))
      (setf exp (svref *exports* n))
      (if (string= path (nfs-export-name exp) :end1 end)
	  (return exp)))))

(defun export-host-access-allowed-p (exp addr)
  (dolist (allow (nfs-export-hosts-allow exp))
    (if (addr-in-network-p addr allow)
	(return t))))

(defun export-user-write-access-allowed-p (exp uid)
  (or (= uid (nfs-export-uid exp))
      (eq (nfs-export-rw-users exp) t)
      (member uid (nfs-export-rw-users exp))))

(defun export-user-read-access-allowed-p (exp uid)
  (or (export-user-write-access-allowed-p exp uid)
      (eq (nfs-export-ro-users exp) t)
      (member uid (nfs-export-ro-users exp))))

