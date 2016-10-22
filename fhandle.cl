;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2014 Franz Inc, Oakland, CA.  All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the Franz
;; preamble to the LGPL found in
;; http://opensource.franz.com/preamble.html.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License can be
;; found at http://opensource.franz.com/license.html.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA  02111-1307  USA

;; file handle stuff

(in-package :user)

(eval-when (compile)
  (use-package :xdr)
  (declaim (optimize (speed 3))))

;; Maps file handle vecs to fh structs
(defparameter *fhandles* (make-hash-table :test #'equalp))
;; Maps export paths to fhandles
(defparameter *export-roots* (make-hash-table :test #'equalp))
;; Tracks used non-persistent file ids
(defparameter *fake-ids* (make-hash-table :values nil))

#-64bit
(progn
  (deftype file-id-type () 'fixnum)
  (defconstant *max-file-id* most-positive-fixnum))
#+64bit
(progn
  (deftype file-id-type () '(unsigned-byte 32))
  (defconstant *max-file-id* (1- (expt 2 32))))

(defstruct fh
  pathname
  (vec (make-ausb8 *nfs-fhsize* :initial-element 0))
  (refs 0 :type fixnum)
  (file-id 0 :type file-id-type) ;; like inode number
  export
  parent ;; nil if root
  children ;; hash of basenames of directory children. values are fh's.  NIL if not used yet.
  verifier ;; used by create call w/ exclusive mode.
  alternate-pathnames) ;; for files with known hard links

(defconstant *fhandle-type-non-persistent* 0)
(defconstant *fhandle-type-persistent*     1)

(defmacro fh-vec-type (vec)
  `(aref (the ausb8 ,vec) 0))

(excl::defsubst fh-vec-non-persistent-p (vec)  
  (declare (optimize speed (safety 0)))
  (= (fh-vec-type vec) *fhandle-type-non-persistent*))

(excl::defsubst fh-vec-persistent-p (vec)  
  (declare (optimize speed (safety 0)))
  (= (fh-vec-type vec) *fhandle-type-persistent*))

(excl::defsubst fh-persistent-p (fh)
  (declare (optimize speed (safety 0)))
  (fh-vec-persistent-p (fh-vec fh)))

(defmethod print-object ((fh fh) stream)
  (format stream "#<fh [~a] ~a~a~a>" 
	  (if (fh-persistent-p fh) "P" "N")
	  (fh-pathname fh)
	  (if (fh-alternate-pathnames fh)
	      (format nil " (aka ~s)" (fh-alternate-pathnames fh))
	    "")
	  (if (fh-children fh)
	      (format nil " (children: ~a)" 
		      (let (res)
			(maphash #'(lambda (k v)
				     (declare (ignore v))
				     (push k res))
				 (fh-children fh))
			res))
	    "")))

(define-condition illegal-filename-error (file-error) 
  (
   (mode :initarg :mode :accessor mode) ;; :lookup or :create
   ))

;; Ref: http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx#naming_conventions
(excl::defsubst illegal-character-in-filename-p (filename)
  (declare (simple-string filename))
  (dotimes (n (length filename))
    (let* ((char (schar filename n))
	   (code (char-code char)))
      (when (or (< code 32)
		(case char
		  ((#\< #\> #\: #\" #\/ #\\ #\| #\? #\*)
		   t)))
	(return t)))))
  
(define-compiler-macro sanity-check-filename (filename mode &key allow-dotnames)
  `(sanity-check-filename-1 ,filename ,mode ,allow-dotnames))

(defun sanity-check-filename-1 (filename mode allow-dotnames)
  (if (or 
       ;; check 1
       (illegal-character-in-filename-p filename)
       ;; check 2
       (and (or (string= filename ".") (string= filename ".."))
	    (not allow-dotnames)) )
      (error 'illegal-filename-error 
	     :mode mode
	     :format-control "Illegal filename: ~S" 
	     :format-arguments (list filename))))

(defun add-filename-to-dirname (dir filename mode &key allow-dotnames)
  (declare (optimize speed (safety 0))
	   (simple-string dir filename))

  (sanity-check-filename filename mode :allow-dotnames allow-dotnames)
  
  (if (char= (schar dir (1- (length dir))) #\\)
      (concatenate 'string dir filename)
    (concatenate 'string dir "\\" filename)))

(defparameter *fhandles-lock* (mp:make-process-lock :name "*fhandles-lock*"))

(defmacro with-fhandles-lock (() &body body)
  `(mp:with-process-lock (*fhandles-lock*)
     ,@body))

;; Non-persistent file handle ids are chosen randomly to prevent the
;; search from taking linearly-increasing time.  Callers must be
;; holding *fhandles-lock*.  All callers of this function are
;; contained within this file.    The id selected by this function
;; is guaranteed to fit into 32-bits.
(defun select-non-persistent-file-id ()
  (declare (optimize speed))
  (let ((fake-ids *fake-ids*)
	id)
    (loop
      ;; 1+ to ensure that we never use 0.
      (setf id (1+ (random *max-file-id*)))
      (when (null (gethash id fake-ids))
	;; Claim it
	(puthash-key id fake-ids)
	;; Return it
	(return id)))))

(defun populate-non-persistent-fhandle (fh)
  (declare (optimize speed (safety 0)))
  (let ((vec (fh-vec fh))
	(id (select-non-persistent-file-id)))
    (setf (fh-vec-type vec) *fhandle-type-non-persistent*)
    (set-sb32-in-vec id vec 4)
    (setf (fh-file-id fh) id)))

(defun populate-persistent-fhandle (fh)
  "If a persistent file handle has found for FILENAME,
   populates (fh-vec FH) with it, populates (fh-file-id FH),
   and returns the 64-bit file id.

   If a persistent file handle was not available, returns NIL."
  (let* ((vec      (fh-vec fh))
	 (pathname (fh-pathname fh))
	 (id       (put-file-id-into-vec pathname vec 4))) ;; 64-bits
    (when id
      (setf (fh-vec-type vec) *fhandle-type-persistent*)
      (setf (fh-file-id fh) id))))

(defun make-fhandle (dirfh filename mode &key root-export)
  "FILENAME must be a basename.

   Returns a new fh struct with the following slots populated:
     fh-pathname
     fh-export
     fh-parent (for non-root fhandles)
     fh-vec
     fh-file-id
  "
  (let* ((fh 
	  ;; special case for root of exports
	  (if* root-export
	     then
		  (make-fh :pathname filename
			   :export root-export)
	     else
		  (make-fh :pathname (add-filename-to-dirname (fh-pathname dirfh) filename mode)
			   :export (fh-export dirfh)
			   :parent dirfh))))
    
    (if* *disable-persistent-fhandles* 
       then (populate-non-persistent-fhandle fh)
       else (or (populate-persistent-fhandle fh)
		(populate-non-persistent-fhandle fh)))
	    
    fh))

;; Used by mountd
;; 'tail' is guaranteed to have no leading or trailing slash
(defun get-fhandle-for-path (tail exp)
  (let ((dirfh (get-export-fhandle exp)))
    (when (string/= tail "")
      (dolist (comp (split-re "[\\\\/]" tail))
	(let ((fh (nfs-probe-file dirfh comp)))
	  (if* (null fh)
	     then (return-from get-fhandle-for-path nil))
	  (setf dirfh fh))))
    dirfh))
	    
(defun get-export-fhandle (exp)
  ;; See if it has already been assigned.
  (let* ((path (nfs-export-path exp))
	 (fh (gethash path *export-roots*)))
    (if* fh
       then fh
       else ;; first use.
	    (setf fh (make-fhandle nil path :lookup :root-export exp))
	    (setf (gethash (fh-vec fh) *fhandles*) fh)
	    (setf (gethash path *export-roots*) fh))))

(defun link-fh-in-dir (fh dirfh filename)
  (declare (optimize (speed 3) (safety 0)))
  (incf (fh-refs fh))
  (setf (gethash filename (fh-children dirfh)) fh))

;; Put a fhandle into the hash.. and make sure the 
;; parent has a child entry.  FILENAME must be a basename.
(defun insert-fhandle (fh filename)
  (let ((debug t))
    (when debug
      (let ((prior-fh (gethash (fh-vec fh) *fhandles*)))
	(when prior-fh
	  ;; FIXME: What else can we do?  We need
	  ;; to fix up the parent's state
	  (logit-stamp "
Replacing mapping for fileid ~a in *fhandles*.
Was ~a, now ~a"
		       (fh-file-id fh)
		       (fh-pathname prior-fh)
		       (fh-pathname fh))))))
  
  (setf (gethash (fh-vec fh) *fhandles*) fh)
  (let ((parent (fh-parent fh)))
    (if (null parent)
	(error "insert-fhandle: ~S has no parent" fh))
    ;; Create a fresh entry
    (if (not (fh-children parent))
	(setf (fh-children parent) (make-hash-table :test #'equalp)))
    (link-fh-in-dir fh parent filename)))

(define-compiler-macro lookup-fh-in-dir (dirfh filename &key allow-dotnames)
  `(lookup-fh-in-dir-1 ,dirfh ,filename ,allow-dotnames))

(defun lookup-fh-in-dir-1 (dirfh filename allow-dotnames)
  (block nil
    (with-fhandles-lock () 
      ;; will error out if filename is not acceptable.
      (sanity-check-filename filename :lookup :allow-dotnames allow-dotnames)
      ;; special cases:
      ;;  . is the same as dirfh 
      ;;  .. is the parent of dirfh
      ;; The allow-dotnames check was done in sanity-check-filename.. so
      ;; if we got this far, it's okay to have . or ..
      (if (string= filename ".")
	  (return dirfh))
      (if (string= filename "..")
	  (return (or (fh-parent dirfh) dirfh)))
      
      ;; Check cache
      (when (fh-children dirfh)
	(let ((fh (gethash filename (fh-children dirfh))))
	  (when fh
	    #+ignore
	    (format t "lookup-fh-in-dir(~a,~a) returning existing ~a~%" 
		    dirfh filename fh)
	    (return fh))))
      
      ;; No hit.  Make an entry.
      ;; make-fhandle will throw an error if something goes wrong (such
      ;; as the file not existing).

      #+ignore
      (format t "lookup-fh-in-dir(~a,~a) making new fh.~%"
	      dirfh filename)
      
      (insert-fhandle (make-fhandle dirfh filename :lookup) filename))))

;; Called by:
;; remove-fhandle, rename-fhandle, nfsd-link
(defun update-alternate-pathnames (fh op altname)
  (ecase op
    (:add 
     (push altname (fh-alternate-pathnames fh)))
    (:remove
     (if* (equalp (fh-pathname fh) (add-filename-to-dirname (fh-pathname (fh-parent fh)) altname :lookup))
	then (let ((nextup (pop (fh-alternate-pathnames fh))))
	       (when nextup
		 (setf (fh-pathname fh) 
		   (add-filename-to-dirname (fh-pathname (fh-parent fh))
					    nextup :lookup))))
	else (setf (fh-alternate-pathnames fh)
	       (delete altname (fh-alternate-pathnames fh) :test #'equalp))
	     t))))

(defun remove-fhandle-from-hash (fh)
  (let ((vec (fh-vec fh)))
    (remhash vec *fhandles*)
    (when (fh-vec-non-persistent-p vec)
      (remhash (get-sb32-in-vec vec 4) *fake-ids*))))

;; To be used with file deletion (or deletion as a side effect
;; of renaming onto an existing file).
;; Called by: nfsd-rename, nfsd-remove, nfsd-rmdir
(defun remove-fhandle (fh filename)
  (declare (optimize (speed 3) (safety 0)))
  (with-fhandles-lock ()
    (if (zerop (decf (fh-refs fh)))
	(remove-fhandle-from-hash fh))
    
    (update-alternate-pathnames fh :remove filename)
    
    ;; remove entry from parent directory.
    (let ((parent (fh-parent fh)))
      (if (null parent)
	  (error "remove-fhandle: ~S has no parent" fh))
      (remhash filename (fh-children parent)))))

;; parentname is the updated directory name.
;; yourname is the new basename
(defun update-fhandle-pathname (fh parentname yourname oldpath oldbasename)
  (let ((newname (add-filename-to-dirname parentname yourname :create)))
    (if* (equalp (fh-pathname fh) oldpath)
       then ;; We're changing the primary name.
	    (setf (fh-pathname fh) newname)
       else ;; Update one of the alternate names.
	    (setf (fh-alternate-pathnames fh)
	      (nsubstitute yourname oldbasename (fh-alternate-pathnames fh)
			   :count 1 :test #'string=)))

    (if (fh-children fh)
	(maphash 
	 #'(lambda (childname childfh)
	     (update-fhandle-pathname 
	      childfh newname childname
	      (add-filename-to-dirname oldpath childname :lookup)
	      childname))
	 (fh-children fh)))))


;; Caller is expected to remove any existing todir/tofilename
;; beforehand.  This function updates the pathname slot of the fhandle
;; (and updates children recursively, so this can be expensive for
;; directories).  
(defun rename-fhandle (fh fromfilename todir tofilename)
  #+ignore
  (format t "rename-fhandle(~a, ~a, ~a, ~a)~%" 
	  fh fromfilename todir tofilename)
  (with-fhandles-lock ()
    (let ((oldparent (fh-parent fh))
	  oldpath)
      (if (null oldparent)
	  (error "rename-fhandle: ~S has no parent" fh))
      
      (setf oldpath 
	(add-filename-to-dirname (fh-pathname oldparent) fromfilename :lookup))
      
      ;; Remove from current parent
      (remhash fromfilename (fh-children oldparent))
      
      ;; update our parent slot
      (setf (fh-parent fh) todir)
      ;; add to destination parent.
      (insert-fhandle fh tofilename)
      
      ;; FIXME:  This doesn't check for hard links that may have been
      ;; moved to another directory (which will break things).  The whole 
      ;; file handle mechanism needs to be redone to handle them properly.
      ;; see the TODO file. 
      (update-fhandle-pathname fh (fh-pathname todir) tofilename oldpath
			       fromfilename))))
			       
  
;; debugging

(defun dump-fhandles ()
  (maphash 
   #'(lambda (vec fh)
       (declare (ignore vec))
       (format t "~a" fh)
       (if (fh-alternate-pathnames fh)
	   (format t " AKA: ~a" (fh-alternate-pathnames fh)))
       (terpri))
   *fhandles*))

(defun validate-fhandle-tree ()
  (labels (
	   (validate-fhandle-tree-1 (fh)
	     (let ((pathname (fh-pathname fh)))
	       (format t "validate-fhandle-tree-1 ~a~%" pathname)
	       (if* (probe-file pathname)
		  then (let ((children (fh-children fh)))
			 (when children
			   (maphash #'(lambda (child-basename child-fh) 
					(declare (ignore child-basename))
					(validate-fhandle-tree-1 child-fh))
				     children)))
		  else (format t "*** ~a no longer exists in filesystem~%" pathname))))
	   
	   (validate-export (path fh)
	     (format t "Export: ~a~%" path)
	     (validate-fhandle-tree-1 fh))
	   )
    
    (maphash #'validate-export *export-roots*)))
  

#+ignore
(defun test ()
  (let ((*fhandles* (make-hash-table)))
    (let* ((topdir (make-fhandle nil "x:\\topdir" :root-export t))
	   (subdir (lookup-fh-in-dir topdir "subdir" :create t))
	   (inner (lookup-fh-in-dir subdir "inner" :create t))
	   (file1 (lookup-fh-in-dir topdir "testfile" :create t))
	   primary)
      (rename-fhandle file1 "testfile" subdir "testfile")
      (if (string/= "x:\\topdir\\subdir\\testfile" (fh-pathname file1))
	  (error "rename-fhandle did not set fh-pathname properly"))
      (rename-fhandle file1 "testfile" topdir "newname")
      (if (string/= "x:\\topdir\\newname" (fh-pathname file1))
	  (error "rename-fhandle did not set fh-pathname properly"))
      (rename-fhandle file1 "newname" topdir "testfile")
      (if (string/= "x:\\topdir\\testfile" (fh-pathname file1))
	  (error "rename-fhandle did not set fh-pathname properly"))
      
      (flet ((link (fh name)
	       (update-alternate-pathnames fh :add name)
	       (link-fh-in-dir fh (fh-parent fh) name))
	     (unlink (fh name)
	       (remove-fhandle fh name)))
	
	;; Hard link tests.
	(link file1 "link")
	(if (not (equalp '("link")  
			 (fh-alternate-pathnames file1)))
	    (error "fh-alternate-pathnames not updated as expected 1"))
	(unlink file1 "link")
	(if (not (null (fh-alternate-pathnames file1)))
	    (error "fh-alternate-pathnames not updated as expected 2"))
	(link file1 "link")
	;; Remove the original link
	(unlink file1 "testfile")
	(if (string/= "x:\\topdir\\link" (fh-pathname file1))
	    (error "fh-pathname not updated properly"))
	(if (not (null (fh-alternate-pathnames file1)))
	    (error "fh-alternate-pathnames not updated properly."))
	(link file1 "testfile")
	(unlink file1 "link")
	
	;; Try renaming a hard link
	(link file1 "link")
	(if (not (equalp '("link")  
			 (fh-alternate-pathnames file1)))
	    (error "fh-alternate-pathnames not updated as expected 1"))
	(rename-fhandle file1 "link" (fh-parent file1) "renamedlink")
	(if (not (string= "x:\\topdir\\testfile" (fh-pathname file1)))
	    (error "renaming a hard link caused fh-pathname to change"))
	(if (not (equalp '("renamedlink")
			 (fh-alternate-pathnames file1)))
	    (error "fh-alternate-pathnames not updates as expected 2"))
	(unlink file1 "renamedlink")
	
	;; Populate a tree w/ a file and some hard links
	(setf primary (lookup-fh-in-dir inner "primary" :create t))
	(dotimes (n 3)
	  (link primary (format nil "link~a" n)))
	
	(rename-fhandle subdir "subdir" topdir "xdir")
	(if (string/= (fh-pathname subdir) "x:\\topdir\\xdir")
	    (error "dir rename broken 1"))
	(if (string/= (fh-pathname inner) "x:\\topdir\\xdir\\inner")
	    (error "dir rename broken 2"))
	(if (string/= (fh-pathname primary) "x:\\topdir\\xdir\\inner\\primary")
	    (error "dir rename broken 3"))
	
	(remove-fhandle primary "primary")
	
	(dump-fhandles)))))
      

(defun invalidate-fhandles (fh)
  (remove-fhandle-from-hash fh)
  (let ((children (fh-children fh)))
    (when children
      (maphash #'(lambda (key value) 
		   (declare (ignore key))
		   (invalidate-fhandles value))
	       children))))

;; called by finalize-exports for exports that have been removed
;; from the configuration.
(defun invalidate-export-fhandles (exp)
  (let ((fh (get-export-fhandle exp)))
    (invalidate-fhandles fh)
    (remhash (nfs-export-path exp) *export-roots*)))

;; FIXME: add a sanity check to verify that the supplied fh vec
;; matches a freshly constructed one.
(defun recover-persistent-fh (vec)
  (when *nfs-debug* (logit-stamp  "uncached persistent file handle seen.~%"))
  (handler-bind ((syscall-error
		  (lambda (c)
		    (when (= *einval* (excl::syscall-error-errno c))
		      (return-from recover-persistent-fh :stale)))))
    (let ((pathname (file-id-vec-to-path vec 4)))
      (when *nfs-debug* (logit-stamp "Looks to be: ~a~%" pathname))
      (multiple-value-bind (exp tail)
	  (locate-nearest-export-by-real-path pathname)
	(when exp
	  (get-fhandle-for-path tail exp))))))

;; XDR

(defun xdr-fhandle-extract-common (o)
  (declare (optimize speed (safety 0)))
  
  (let ((vec (opaque-vec o))
	(offset (opaque-offset o)))
    (block nil
      (when (/= (opaque-len o) *nfs-fhsize*)
	(return :inval))
      
      ;; BLEH
      (let ((fh-vec (make-ausb8 *nfs-fhsize*)))
	(declare (dynamic-extent fh-vec))
	(copy-ausb8-into fh-vec 0 vec offset *nfs-fhsize*)
	
	(with-fhandles-lock ()
	  (let ((fh (gethash fh-vec *fhandles*)))
	    (if* fh
	       then fh
	     elseif (and (not *disable-persistent-fhandles*)
			 (fh-vec-persistent-p fh-vec)
			 (setf fh (recover-persistent-fh fh-vec)))
	       then fh
	       else :stale)))))))

(defun xdr-fhandle (xdr vers &optional fh)
  "Build: Populates XDR with the information from fh-vec.
   Extract: Returns an fh struct, or :stale or :inval"
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum vers))
  ;;(check-type xdr xdr)
  ;;(check-type vers number)
  (ecase (xdr-direction xdr)
    (:build
     (if (null fh)
	 (error "xdr-fhandle: 'fh' parameter is required when building"))
     (ecase vers
       (2
	(xdr-opaque-fixed xdr (fh-vec fh)))
       (3
	(xdr-opaque-variable xdr (fh-vec fh)))))
        
    (:extract
     (block nil
       (let ((o 
	      (case vers
		(2
		 (xdr-opaque-fixed xdr nil *nfs-fhsize*))
		(3 
		 (xdr-opaque-variable xdr))
		(t
		 (logit "Invalid file handle version: ~D" vers)
		 (return :inval)))))
	 (xdr-fhandle-extract-common o))))))

(defun xdr-fhandle2 (xdr &optional fh)
  (xdr-fhandle xdr 2 fh))

(defun xdr-fhandle3 (xdr &optional fh)
  (xdr-fhandle xdr 3 fh))

;; For use by nlm.cl
(defun fhandle-to-vec (fh vers)
  (declare (ignore vers))
  (fh-vec fh))

;; For use by nlm.cl
(defun opaque-to-fhandle3 (o)
  (xdr-fhandle-extract-common o))


