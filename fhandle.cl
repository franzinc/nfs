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
  alternate-names ;; list of basenames of known hard links to this file within the same directory
  )

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
	  (if (fh-alternate-names fh)
	      (format nil " (aka ~s)" (fh-alternate-names fh))
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
  "If a file id is found for FILENAME,
   populates (fh-vec FH) with it, populates (fh-file-id FH),
   and returns the 64-bit file id.

   If a file id is not available, returns NIL."
  (let* ((vec      (fh-vec fh))
	 (pathname (fh-pathname fh))
	 (id       (put-file-id-into-vec pathname vec 4))) ;; 64-bits
    (when id
      (setf (fh-vec-type vec) *fhandle-type-persistent*)
      (setf (fh-file-id fh)
	(if* *enable-32bit-file-id-truncate*
	   then (logand #xffffffff id)
	   else id)))))

(defun make-fhandle (dirfh filename mode &key root-export)
  "FILENAME must be a basename unless ROOT-EXPORT is true.

   MODE is a keyword indicating the reason for making this
   fhandle.  It is only used for error reporting.

   Returns a new fh struct with the following slots populated:
     fh-pathname
     fh-export
     fh-parent (for non-root fhandles)
     fh-vec
     fh-file-id
  "
  (let* ((fh 
	  ;; special case for root of exports.
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

;;;Called by:
;;;recover-persistent-fh, :operator
;;;mount::mountproc-mnt-common, :operator
(defun get-fhandle-for-path (tail exp)
  "TAIL must either be NIL, in which case the file handle of EXP is returned,
   or TAIL must be a string representing a subpath below EXP.  TAIL 
   must not have leading or trailing slashes.

   Returns the file handle associated with the specified subpath
   of EXP.  Returns NIL if no such file or directory is found."
   
  (let ((dirfh (get-export-fhandle exp)))
    (when tail
      ;; Split TAIL on slash or backslash.
      (dolist (comp (split-re "[\\\\/]" tail))
	(let ((fh (nfs-probe-file dirfh comp)))
	  (if* (null fh)
	     then (return-from get-fhandle-for-path nil))
	  (setf dirfh fh))))
    dirfh))
	    
(defun get-export-fhandle (exp)
  ;; See if it has already been assigned.
  (let* ((path (nfs-export-path exp))
	 (fh   (gethash path *export-roots*)))
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

;; Insert file handle FH into the *fhandles* hash and make sure the
;; parent has a child entry for it.  FILENAME must be a basename.
(defun insert-fhandle (fh filename rename)
  (when (not rename)
    (let ((prior-fh (gethash (fh-vec fh) *fhandles*))
	  (debug nil))
      (when prior-fh
	;; We reach here if there is already an existing entry in *fhandles*
	;; for the file-id of FH.  Scenarios which could result in
	;; this state: 
	;; * A file or directory was renamed/moved outside of Allegro NFS.
	;; * A file id was recycled.  This seems to be a low probability event.
	;; * A new hard link is discovered.  In this case the removal of the
	;;   prior entry from the parent fh is the wrong thing to do.
	;;   However, I don't think it's a big deal.  In the worst case, alternating
	;;   access to each of the hard link names will result in repeated adjustment
	;;   of the file handle database.
	;; * ??
      
	(when debug
	  (logit-stamp "~%Replacing mapping for fileid ~a in *fhandles*.~%Was ~a, now ~a~%"
		       (fh-file-id fh)
		       (fh-pathname prior-fh)
		       (fh-pathname fh)))
      
	;; Remove knowledge of the old basename from the parent
	;; since we know that it is out of date.
	(remove-fhandle prior-fh (basename (fh-pathname prior-fh)))
	;; If prior-fh refers to a directory, invalidate-fhandles will recursively
	;; remove information about its children.
	(invalidate-fhandles prior-fh)))
    
    (setf (gethash (fh-vec fh) *fhandles*) fh))
  ;; end (when (not rename).. )
  
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
      
      (insert-fhandle (make-fhandle dirfh filename :lookup) filename nil))))

;; Called by:
;;;nfsd-link, :operator
;;;remove-fhandle, :operator
(defun update-alternate-pathnames (fh op altname)
  (ecase op
    (:add 
     (push altname (fh-alternate-names fh)))
    (:remove
     (let* ((parent-pathname (fh-pathname (fh-parent fh)))
	    (alt-path-to-remove (add-filename-to-dirname parent-pathname altname :lookup)))
       (if* (equalp alt-path-to-remove (fh-pathname fh))
	  then ;; The caller has requested that we remove the primary name.
	       ;; Promote the first alternate pathname to the primary pathname (if there is one).
	       (let ((first-alt-pathname (pop (fh-alternate-names fh))))
		 (when first-alt-pathname
		   (setf (fh-pathname fh) 
		     (add-filename-to-dirname parent-pathname first-alt-pathname :lookup))))
	  else ;; The caller has requested the removal of an alternate name.
	       (setf (fh-alternate-names fh)
		 (delete altname (fh-alternate-names fh) :test #'equalp))
	       t)))))

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
	    (setf (fh-alternate-names fh)
	      (nsubstitute yourname oldbasename (fh-alternate-names fh)
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
      (insert-fhandle fh tofilename t)
      
      ;; FIXME:  This doesn't check for hard links that may have been
      ;; moved to another directory (which will break things).  The whole 
      ;; file handle mechanism needs to be redone to handle them properly.
      ;; see the doc/todo.txt file.
      (update-fhandle-pathname fh (fh-pathname todir) tofilename oldpath
			       fromfilename))))
			       
  
;; debugging

(defun dump-fhandles ()
  (maphash 
   #'(lambda (vec fh)
       (declare (ignore vec))
       (format t "~a~%" fh))
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
  ;; Configuration
  (let* ((top-dir-path "c:\\nfstest")
	 (sub-dir-name "subdir")
	 (sub-dir-path (add-filename-to-dirname top-dir-path sub-dir-name nil))
	 (inner-name "inner")
	 (inner-dir-path (add-filename-to-dirname sub-dir-path inner-name nil))
	 (primary-file-path (add-filename-to-dirname inner-dir-path "primary" nil))
	 (testfile-name "testfile")
	 (testfile-path (add-filename-to-dirname top-dir-path testfile-name nil)))

    (flet ((ensure-dir (path)
	     (when (null (probe-directory path))
	       (make-directory path)))
	   (set-file-contents (path contents)
	     (setf (file-contents path :if-exists :supersede :if-does-not-exist :create) contents)))

      ;; Setup
      (ensure-dir top-dir-path)
      (ensure-dir sub-dir-path)
      (ensure-dir inner-dir-path)
      (set-file-contents testfile-path   "This is the test file")
      (set-file-contents primary-file-path "This is the primary file")

      ;; Begin testing
      (let* ((*fhandles*  (make-hash-table))
	     (topdir-fh   (make-fhandle nil top-dir-path :making-root-export :root-export t))
	     (subdir-fh   (lookup-fh-in-dir topdir-fh sub-dir-name))
	     (inner-fh    (lookup-fh-in-dir subdir-fh inner-name))
	     (testfile-fh (lookup-fh-in-dir topdir-fh testfile-name)))
	
	(labels ((ensure-fh-pathname (fh expected operation)
		   (let ((pathname (fh-pathname fh)))
		     (when (string/= pathname expected)
		       (error "After ~a, expected fh-pathname to be ~a but it was ~a"
			      operation expected pathname))))
		 (rename-test (fh from-filename dest-dir-fh to-filename)
		   (let* ((old-pathname (fh-pathname fh))
			  (old-parent-fh (fh-parent fh))
			  (new-pathname (add-filename-to-dirname (fh-pathname dest-dir-fh) to-filename nil))
			  (operation (format nil "renaming ~a to ~a" old-pathname new-pathname)))
		     (rename-fhandle fh from-filename dest-dir-fh to-filename)
		     ;; Ensure that fh-pathname was updated properly
		     (ensure-fh-pathname fh new-pathname operation)
		     ;; Ensure that the old parent no longer has an entry for from-filename
		     (assert (null (gethash from-filename (fh-children old-parent-fh))))
		     ;; Ensure that dest-dir-fh has an fh-children for to-filename
		     (assert (gethash to-filename (fh-children dest-dir-fh)))
		     )))
	  
	  ;; Move testfile file handle from the top level to the subdir.
	  (rename-test testfile-fh testfile-name subdir-fh testfile-name)
	  
	  ;; Move testfile back to the top level, but with name "newname"
	  (rename-test testfile-fh testfile-name topdir-fh "newname")
	  
	  ;; And rename back to its original name
	  (rename-test testfile-fh "newname" topdir-fh testfile-name)

	  (flet ((link (fh name)
		   ;; Update file handle info for a new hard link to FH,
		   ;; in the same directory, with basename NAME.
		   ;; Performs verification of the data structures afterward.
		   
		   (let* ((alt-names-before (fh-alternate-names fh))
			  (expected-alt-names-after (cons name alt-names-before)))
		     (update-alternate-pathnames fh :add name)
		     (link-fh-in-dir fh (fh-parent fh) name)
		     ;; Verify
		     (let ((alt-names-after (fh-alternate-names fh)))
		       (when (not (equalp alt-names-after expected-alt-names-after))
			 (error "After making a link to ~a named ~a, ~
                               expected fh-alternate-names to be ~s but it is ~s"
				fh name expected-alt-names-after alt-names-after)))
		     (assert (gethash name (fh-children (fh-parent fh))))))
		     
		 (unlink (fh name &key primary)
		   ;; If PRIMARY is true, it indicates that the caller is expecting
		   ;; to delete the primary link (and therefore expects the first altername
		   ;; name to be promoted to the primary name)
		   
		   (when primary
		     ;; Verify that NAME corresponds to the primary basename 
		     ;; of FH
		     (assert (equalp name (excl.osi:basename (fh-pathname fh)))))
		   
		   (let ((alt-names-before (fh-alternate-names fh)))
		     (remove-fhandle fh name)
		   
		     ;; Verify
		     ;; Ensure that the parent dir no longer knows about the name.
		     (assert (null (gethash name (fh-children (fh-parent fh)))))
		     (if* primary
			then ;; Verify that the first alt name became the primary basename.
			     (assert (equalp (excl.osi:basename (fh-pathname fh)) (first alt-names-before)))
			     (assert (equalp (fh-alternate-names fh) (rest alt-names-before)))
			else (assert (equalp (fh-alternate-names fh) (remove name alt-names-before :test #'equalp))))))
		 ) ;; flet defs

	
	    ;; Hard link tests.
	    (link testfile-fh "link")
	    (unlink testfile-fh "link")
	  
	    (link testfile-fh "link")
	    ;; Remove the original file.  This should promote the
	    ;; hard link name to the primary name.
	    (unlink testfile-fh "testfile" :primary t)
	    
	    ;; Put things back to normal
	    (link testfile-fh testfile-name)
	    (unlink testfile-fh "link" :primary t)

	    ;; Try renaming a hard link
	    (link testfile-fh "link")
	    (rename-fhandle testfile-fh "link" (fh-parent testfile-fh) "renamedlink")
	    (assert (equalp (fh-alternate-names testfile-fh) '("renamedlink")))
	    (assert (equalp (fh-pathname testfile-fh) testfile-path))
	    ;; Cleanup
	    (unlink testfile-fh "renamedlink")
	
	    ;; Populate a tree w/ a file and some hard links
	    (let ((primary-fh (lookup-fh-in-dir inner-fh "primary")))
	      (dotimes (n 3)
		(link primary-fh (format nil "link~a" n)))

	      ;; Renaming a directory should cause the fh-pathname of 
	      ;; all its children (recursively) to be updated.
	      (rename-test subdir-fh sub-dir-name topdir-fh "xdir")
	      
	      (let* ((xdir-path (add-filename-to-dirname top-dir-path "xdir" nil))
		     (xdir-inner-path (add-filename-to-dirname xdir-path inner-name nil)))
	      
		(if (string/= (fh-pathname subdir-fh) xdir-path)
		    (error "dir rename broken 1"))
		(if (string/= (fh-pathname inner-fh) xdir-inner-path)
		    (error "dir rename broken 2"))
		(if (string/= (fh-pathname primary-fh) (add-filename-to-dirname xdir-inner-path "primary" nil))
		    (error "dir rename broken 3")))
	      
	      (remove-fhandle primary-fh "primary"))
	    
	    #+ignore
	    (dump-fhandles)))))))
      

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
		    (let ((errno (syscall-error-errno c)))
		      (when (or (eql errno *einval*)
				(eql errno *enoent*))
			(return-from recover-persistent-fh :stale))))))
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


