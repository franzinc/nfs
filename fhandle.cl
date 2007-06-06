;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
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
;;
;; $Id: fhandle.cl,v 1.25 2007/06/06 19:27:05 dancy Exp $

;; file handle stuff

(in-package :user)

(eval-when (compile)
  (use-package :xdr)
  (declaim (optimize (speed 3))))

(eval-when (compile load eval)
;; Far too large for our needs.
(defconstant *fhsize2* 32)
(defconstant *fhsizewords2* (/ *fhsize2* 4))
;; NFSv3 allows for a variable length file handle of up to 64 bytes.
;; We only use as much as needed to hold a fixnum (rounded to a 4 byte
;; boundary)
(defconstant *fhsize3* 
    (/ (floor (roundup (log (1+ most-positive-fixnum) 2) 32)) 8))
(defconstant *fhsizewords3* (/ *fhsize3* 4))
)

;; Maps file handle ids to file handles
(defparameter *fhandles* (make-hash-table))
;; Maps export paths to fhandles
(defparameter *export-roots* (make-hash-table :test #'equalp))

(defstruct fh
  pathname
  id
  (refs 0 :type fixnum)
  export
  parent ;; nil if root
  children ;; hash of basenames of directory children. values are fh's
  verifier ;; used by create call w/ exclusive mode.
  alternate-pathnames) ;; for files with known hard links

(defmethod print-object ((fh fh) stream)
  (format stream "#<fh ~d=~a~a~a>" 
	  (fh-id fh) 
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

(defun add-filename-to-dirname (dir filename)
  (if (char= (schar dir (1- (length dir))) #\\)
      (concatenate 'string dir filename)
    (concatenate 'string dir "\\" filename)))

;; File handle ids are chosen randomly to prevent
;; the search from taking linearly-increasing time.
;; caller should use without-interrupts.  Callers are 
;; all contained within this file.
(defun make-fhandle (dirfh filename &key root-export)
  (let ((id (random #.most-positive-fixnum)))
    (while (gethash id *fhandles*)
      (setf id (random #.most-positive-fixnum)))

    ;; special case for root of exports
    (if* root-export
       then
	    (make-fh :pathname filename
		     :id id
		     :export root-export)
       else
	    (make-fh :pathname (add-filename-to-dirname (fh-pathname dirfh)
							filename)
		     :id id
		     :export (fh-export dirfh)
		     :parent dirfh))))


;; Used by mountd
(defun get-export-fhandle (exp)
  ;; See if it has already been assigned.
  (let* ((path (nfs-export-path exp))
	 (fh (gethash path *export-roots*)))
    (if* fh
       then
	    fh
       else
	    ;; first use.
	    (setf fh (make-fhandle nil path :root-export exp))
	    (setf (gethash (fh-id fh) *fhandles*) fh)
	    (setf (gethash path *export-roots*) fh))))

(define-condition illegal-filename-error (file-error) ())

(defun sanity-check-filename (filename &key allow-dotnames)
  (if (or 
       ;; check 1
       (or (position #\\ filename)
	   (position #\/ filename)
	   (position #\: filename))
       ;; check 2
       (and (or (string= filename ".") (string= filename ".."))
	    (not allow-dotnames)) )
      (error 'illegal-filename-error 
	     :format-control "Illegal filename: ~S" 
	     :format-arguments (list filename))))

(defun link-fh-in-dir (fh dirfh filename)
  (declare (optimize (speed 3) (safety 0)))
  (incf (fh-refs fh))
  (setf (gethash filename (fh-children dirfh)) fh))

;; Put a fhandle into the hash.. and make sure the 
;; parent has a child entry.
(defun insert-fhandle (fh filename)
  (setf (gethash (fh-id fh) *fhandles*) fh)
  (let ((parent (fh-parent fh)))
    (if (null parent)
	(error "insert-fhandle: ~S has no parent" fh))
    ;; Create a fresh entry
    (if (not (fh-children parent))
	(setf (fh-children parent) (make-hash-table :test #'equalp)))
    (link-fh-in-dir fh parent filename)))
      
(defun lookup-fh-in-dir (dirfh filename &key create allow-dotnames)
  (block nil
    (without-interrupts 
      ;; will error out if filename is not acceptable.
      (sanity-check-filename filename :allow-dotnames allow-dotnames)
      ;; special cases:
      ;;  . is the same as dirfh 
      ;;  .. is the parent of dirfh
      ;; The allow-dotnames check was done in sanity-check-filename.. so
      ;; if we got this far, it's okay to have . or ..
      (if (string= filename ".")
	  (return dirfh))
      (if (string= filename "..")
	  (return (or (fh-parent dirfh) dirfh)))
      
      ;; See if an entry exists.
      (when (fh-children dirfh)
	(let ((fh (gethash filename (fh-children dirfh))))
	  (when fh
	    #+ignore
	    (format t "lookup-fh-in-dir(~a,~a) returning existing ~a~%" 
		    dirfh filename fh)
	    (return fh))))
      
      ;; no hash table or no entry
      (when (not create)
	#+ignore
	(format t "lookup-fh-in-dir(~a,~a) returning nil~%"
		dirfh filename)
	(return nil))
      
      ;; Create a fresh entry
      #+ignore
      (format t "lookup-fh-in-dir(~a,~a) making new fh.~%"
	      dirfh filename)
      (insert-fhandle (make-fhandle dirfh filename) filename))))

;; To be used with file deletion (or deletion as a side effect
;; of renaming onto an existing file).
;; Called by: nfsd-rename, nfsd-remove, nfsd-rmdir, nfs-probe-file,
;; with-potential-fhandle
(defun remove-fhandle (fh filename)
  (declare (optimize (speed 3)))
  (without-interrupts
    (if (zerop (setf (fh-refs fh) (the fixnum (1- (fh-refs fh)))))
	(remhash (fh-id fh) *fhandles*))
    
    (update-alternate-pathnames fh :remove filename)
    
    ;; remove entry from parent directory.
    (let ((parent (fh-parent fh)))
      (if (null parent)
	  (error "remove-fhandle: ~S has no parent" fh))
      (remhash filename (fh-children parent)))))

;; Called by:
;; remove-fhandle, rename-fhandle, nfsd-link
(defun update-alternate-pathnames (fh op altname)
  (ecase op
    (:add 
     (push altname (fh-alternate-pathnames fh)))
    (:remove
     (if* (equalp (fh-pathname fh) (add-filename-to-dirname (fh-pathname (fh-parent fh)) altname))
	then (let ((nextup (pop (fh-alternate-pathnames fh))))
	       (when nextup
		 (setf (fh-pathname fh) 
		   (add-filename-to-dirname (fh-pathname (fh-parent fh))
					    nextup))))
	else (setf (fh-alternate-pathnames fh)
	       (delete altname (fh-alternate-pathnames fh) :test #'equalp))
	     t))))

;; Caller is expected to remove any existing todir/tofilename
;; beforehand.  This function updates the pathname slot of the fhandle
;; (and updates children recursively, so this can be expensive for
;; directories).  
(defun rename-fhandle (fh fromfilename todir tofilename)
  #+ignore
  (format t "rename-fhandle(~a, ~a, ~a, ~a)~%" 
	  fh fromfilename todir tofilename)
  (without-interrupts
    (let ((oldparent (fh-parent fh))
	  oldpath)
      (if (null oldparent)
	  (error "rename-fhandle: ~S has no parent" fh))
      
      (setf oldpath 
	(add-filename-to-dirname (fh-pathname oldparent) fromfilename))
      
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
			       
  
;; parentname is the updated directory name.
;; yourname is the new basename
(defun update-fhandle-pathname (fh parentname yourname oldpath oldbasename)
  (let ((newname (add-filename-to-dirname parentname yourname)))
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
	      (add-filename-to-dirname oldpath childname)
	      childname))
	 (fh-children fh)))))

;; debugging

#+ignore
(defun dump-fhandles ()
  (maphash 
   #'(lambda (id fh)
       (format t "~a (#x~x)-> ~a" id id (fh-pathname fh))
       (if (fh-alternate-pathnames fh)
	   (format t " AKA: ~a" (fh-alternate-pathnames fh)))
       (terpri))
   *fhandles*))

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
      

;; If body runs to completion, the filehandle will be saved,
;; otherwise, it will be removed.
(defmacro with-potential-fhandle ((fhvar dirfh filename &key allow-dotnames) 
				  &body body)
  (let ((successvar (gensym))
	(dirfhvar (gensym))
	(filenamevar (gensym))
	(allow-dotnamesvar (gensym)))
    `(let* ((,dirfhvar ,dirfh)
	    (,filenamevar ,filename)
	    (,allow-dotnamesvar ,allow-dotnames)
	    (,fhvar (lookup-fh-in-dir ,dirfhvar ,filenamevar :create t 
				      :allow-dotnames ,allow-dotnamesvar))
	    (,successvar nil))
       (unwind-protect 
	   (multiple-value-prog1 (progn ,@body) (setf ,successvar t))
	 (when (not ,successvar)
	   (remove-fhandle ,fhvar ,filenamevar))))))

(defun invalidate-fhandles (fh)
  (remhash (fh-id fh) *fhandles*)
  (let ((children (fh-children fh)))
    (when children
      (maphash #'(lambda (key value) 
		   (declare (ignore key))
		   (invalidate-fhandles value))
	       children))))

(defun invalidate-export-fhandles (exp)
  (let ((fh (get-export-fhandle exp)))
    (invalidate-fhandles fh)
    (remhash (nfs-export-path exp) *export-roots*)))

;; XDR

(defun xdr-fhandle2 (xdr &optional fh)
  (xdr-fhandle xdr 2 fh))

(defun xdr-fhandle3 (xdr &optional fh)
  (xdr-fhandle xdr 3 fh))

;; For use by nlm.cl
(defun fhandle-to-vec (fh vers)
  (let ((id (fh-id fh)))
    (let ((xdr (create-xdr :direction :build)))
      (xdr-fhandle-build-common xdr (ecase vers
				      (2 #.*fhsizewords2*)
				      (3 #.*fhsizewords3*))
				id)
      (xdr-get-vec xdr))))

;; For use by nlm.cl
(defun opaque-to-fhandle3 (o)
  (with-opaque-xdr (xdr o)
    (let ((words (/ (- (xdr-size xdr) (xdr-pos xdr)) 4)))
      (if* (/= words #.*fhsizewords3*)
	 then (logit "Invalid file handle size (~D words)" words)
	      :inval
	 else (xdr-fhandle-extract-common xdr words)))))

(defun xdr-fhandle-build-common (xdr words value)
  (dotimes (i words)
    (declare (type fixnum i))
    (xdr-unsigned-int xdr (logand value #xffffffff))
    (setf value (ash value -32))))

(defun xdr-fhandle-extract-common (xdr words)
  (block nil
    (let ((id 0)
	  (shift 0))
      (dotimes (i words)
	(declare (type fixnum words shift i))
	(setf id (logior id (ash (xdr-unsigned-int xdr) shift)))
	(incf shift 32))
      (when (not (fixnump id))
	(logit "Invalid file handle (non-fixnum) ~D" id)
	(return :inval))
      (without-interrupts
	(let ((fh (gethash id *fhandles*)))
	  (if* (null fh)
	     then :stale
	     else fh))))))

(defun xdr-fhandle (xdr vers &optional fh)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum vers))
  ;;(check-type xdr xdr)
  ;;(check-type vers number)
  (ecase (xdr-direction xdr)
    (:build
     (if (null fh)
	 (error "xdr-fhandle: 'fh' parameter is required when building"))
     (let ((value (fh-id fh))
	   words)
       (declare (type fixnum value words))
       (ecase vers
	 (2
	  (setf words #.*fhsizewords2*))
	 (3 
	  (setf words #.*fhsizewords3*)
	  (xdr-int xdr #.*fhsize3*)))
       (xdr-fhandle-build-common xdr words value)))
        
    (:extract
     (block nil
       (let (words)
	 ;; Can't trust client not to fool with the file handle
	 ;; so can't declare this.
	 ;;(declare (type fixnum id))   
	 (case vers
	   (2
	    (setf words #.*fhsizewords2*))
	   (3 
	    (setf words (/ (xdr-unsigned-int xdr) 4))
	    ;; sanity check.
	    (when (/= words #.*fhsizewords3*)
	      (logit "Invalid file handle size (~D words)" words)
	      (return :inval)))
	   (t 
	    (logit "Invalid file handle version: ~D" vers)
	    (return :inval)))
	 (xdr-fhandle-extract-common xdr words))))))
