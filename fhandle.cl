;; file handle stuff

;; $Id: fhandle.cl,v 1.3 2001/05/23 05:53:12 dancy Exp $

(defconstant *fhsize* 32)

(defparameter *fhandles* nil)
(defparameter *pathnames* nil)

(defun ensure-fhandles ()
  (unless *fhandles*
    (setf *fhandles* (make-hash-table))))

(defun ensure-pathnames ()
  (unless *pathnames*
    (setf *pathnames* (make-hash-table :test #'equalp))))

(defun get-existing-fhandle-id (p)
  (ensure-pathnames)
  (gethash (pathname p) *pathnames*))

(defun get-unused-fhandle-id ()
  (ensure-fhandles)
  (let ((id 1))
    (while (gethash id *fhandles*)
      (setf id (random (expt 2 20))))
    id))

(defun pathname-to-fhandle-id (p)
  (setf p (pathname p))
  (let ((res (get-existing-fhandle-id p)))
    (if res
        res
      (let ((id (get-unused-fhandle-id)))
        (setf (gethash id *fhandles*) p)
        (setf (gethash p *pathnames*) id)
        id))))

(defun pathname-to-fhandle (p)
  (let ((id (pathname-to-fhandle-id p))
        (xdr (create-xdr :direction :build)))
    (dotimes (i (/ *fhsize* 4))
      (xdr-unsigned-int xdr id))
    (values (xdr-get-vec xdr) id xdr)))


(defun fhandle-id-to-pathname (id)
  (ensure-fhandles)
  (gethash id *fhandles*))

(defun fhandle-vec-to-pathname (fh)
  (let* ((xdr (create-xdr :vec fh))
        (id (xdr-unsigned-int xdr)))
    (values (fhandle-id-to-pathname id) id)))

(defun fhandle-to-pathname (fh) ;; xdr
  (fhandle-vec-to-pathname (xdr-get-vec fh)))

(defun dump-fhandles ()
  (maphash #'(lambda (x y) (format t "~S -> ~S~%" x y))
	   *fhandles*))

