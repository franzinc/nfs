;; file handle stuff

;; $Id: fhandle.cl,v 1.5 2001/06/07 17:14:05 dancy Exp $

(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

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
        (xdr (create-xdr :direction :build :size 32)))
    (dotimes (i (/ *fhsize* 4))
      (xdr-unsigned-int xdr id))
    (values (xdr-get-vec xdr) id xdr)))

(defun pathname-to-fhandle-with-xdr (xdr p)
  (let ((id (pathname-to-fhandle-id p)))
    (dotimes (i (/ *fhsize* 4))
      (xdr-unsigned-int xdr id))
    id))


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

;;; most of the time...  I want to take an xdr and get an pathname
;;; out of it
(defun xdr-fhandle-to-pathname (xdr)
  (let ((id (xdr-int xdr)))
    (xdr-advance xdr (- *fhsize* 4))
    (values (fhandle-id-to-pathname id) id)))

