;; file handle stuff

;; $Id: fhandle.cl,v 1.8 2001/07/04 20:55:27 dancy Exp $

(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

(defconstant *fhsize* 32)

(defparameter *fhandles* (make-hash-table))
(defparameter *pathnames* (make-hash-table :test #'equalp))

(defun get-unused-fhandle-id ()
  (let ((id 1))
    (while (gethash id *fhandles*)
      (setf id (random (expt 2 20))))
    id))

(defun pathname-to-fhandle-id (p)
  (declare
   (type pathname p))
  (let ((res (gethash p *pathnames*)))
    (if res
        res
      (let ((id (get-unused-fhandle-id)))
        (setf (gethash id *fhandles*) p)
        (setf (gethash p *pathnames*) id)
        id))))

(defun pathname-to-fhandle-with-xdr (xdr p)
  (declare
   (type pathname p)
   (type xdr xdr))
  (let ((id (pathname-to-fhandle-id p)))
    (dotimes (i (/ *fhsize* 4))
      (xdr-unsigned-int xdr id))
    id))


(defmacro fhandle-id-to-pathname (id)
  `(gethash ,id *fhandles*))

;;; most of the time...  I want to take an xdr and get an pathname
;;; out of it
(defun xdr-fhandle-to-pathname (xdr)
  (declare 
   (type xdr xdr))
  (let ((id (xdr-int xdr)))
    (xdr-advance xdr (- *fhsize* 4))
    (values (fhandle-id-to-pathname id) id)))

(defun opaque-fhandle-to-pathname (of)
  (let ((xdr (first of))
	(offset (second of)))
    (declare
     (type xdr xdr)
     (type fixnum offset))
    (xdr-with-seek (xdr offset :absolute t)
		   (xdr-fhandle-to-pathname xdr))))

;;; from and to are pathnames.
;;; need to update *fhandles* and *pathnames*
(defun swap-fhandles (from to)
  (let ((fromid (pathname-to-fhandle-id from))
	(toid (pathname-to-fhandle-id to)))
    (setf (gethash to *pathnames*) fromid) 
    (setf (gethash from *pathnames*) toid)
    (setf (gethash fromid *fhandles*) to)
    (setf (gethash toid *fhandles*) from)))
    

;;; for debugging
(defun dump-fhandles ()
  (maphash #'(lambda (x y) (format t "~S -> ~S~%" x y))
	   *fhandles*))
