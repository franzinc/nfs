;; $Id: xdr.cl,v 1.8 2001/06/07 19:09:38 dancy Exp $

(in-package :user)

(eval-when (compile)
  (declaim (optimize (speed 3) (safety 1))))

(defstruct xdr
  (vec :type '(simple-array (unsigned-byte 8) (*)))
  (size :type fixnum)			; build: maximum offset in vec (not necessarily the length of the vec)
  direction
  (pos :type fixnum)) ;; current position in vec (extracting and building)

(defparameter *xdrdefaultsize* 256)

(eval-when (compile eval load)
  (defmacro xdr-with-seek ((xdr pos &key absolute) &body body)
    (let ((oldpos (gensym))
	  (res (gensym)))
      `(let ((,oldpos (xdr-pos ,xdr))
	     ,res)
	 (if ,absolute 
	     (setf (xdr-pos ,xdr) ,pos)
	   (incf (xdr-pos ,xdr) ,pos))
	 (setf ,res (multiple-value-list (progn ,@body)))
	 (setf (xdr-pos ,xdr) ,oldpos)
	 (values-list ,res)
	 ))))

(eval-when (compile eval load)
  (defmacro xdr-compute-bytes-added ((xdr) &body body)
    (let ((oldpos (gensym)))
      `(let ((,oldpos (xdr-pos ,xdr)))
	 ,@body
	 (- (xdr-pos ,xdr) ,oldpos)))))

(eval-when (compile eval load)
  (defmacro with-xdr-xdr ((pair &key name) &body body)
    (let ((thexdr (gensym))
	  (offset (gensym))
	  (pairval (gensym)))
      (if (null name)
	  (setf name pair))
      `(let* ((,pairval ,pair)
	      (,thexdr (car ,pairval))
	      (,offset (cdr ,pairval))
	      (,name ,thexdr))
	 ;;(format t "seeking to offset ~D~%" ,offset)
	 (xdr-with-seek (,thexdr ,offset :absolute t)
			;;(format t "offset is currently ~D~%" (xdr-pos ,thexdr))
			 ,@body)))))

;; used during extraction
(defmacro xdr-advance (xdr size)
  (let ((sizeval (gensym)))
  `(let ((,sizeval ,size))
     (incf (the fixnum (xdr-pos ,xdr)) (the fixnum ,sizeval)))))

;; used during building... bumps the max size (xdr-size) value if 
;; necessary
(defmacro xdr-update-pos (xdr amount)
  `(progn
     (incf (the fixnum (xdr-pos ,xdr)) (the fixnum ,amount))
     (if (> (the fixnum (xdr-pos ,xdr)) (the fixnum (xdr-size ,xdr)))
	 (setf (the fixnum (xdr-size ,xdr)) (the fixnum (xdr-pos ,xdr))))))

;; used during building.  This will not work correctly if used within
;; a xdr-with-seek
(defmacro xdr-backspace (xdr size)
  `(progn
     (decf (the fixnum (xdr-pos ,xdr)) (the fixnum ,size))
     (decf (the fixnum (xdr-size ,xdr)) (the fixnum ,size))))

(defun create-xdr (&key (direction :extract) vec size)
  (let ((xdr (make-xdr)))
    (setf (xdr-direction xdr) direction)
    (cond 
     ((eq direction :extract) 
      (unless (vectorp vec)
        (error "create-xdr: 'vec' parameter must be specified and must be a vector"))
      (setf (xdr-pos xdr) 0)
      (setf (xdr-vec xdr) vec)
      (unless size
	(setf size (length vec)))
      (setf (xdr-size xdr) size))
     ((eq direction :build)
      (unless size 
	(setf size *xdrdefaultsize*))
      (setf (xdr-vec xdr) (make-vec size 0))
      (setf (xdr-size xdr) 0)
      (setf (xdr-pos xdr) 0)
      )
     (t 
      (error "create-xdr: Unknown direction: ~A~%" direction)))
    xdr))

(defun xdr-flush (xdr)
  (setf (xdr-size xdr) 0)
  (setf (xdr-pos xdr) 0))

(defun xdr-get-vec (xdr)
  (subseq (xdr-vec xdr) 0 (xdr-size xdr)))

(defun xdr-get-complete-vec (xdr)
  (xdr-vec xdr))

(defun xdr-expand-check (xdr more)
  (if (> (+ (xdr-pos xdr) more) (length (xdr-vec xdr)))
      (progn
	(format t "expanding xdr~%")
	(setf (xdr-vec xdr) (concatenate '(vector (unsigned-byte 8)) (xdr-vec xdr) (make-vec (max *xdrdefaultsize* more)))))))



(defun extract-uint-from-array (array offset)
  (declare (type (simple-array (unsigned-byte 8) (*)) array)
	   (type (mod 65563) offset))
  (+ (aref array (+ offset 3))
     (ash (aref array (+ offset 2)) 8)
     (ash (aref array (+ offset 1)) 16)
     (ash (aref array offset) 24)))

(defun set-uint-in-array (uint array offset)
  (declare (type (simple-array (unsigned-byte 8) (*)) array)
	   (type (mod 65563) offset)
	   (type (unsigned-byte 32) uint))
  (setf (aref array offset) (ash uint -24))
  (let ((uintfixnum (logand uint #x00ffffff)))
    (declare (type fixnum uintfixnum))
    (setf (aref array (+ offset 1)) (logand #xff (ash uintfixnum -16)))
    (setf (aref array (+ offset 2)) (logand #xff (ash uintfixnum -8)))
    (setf (aref array (+ offset 3)) (logand #xff uintfixnum))))
  

(defun xdr-unsigned-int (xdr &optional int)
  (declare (type (unsigned-byte 32) int))
  (let ((direction (xdr-direction xdr))
	res)
    (cond
     ((eq direction :extract)
      (setf res (extract-uint-from-array (xdr-vec xdr) (xdr-pos xdr)))
      (xdr-advance xdr 4)
      res)
     ((eq direction :build)
      (xdr-expand-check xdr 4)
      (set-uint-in-array int (xdr-vec xdr) (xdr-pos xdr))
      (xdr-update-pos xdr 4)))))


(defun xdr-int (xdr &optional int)
  (let ((direction (xdr-direction xdr))
	res)
    (cond
     ((eq direction :extract)
      (setf res (xdr-unsigned-int xdr))
      (if (not (= 0 (logand res #x80000000)))
	  (incf res -4294967296))
      res)
     ((eq direction :build)
      (xdr-unsigned-int xdr int)))))

;; returns a vector
(defun xdr-opaque-fixed (xdr &key vec len)
  (let ((direction (xdr-direction xdr))
	res
	plen)
    (cond
     ((eq direction :extract)
      (unless len
        (error "xdr-opaque-fixed: 'len' parameter is required"))
      (setf res (list xdr (xdr-pos xdr) len))
      (xdr-advance xdr (compute-padded-len len))
      res)
     ((eq direction :build)
      (unless vec
        (error "xdr-opaque-fixed: 'vec' parameter is required"))
      (unless len
        (setf len (length vec)))
      (setf plen (compute-padded-len len))
      (xdr-expand-check xdr plen)
      (dotimes (i len)
        (setf (aref (xdr-vec xdr) (+ i (xdr-pos xdr))) (aref vec i)))
      (xdr-update-pos xdr plen)))))


 ;;; extract:  returns (xdr offset length)
(defun xdr-opaque-variable (xdr &key vec len)
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (xdr-opaque-fixed xdr :len (xdr-int xdr)))
     ((eq direction :build)
      (unless vec
        (error "xdr-opaque-variable: 'vec' parameter is required"))
      (unless len
	(setf len (length vec)))
      (xdr-int xdr len)
      (xdr-opaque-fixed xdr :vec vec :len len)))))

(defun xdr-opaque-variable-from-stream (xdr stream count)
  (unless (eq (xdr-direction xdr) :build)
    (error "xdr-opaque-variable-from-stream is only good for building"))
  ;;(format t "count is ~D~%" count)
  (let (bytesread)
    (xdr-with-seek (xdr 4)
		   (xdr-expand-check xdr (compute-padded-len count))
		   (setf bytesread 
		     (- (read-sequence (xdr-vec xdr) stream 
				       :start (xdr-pos xdr)
				       :end (+ (xdr-pos xdr) count))
			(xdr-pos xdr))))
    ;;(format t "bytes read is ~D~%" bytesread)
    (xdr-int xdr bytesread)
    (xdr-update-pos xdr bytesread)))

(defun xdr-array-fixed (xdr typefunc &key len things)
  (let ((direction (xdr-direction xdr))
        res)
    (cond
     ((eq direction :extract)
      (unless len
        (error "xdr-array-fixed: 'len' parameter is required"))
      (dotimes (i len)
        (push (funcall typefunc xdr) res))
      (reverse res))
     ((eq direction :build)
      (unless (consp things)
        (error "xdr-array-fixed: 'things' parameter must be a list"))
      (dolist (thing things)
        (funcall typefunc xdr thing))))))

(defun xdr-array-variable (xdr typefunc &optional things)
  (let ((direction (xdr-direction xdr))
        len)
    (cond
     ((eq direction :extract)
      (setf len (xdr-int xdr))
      (xdr-array-fixed xdr typefunc :len len))
     ((eq direction :build)
      (setf len (length things))
      (xdr-int xdr len)
      (xdr-array-fixed xdr typefunc :len len :things things)))))


;; auth_flavor: AUTH_NULL = 0 AUTH_UNIX = 1 AUTH_SHORT = 2 AUTH_DES =
;; 3

(defstruct opaque-auth
  (flavor :type fixnum) ;; int
  body-xdr
  (body-offset :type fixnum))

(defun xdr-opaque-auth (xdr &optional flavor body)
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (let ((oa (make-opaque-auth)))
	;;(format t "xdr-opaque-auth:  current xdr pos is ~D~%" (xdr-pos xdr))
        (setf (opaque-auth-flavor oa) (xdr-int xdr))
	;;(format t "xdr-opaque-auth: auth-flavor is ~D~%" (opaque-auth-flavor oa))
	(setf (opaque-auth-body-xdr oa) xdr)
	(setf (opaque-auth-body-offset oa) (+ 4 (xdr-pos xdr))) ;; add 4 to skip the integer which contains the size of the body
	(xdr-advance xdr (compute-variable-bytes-used xdr))
	;;(format t "xdr-opaque-auth: new xdr pos is ~D~%" (xdr-pos xdr))
        oa))
     ((eq direction :build)
      (xdr-int xdr flavor)
      (xdr-opaque-variable xdr :vec body)))))

(defun xdr-auth-null (xdr)
  (unless (eq (xdr-direction xdr) :build)
    (error "xdr-auth-null: Not ready for extraction yet"))
  (xdr-opaque-auth xdr 0 #()))

(defstruct auth-unix 
  stamp
  machinename ;; string machinename<255>
  uid
  gid
  gids ;; unsigned int gids<16>
  ) 

(defun xdr-auth-unix (xdr &optional au)
  (let ((direction (xdr-direction xdr)))
    (cond 
     ((eq direction :extract)
      (let* ((stamp (xdr-int xdr))
             (machinename (xdr-string xdr))
             (uid (xdr-int xdr))
             (gid (xdr-int xdr))
             (gids (xdr-array-variable xdr #'xdr-int))
             (newau (make-auth-unix
                     :stamp stamp
                     :machinename machinename
                     :uid uid
                     :gid gid
                     :gids gids)))
        newau))
     ((eq direction :build)
      (xdr-int xdr (auth-unix-stamp au))
      (xdr-string xdr (auth-unix-machinename au))
      (xdr-int xdr (auth-unix-uid au))
      (xdr-int xdr (auth-unix-gid au))
      (xdr-array-variable xdr #'xdr-int (auth-unix-gids au))))))

(defun xdr-opaque-auth-struct-to-auth-unix-struct (oa)
  (let ((xdr (opaque-auth-body-xdr oa))
	(au (make-auth-unix))
	(pos (opaque-auth-body-offset oa)))
    (xdr-with-seek (xdr pos :absolute t)
		   ;;(format t "seeked to position ~D within xdr~%" pos)
		   (setf (auth-unix-stamp au) (xdr-int xdr))
		   ;;(format t "stamp is ~D~%" (auth-unix-stamp au))
		   (setf (auth-unix-machinename au) (xdr-string xdr))
		   ;;(format t "machine name is ~A~%" (auth-unix-machinename au))
		   (setf (auth-unix-uid au) (xdr-int xdr))
		   (setf (auth-unix-gid au) (xdr-int xdr))
		   (setf (auth-unix-gids au) (xdr-array-variable xdr #'xdr-int)))
    au))



(defun make-vec (size &optional init)
  (declare (optimize speed))
  (if init
      (excl::.primcall 'sys::make-svector size init 98 init)
    (make-array (list size) :element-type '(unsigned-byte 8))))

(defun compute-padded-len (len)
  (let ((remainder (mod len 4)))
    (+ len (if (> remainder 0) (- 4 remainder) 0))))

(defun compute-variable-bytes-used (xdr)
  (let ((res))
    ;;(format t "cvbu: before seek: ~D/~D~%" (xdr-pos xdr) (xdr-size xdr))
    (setf res (xdr-with-seek (xdr 0)
			     (let ((len (xdr-int xdr)))
			       ;;(format t "cvbu: len is ~D~%" len)
			       (+ 4 (compute-padded-len len)))))
    ;;(format t "cvbu: after seek: ~D/~D~%" (xdr-pos xdr) (xdr-size xdr))
    res))
    
(defun xdr-string (xdr &optional string)
  (let ((direction (xdr-direction xdr))
        len
	plen
        newstring)
    (cond 
     ((eq direction :extract)
      (setf len (xdr-int xdr))
      ;;(format t "xdr-string: len is ~D~%" len)
      (setf newstring (make-string len))
      (dotimes (i len)
        (setf (schar newstring i) (code-char (aref (xdr-vec xdr) (+ i (xdr-pos xdr))))))
      (xdr-advance xdr (compute-padded-len len))
      newstring)
     ((eq direction :build)
      (setf len (length string))
      (setf plen (compute-padded-len len))
      (xdr-int xdr len)
      (xdr-expand-check xdr plen)
      (dotimes (i len)
        (setf (aref (xdr-vec xdr) (+ (xdr-pos xdr) i)) (char-code (schar string i))))
      (xdr-update-pos xdr plen)))))

      
      

(defun xdr-xdr (xdr &optional xdr2)
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :build)
      (unless xdr2
        (error "xdr-xdr: 'xdr2' parameter is required"))
      (let ((size (xdr-size xdr2)))
	(xdr-expand-check xdr size)
	;; do the copy....
	(dotimes (i size)
	  (setf (aref (xdr-vec xdr) (+ i (xdr-pos xdr)))
	    (aref (xdr-vec xdr2) i)))
	(xdr-update-pos xdr size)))
     ((eq direction :extract)
      (cons xdr (xdr-pos xdr))))))
      

(defun xdr-timeval (xdr &optional timeval)
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (list (xdr-unsigned-int xdr) (xdr-unsigned-int xdr)))
     ((eq direction :build)
      (unless timeval
        (error "xdr-timeval: 'timeval' parameter required"))
      (xdr-unsigned-int xdr (first timeval))
      (xdr-unsigned-int xdr (second timeval))))))


