;; $Id: xdr.cl,v 1.5 2001/05/23 21:02:59 dancy Exp $

(in-package :user)

(declaim (optimize (speed 3)))

(defstruct xdr
  vec
  size
  direction
  pos ;; for extracting
  )

(defparameter *xdrdefaultsize* 16000)

(defun create-xdr (&key (direction :extract) vec (size *xdrdefaultsize*))
  (let ((xdr (make-xdr)))
    (setf (xdr-direction xdr) direction)
    (cond 
     ((eq direction :extract) 
      (unless (vectorp vec)
        (error "create-xdr: 'vec' parameter must be specified and must be a vector"))
      (setf (xdr-pos xdr) 0)
      (setf (xdr-vec xdr) vec)
      (setf (xdr-size xdr) (length vec)))
     ((eq direction :build)
      (setf (xdr-vec xdr) (make-vec size :init 0))
      (setf (xdr-size xdr) 0)
      (setf (xdr-pos xdr) 0)
      )
     (t 
      (error "create-xdr: Unknown direction: ~A~%" direction)))
    xdr))

;;; should this use the new 'pos' slot?
(defun xdr-get-vec (xdr)
  (subseq (xdr-vec xdr) 0 (xdr-size xdr)))

(defun xdr-expand-check (xdr more)
  (if (> (+ (xdr-size xdr) more) (length (xdr-vec xdr)))
      (progn
	(format t "expanding xdr~%")
	(setf (xdr-vec xdr) (concatenate '(vector (unsigned-byte 8)) (xdr-vec xdr) (make-vec *xdrdefaultsize*))))))

(defun xdr-advance (xdr size)
  (decf (xdr-size xdr) size)
  (incf (xdr-pos xdr) size))

(defun xdr-unsigned-int (xdr &optional int)
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (unless (>= (xdr-size xdr) 4)
        (error "xdr-unsigned-int: No data left in this xdr!"))
      (let ((res 0)
            (shift 24))
        (dotimes (i 4)
          (incf res (ash (aref (xdr-vec xdr) (+ i (xdr-pos xdr))) shift))
          (decf shift 8))
	(xdr-advance xdr 4)
        res))
     ((eq direction :build)
      (xdr-expand-check xdr 4)
      (let ((shifts -24)
            (mask #xff000000))
        (dotimes (i 4)
          (setf (aref (xdr-vec xdr) (+ (xdr-size xdr) i)) (ash (logand int mask) shifts))
          (incf shifts 8)
          (setf mask (ash mask -8)))
	(incf (xdr-size xdr) 4))))))

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

(defun xdr-opaque-fixed (xdr &key vec len)
  (let ((direction (xdr-direction xdr))
        newvec
	plen)
    (cond
     ((eq direction :extract)
      (unless len
        (error "xdr-opaque-fixed: 'len' parameter is required"))
      (if (> len (xdr-size xdr))
          (error "xdr-opaque-fixed: Not enough data left!"))
      (setf newvec
	(subseq (xdr-vec xdr) (xdr-pos xdr) (+ (xdr-pos xdr) len)))
      (xdr-advance xdr (compute-padded-len len))
      newvec)
     ((eq direction :build)
      (unless vec
        (error "xdr-opaque-fixed: 'vec' parameter is required"))
      (unless len
        (setf len (length vec)))
      (setf plen (compute-padded-len len))
      (xdr-expand-check xdr plen)
      (dotimes (i len)
        (setf (aref (xdr-vec xdr) (+ i (xdr-size xdr))) (aref vec i)))
      (incf (xdr-size xdr) plen)))))

      
(defun xdr-opaque-variable (xdr &key vec len)
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (setf len (xdr-int xdr))
      (if (> len (xdr-size xdr))
          (error "xdr-opaque-variable: Not enough data left!"))
      (xdr-opaque-fixed xdr :len len))
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
  (xdr-int xdr 0) ;; placeholder since we don't know the count yet
  (xdr-expand-check xdr (compute-padded-len count))
  (let* ((newpos 
	  (read-sequence (xdr-vec xdr) stream 
			:start (xdr-size xdr)
			:end (+ (xdr-size xdr) count)))
	 (bytesread (- newpos (xdr-size xdr))))
    ;; now backtrack and update the count
    (decf (xdr-size xdr) 4)
    (xdr-int xdr bytesread)
    ;; now update the size
    (incf (xdr-size xdr) bytesread)))

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
  flavor ;; int
  body) ;; variable up to 400 bytes  (xdr)

(defun xdr-opaque-auth (xdr &optional flavor body)
  (let ((direction (xdr-direction xdr)))
    (cond
     ((eq direction :extract)
      (let ((oa (make-opaque-auth)))
        (setf (opaque-auth-flavor oa) (xdr-int xdr))
        ;;(format t "xdr-opaque-auth: auth-flavor is ~D~%" (opaque-auth-flavor oa))
        (setf (opaque-auth-body oa) (create-xdr :vec (xdr-opaque-variable xdr)))
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


(defun make-vec (size &key init)
  (if init
      (make-array size :element-type '(unsigned-byte 8) :initial-element init)
    (make-array size :element-type '(unsigned-byte 8))))

(defun compute-padded-len (len)
  (let ((remainder (mod len 4)))
    (+ len (if (> remainder 0) (- 4 remainder) 0))))

(defun xdr-string (xdr &optional string)
  (let ((direction (xdr-direction xdr))
        len
	plen
        newstring)
    (cond 
     ((eq direction :extract)
      (setf len (xdr-int xdr))
      (setf newstring (make-string len))
      (if (< (xdr-size xdr) len)
          (error "xdr-string: Not enough data left!"))
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
        (setf (aref (xdr-vec xdr) (+ (xdr-size xdr) i)) (char-code (schar string i))))
      (incf (xdr-size xdr) plen)))))

      
      

(defun xdr-xdr (xdr &optional xdr2)
  (let ((direction (xdr-direction xdr))
        res)
    (cond
     ((eq direction :build)
      (unless xdr2
        (error "xdr-xdr: 'xdr2' parameter is required"))
      (let ((size (xdr-size xdr2)))
	(xdr-expand-check xdr size)
	;; do the copy....
	(dotimes (i size)
	  (setf (aref (xdr-vec xdr) (+ i (xdr-size xdr)))
	    (aref (xdr-vec xdr2) (+ i (xdr-pos xdr2))))) 
	(incf (xdr-size xdr) size)))
     ((eq direction :extract)
      (setf res 
	(create-xdr :vec (subseq (xdr-vec xdr) (xdr-pos xdr) (+ (xdr-pos xdr) (xdr-size xdr)))))
      (setf (xdr-pos xdr) (xdr-size xdr))
      res))))
      

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

