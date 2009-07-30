(in-package :cg-user)

(eval-when (compile load eval)
  (require :sock))

(defstruct network-address
  network
  mask)

(defun my-dotted-to-ipaddr (addr)
  (if (not (match-regexp "^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$" addr))
      (error "Invalid address specification"))
  (socket:dotted-to-ipaddr addr))

;; Acceptable formats:
;; a.b.c.d
;; a.b.c.d/x
;; a.b.c.d/x.y.z.w
;; t  (shortcut for 0.0.0.0/0)

(defun parse-addr (addr)
  ;; convenience
  (if (eq addr t)
      (setf addr "0.0.0.0/0"))
  (setf addr (string-trim '(#\space) addr))
  (if (string= addr "")
      (error "blank string passed to parse-addr"))
  (let* ((slashpos (position #\/ addr))
         (mask #xffffffff)
         (network (my-dotted-to-ipaddr 
                   (subseq addr 0 (or slashpos (length addr))))))
    (if* slashpos
       then
            (setf addr (subseq addr (1+ slashpos)))
            (setf mask 
              (if (position #\. addr)
                  (my-dotted-to-ipaddr addr)
                (masklength-to-mask addr)))
            (setf network (logand network mask)))
    (make-network-address
     :network network
     :mask mask)))

(defun masklength-to-mask (value)
  (if (stringp value)
      (setf value (parse-integer value)))
  (if (or (< value 0) (> value 32))
      (error "Invalid mask length: ~A" value))
  (- #xffffffff (1- (expt 2 (- 32 value)))))


