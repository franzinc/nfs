#! /fi/cl/10.1/bin/mlisp -#C

(eval-when (compile eval load)
  (require :shell)
  (use-package :excl.shell))

(in-package :user)

(eval-when (compile eval load)
  (require :osi))

(defstruct datum
  export-name
  testpath
  iteration
  nfs-version
  blocksize
  transport
  duration
  read-bytes
  rate
  reads)

(defun read-data-groups (data-file &aux form (res '()))
  (with-open-file (s data-file)
    (loop
      (setq form (read s nil s))
      (when (eq form s) (return (nreverse res)))
      (push (mapcar (lambda (x)
		      (apply #'make-datum x))
		    form)
	    res))))

(defun calc-avg-rate (group)
  (assert group)
  ;; group is a list of datum's and we return the avg rate, ignoring the
  ;; high and low rate
  (flet ((remove-high-and-low (group)
	   ;; Toss the low and high rate item
	   (setq group (sort (copy-list group) #'< :key #'datum-rate))
	   (setq group (cdr group))
	   (nbutlast group 1)))
    (let ((len (length group))
	  (group group))
      (when (> (length group) 3)
	(setq group (remove-high-and-low group))
	;; length - 2 (for the 2 items removed)
	(decf len 2))
      (/ (reduce #'+ (mapcar #'datum-rate group))
	 len))))

(defun print-stats (ref new)
  (flet ((%change (ref new)
	   (let* ((diff (- new ref))
		  (%change (* 100.0 (/ (abs diff) ref))))
	     ;; Since we're dealing with rates, higher is better.
	     ;; If diff is positive, then the % change is positive (faster),
	     ;; otherwise it's negative (slower)
	     (if* (minusp diff)
		then ;; slower, show as negative
		     (- %change)
		else ;; Slower, show as positive
		     %change)))
	 (datum-pretty-name (datum)
	   (format nil "NFSv~d/~a, BS=~d"
		   (datum-nfs-version datum)
		   (datum-transport datum)
		   (datum-blocksize datum))))
    (format t "~30a ~20a~%" "" "     rates: KB/sec")
    (format t "~30a ~10@a ~10@a ~11@a~%" "what" "ref" "new" "%change")
    (do* ((ref-groups (read-data-groups ref)
		      (cdr ref-groups))
	  (new-groups (read-data-groups new)
		      (cdr new-groups))
	  (ref-group #1=(car ref-groups) #1#)
	  (new-group #2=(car new-groups) #2#)
	
	  (ref-rate #3=(and ref-group (calc-avg-rate ref-group)) #3#)
	  (new-rate #4=(and new-group (calc-avg-rate new-group)) #4#))
	((null ref-groups)
	 (when (not (null new-groups))
	   (error "new-groups longer than ref-groups: ~s." new-groups)))
      (assert (string= (datum-pretty-name (car ref-group))
		       (datum-pretty-name (car new-group))))
      (format t "~&")
      (format t "~30a ~10,2f ~10,2f ~10,2f%~%"
	      (datum-pretty-name (car ref-group))
	      ref-rate
	      new-rate
	      (%change ref-rate new-rate)))))

#+ignore
(print-stats
 "5.1"					; reference results
 "6.3.0.rc1"				; test results
 )

(sys:with-command-line-arguments ("v" verbose) (rest)
  (declare (ignore verbose))
  (when (not (eql 2 (length rest)))
    (die "expected only 2 arguments"))
  (let* ((ref (merge-pathnames (first rest) "results/"))
	 (new (merge-pathnames (second rest) "results/")))
    (or (probe-file ref) (error-die "~a does not exist." ref))
    (or (probe-file new) (error-die "~a does not exist." new))
    (format t ";; ~a to ~a performance comparison:~%~%"
	    (first rest)
	    (second rest))
    (print-stats ref new)))
