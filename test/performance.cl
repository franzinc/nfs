#! /fi/cl/9.0/bin/mlisp -#!

(in-package :user)

(defstruct datum
  export-name
  testpath
  iteration
  nfs-version
  blocksize
  transport
  duration
  read-bytes
  rate)

(defun read-data-groups (data-file &aux form (res '()))
  (with-open-file (s data-file)
    (loop
      (setq form (read s nil s))
      (when (eq form s) (return res))
      (push (mapcar (lambda (x)
		      (apply #'make-datum x))
		    form)
	    res))))

(defun calc-avg-rate (group)
  ;; group is a list of datum's and we return the avg rate, ignoring the
  ;; high and low rate
  (flet ((remove-high-and-low (group)
	   ;; Toss the low and high rate item
	   (setq group (sort (copy-list group) #'< :key #'datum-rate))
	   (setq group (cdr group))
	   (nbutlast group 1)))
    (/ (reduce #'+ (mapcar #'datum-rate (remove-high-and-low group)))
       ;; length - 2 (for the 2 items removed)
       (- (length group) 2))))

(defun doit (ref new)
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
	
	  (ref-rate #3=(calc-avg-rate ref-group) #3#)
	  (new-rate #4=(calc-avg-rate new-group) #4#))
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

(doit "test/performance.log.5.1"
      "test/performance.log")
(exit 0 :quiet t)
