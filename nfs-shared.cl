(in-package :user)

;; $Id: nfs-shared.cl,v 1.4 2005/06/28 16:49:09 dancy Exp $

;; This file contains stuff that is shared between the nfs server code
;; and the configuration app code.

(eval-when (compile eval load)
  (require :regexp2)
  (require :shell)
  (use-package :excl.shell))

(defun cleanup-dir (dir)
  ;; n: => n:\
  ;; n:\src\ => n:\src
  ;; \\foo\bar => \\foo\bar\
  
  ;; convert forward slashes to back slashes
  (setq dir (namestring (pathname dir)))
  
  (if* (=~ "^[A-Za-z]:$" dir)
     then (+= dir "\\")
   elseif (=~ "([A-Za-z]:.+)\\\\$" dir)
     then $1
   elseif (=~ "(\\\\\\\\[^\\\\]+\\\\[^\\\\]+)$" dir)
     then (+= $1 "\\")
     else dir))

#+ignore
(defun cleanup-dir (dir)
  ;; Change all forward slashes to backslashes.  
  (setf dir (substitute #\\ #\/ dir))
  
  (multiple-value-bind (matched dummy remainder)
      (match-re "^[a-z]:(.*)" dir :case-fold t)
    (declare (ignore dummy))
    (if (not matched)
	(error "~A is not a valid directory specification." dir))
    (cond
     ((string= remainder "")
      (concatenate 'string dir "\\"))
     ((string= remainder "\\")
      dir)
     ((char= (schar dir (1- (length dir))) #\\)
      ;; strip trailing backslash
      (subseq dir 0 (1- (length dir))))
     (t ;; already in canonical form
      dir))))
      
#+ignore
(defun test-cleanup-dir ()
  (let (val)
    (dolist (x (list (cons "n:\\" "n:")
		     (cons "n:\\" "n:/")
		     (cons "n:\\" "n:\\")
		     (cons "n:\\src" "n:/src")
		     (cons "n:\\src" "n:\\src")
		     (cons "n:\\src" "n:\\src\\")
		     (cons "n:\\src" "n:/src/")))
      (when (not (string= (car x)
			  (setq val (cleanup-dir (cdr x)))))
	(error "(cleanup-dir ~s): expected ~s, got ~s."
	       (cdr x)
	       (car x)
	       val)))))

(defvar *log-stream* nil)

(defun logit (format-string &rest format-args)
  (when *log-stream*
    (apply #'format *log-stream* format-string format-args)
    (force-output *log-stream*)))

(defvar *nfs-debug-stream* nil)

(eval-when (compile eval load)
  (require :streamc) ;; for make-broadcast-stream
  )

(defvar *log-file* "sys:nfsdebug.txt")

(defun setup-logging (&optional reopen)
  (declare (special *nfs-debug*))
  (when reopen
    (when *nfs-debug-stream* (close *nfs-debug-stream*))
    (setq *log-stream* nil))
  
  (when (null *log-stream*)
    (setq *log-stream* *initial-terminal-io*)
    (when *nfs-debug*
      (setq *nfs-debug-stream*
	(open *log-file* :direction :output :if-exists :append
	      :if-does-not-exist :create))
      (setq *log-stream*
	(make-broadcast-stream *log-stream* *nfs-debug-stream*))
      (logit "Log file: ~a~%"
	     (translate-logical-pathname (pathname *log-file*))))))
