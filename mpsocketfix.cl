;; mp socket fix

;; patch for bug10759

(in-package :mp)

;; this was modified
(eval-when (compile)
  (defmacro stream-or-fd-fd (s)
    (unless (symbolp s)
      (error "stream-or-fd requires a symbol"))
    `(if* (fixnump ,s)
	then ,s
      elseif (streamp ,s)
	then (stream-input-fn ,s)
	else (let ((fd (wait-for-input-fd ,s)))
	       (if* (and fd (< fd 0))
		  then nil
		elseif (and fd (>= fd 0))
		  then fd
		  else (excl::.type-error ,s
					  '(or stream fixnum)))))))






; these are unmodified, but use the above:

(defun wait-for-input-available (streams
				 &key (wait-function #'stream::stream-listen)
				      (whostate "waiting for input")
				      timeout)
  (declare (optimize (speed 3)))
  (if (not (listp streams))
      (setq streams (list streams)))
  (let ((fds sys::*stack-group-watchfor-fds*)
	(ret-list (list nil)))		; Return value set in real wait fun
    (dolist (stream-or-fd streams)
      (let ((fd (stream-or-fd-fd stream-or-fd)))
	(when fd
	  (push fd fds))))
    (let ((sys::*stack-group-watchfor-fds* fds))
      (if* timeout
	 then (process-wait-with-timeout
	       whostate timeout #'multi-stream-wait-for-input-wait-function
	       streams wait-function ret-list)
	 else (process-wait whostate
			    #'multi-stream-wait-for-input-wait-function
			    streams wait-function ret-list)))
    (cdr ret-list)))


(defun wait-for-input-available-one (fd stream wait-function)
  (declare (optimize (speed 3)))
  (let ((sys::*stack-group-watchfor-fds*
	  (cons (stream-or-fd-fd fd) sys::*stack-group-watchfor-fds*)))
    (process-wait "waiting for input" wait-function stream)))
