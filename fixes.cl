(in-package :excl)

#-(version>= 6 1)
(eval-when (compile load eval)
  (defconstant excl::sc-rmdir 75))

#-(version>= 6 1)
(defun rmdir (directory)
  (let ((dir (truename directory)))
    (multiple-value-bind (res errcode)
	(with-native-string (x (namestring dir))
	  (excl::.primcall 'sys::lisp-syscall #.sc-rmdir x))
      (if* errcode
	 then (error 'file-error
		     :pathname dir
		     :errno errcode
		     :format-control "Could not remove directory: ~a."
		     :format-arguments (list (er-number-to-string errcode)))
	 else res))))
