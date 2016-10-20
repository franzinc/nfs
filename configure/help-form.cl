;; Code for the dialog :help-form

(in-package :common-graphics-user)

(defun do-help (thing)
  (declare (ignore thing))
  (let ((form (help-form))
        (textfile (merge-pathnames "configuration.txt" *progpath*)))
    ;; for testing in development
    (if (not (probe-file textfile))
        (setf textfile "configuration.txt"))
    (setf (value (my-find-component :text form)) (file-contents textfile))))
    
    
(defun help-form-close-help-button-on-change (widget
                                              new-value
                                              old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (user-close (parent widget))
  t)
