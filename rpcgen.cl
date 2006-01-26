(in-package :user)

(eval-when (compile load eval)
  (require :osi))

(defmacro with-input-from-subprocess ((cmd var) &body body)
  `(multiple-value-bind (,var dummy pid)
       (run-shell-command ,cmd :output :stream :wait nil)
     (declare (ignore dummy))
     (unwind-protect
	 (progn ,@body)
       (close ,var)
       (sys:reap-os-subprocess :pid pid))))
    
(defun rpcgen (filename)
  ;; Generate an error message if the file doesn't exist.
  (close (open filename))

  (with-input-from-subprocess
      ((format nil "bash -c 'cpp ~a | grep -v ^#'" filename)
       stream)

    (while (rpcgen-peekchar stream)
      (let ((token (rpcgen-lex stream :word)))
	(cond
	 ((string= token "const")
	  (rpcgen-const stream))
	 ((string= token "enum")
	  (rpcgen-enum stream))
	 ((string= token "typedef")
	  (rpcgen-typedef stream))
	 ((string= token "struct")
	  (rpcgen-struct stream))
	 ((string= token "union")
	  (rpcgen-union stream))
	 ((string= token "program")
	  (rpcgen-program stream (pathname-name filename)))
	 (t
	  (error "Unexpected token: ~a" token)))))))

(defun rpcgen-lex (stream type)
  (let (res char)
    
    ;; Skip any whitespace
    (while (setf char (read-char stream nil nil))
      (if (not (excl::whitespace-char-p char))
	  (return)))
    
    (if char
	(unread-char char stream))
    
    (while (setf char (read-char stream nil nil))
      (if* (or (alphanumericp char) (char= char #\_))
	 then (push char res)
	 else (return)))
    
    (if* res 
       then ;; we just terminated an identifier or reserved word
	    (if char
		(unread-char char stream))
	    
	    (if (not (eq type :word))
		(error "Got a word when looking for ~s" type))
	    
	    (make-array (length res) :element-type 'character
			 :initial-contents (nreverse res))
       else ;; EOF or special character.  Return it.
	    (if (not (eq char type))
		(error "Got ~s when looking for ~s" char type))
	    
	    char)))

(defun rpcgen-peekchar (stream)
  (let (char)
    ;; Skip any whitespace
    (while (setf char (read-char stream nil nil))
      (if (not (excl::whitespace-char-p char))
	  (return)))
    
    (if char
	(unread-char char stream))
    
    char))

(defun rpcgen-const (stream)
  (let ((varname (constantify-string (rpcgen-lex stream :word))))
    (rpcgen-lex stream #\=)
    (let ((expression (read stream)))
      (rpcgen-lex stream #\;)
      (format t "(eval-when (compile load eval)~%  (defconstant ~a ~a))~%~%" 
	      varname expression))))

(defun rpcgen-opaque (stream)
  (let ((typename (lispify-string (rpcgen-lex stream :word))))
    (rpcgen-lex stream #\<)
    (rpcgen-lex stream :word)
    (rpcgen-lex stream #\>)
    (rpcgen-lex stream #\;)
    
    (format t "~
(defun xdr-~a (xdr &optional vec)
  (ecase (xdr-direction xdr)
    (:build 
     (xdr-opaque-variable xdr :vec vec))
    (:extract
     (xdr-opaque-variable xdr))))

"  typename)))

(defun rpcgen-typedef (stream)
  (let ((token (rpcgen-lex stream :word)))
    (if* (string= token "opaque")
       then (rpcgen-opaque stream)
       else (error "Unexpected word: ~a" token))))

(defun rpcgen-enum (stream)
  (let ((typename (lispify-string (rpcgen-lex stream :word))))
    (rpcgen-lex stream #\{)

    (format t ";; enum ~a~%(eval-when (compile load eval)" typename)
    
    (loop
      (let ((varname (constantify-string (rpcgen-lex stream :word)))
	    value)
	(rpcgen-lex stream #\=)
	(setf value (parse-integer (rpcgen-lex stream :word)))
	(format t "~% (defconstant ~a ~a)" varname value)
	(if (not (eq (rpcgen-peekchar stream) #\,))
	    (return))
	(rpcgen-lex stream #\,)))
    
    (rpcgen-lex stream #\})	
    (rpcgen-lex stream #\;)
    
    (format t ")~%~%")
    
    (format t "(defun xdr-~a (xdr &optional int)~% (xdr-int xdr int))~%~%"
	    typename)))

(defun rpcgen-struct (stream)
  (let ((typename (lispify-string (rpcgen-lex stream :word)))
	(first t)
	(indent "                          "))
    (rpcgen-lex stream #\{)
    (format t "(defxdrstruct ~a (" typename)
    
    (while (not (eq (rpcgen-peekchar stream) #\}))
      (multiple-value-bind (slottype slotname varfixed len)
	  (rpcgen-parse-type-and-name stream)
	(rpcgen-lex stream #\;)
	(if* first
	   then (setf first nil)
	   else (format t "~%~a" indent))
	(format t "(~a ~a" slottype slotname)
	(if varfixed
	    (format t " ~s ~a" varfixed len))
	(format t ")")))
    
    (format t "))~%~%")
    
    (rpcgen-lex stream #\})
    (rpcgen-lex stream #\;)))

(defun rpcgen-union (stream)
  (let ((typename (lispify-string (rpcgen-lex stream :word))))
    (if (not (string= (rpcgen-lex stream :word) "switch"))
	(error "Syntax error: Expected 'switch'"))
    
    (format t "(defxdrunion ~a " typename)
    
    (rpcgen-lex stream #\()
    (multiple-value-bind (type name)
	(rpcgen-parse-type-and-name stream)
      (format t "(~a ~a)~% (~%" type name))
    (rpcgen-lex stream #\))
    (rpcgen-lex stream #\{)
    
    (while (not (eq (rpcgen-peekchar stream) #\}))
      (let ((token (rpcgen-lex stream :word)))
	(cond 
	 ((string= token "case")
	  (let ((case (constantify-string (rpcgen-lex stream :word))))
	    (rpcgen-lex stream #\:)
	    (multiple-value-bind (type slot)
		(rpcgen-parse-type-and-name stream)
	      (rpcgen-lex stream #\;)
	      (format t "  (#.~a ~a ~a)~%" case type slot))))
	 ((string= token "default")
	  (rpcgen-lex stream #\:)
	  (multiple-value-bind (type slot)
	      (rpcgen-parse-type-and-name stream)
	    (rpcgen-lex stream #\;)
	    (format t "  (:default ~a" type)
	    (if (string/= type "void")
		(format t " ~a" slot))
	    (format t ")~%")))
	 (t
	  (error "Unexpected word: ~a" token)))))
    
    (format t " ))~%~%")
      
    (rpcgen-lex stream #\})
    (rpcgen-lex stream #\;)))

(defun rpcgen-parse-type (stream)
  (let ((type (rpcgen-lex stream :word)))
    (if (string= type "struct")
	(setf type (rpcgen-lex stream :word)))
    (lispify-string type)))

;; returns values:
;;  type, name, variable/fixed, length 
(defun rpcgen-parse-type-and-name (stream)
  (let ((type (rpcgen-parse-type stream)))
    (if* (string= type "void")
       then "void"
       else (let ((name (lispify-string (rpcgen-lex stream :word)))
		  (char (rpcgen-peekchar stream))
		  variable-fixed len)
	      (if* (or (eq char #\[) (eq char #\<))
		 then (rpcgen-lex stream char)
		      (setf len (constantify-string (rpcgen-lex stream :word)))
		      (ecase char
			(#\[
			 (setf char #\])
			 (setf variable-fixed :fixed))
			(#\<
			 (setf char #\>)
			 (setf variable-fixed :variable)))
		      (rpcgen-lex stream char))
	      
	      (when (string= type "string")
		(if (eq variable-fixed :fixed)
		    (error "string ~a[~a]: Ambigious.  Fixed size string or array of strings? Aborting" name len))
		(setf variable-fixed nil))
		      
	      (values type name variable-fixed len)))))

(defun rpcgen-program (stream main-prg-name)
  (let (prgname prognum vers-defs)
    
    (setf prgname (constantify-string (rpcgen-lex stream :word)))
    
    (rpcgen-lex stream #\{)
    
    (while (not (eq (rpcgen-peekchar stream) #\}))
      (push (rpcgen-version stream) vers-defs))
    
    (rpcgen-lex stream #\})
    (rpcgen-lex stream #\=)
    
    (setf prognum (parse-integer (rpcgen-lex stream :word)))
    (rpcgen-lex stream #\;)
    
    (setf vers-defs (nreverse vers-defs))
    
    (format t "(eval-when (compile load eval)~%")
    (format t " (defconstant ~a ~d)~%" prgname prognum)
    (dolist (vdef vers-defs)
      (format t " (defconstant ~a ~d)~%" (first vdef) (second vdef))
      (dolist (func (third vdef))
	(format t " (defconstant ~a ~d)~%" 
		(constantify-string (first func))
		(second func))))
      
    (format t ")~%~%")
    
    (format t "(def-rpc-program (~a #.~a)~%" main-prg-name prgname)
    (format t "  (~%")
    (dolist (vdef vers-defs)
      (format t "   (#.~a~%" (first vdef))
      (dolist (func (third vdef))
	(format t "     (#.~a ~a ~a ~a)~%" 
		(constantify-string (first func))
		(first func)
		(third func)
		(fourth func)))
      (format t "   )~%"))
    (format t "  ))~%")))

;; returns a list of vers-const, versnum, list-of-funcs
(defun rpcgen-version (stream)
  (let (vers-const funcs versnum)
    
    (if (not (equalp (rpcgen-lex stream :word) "version"))
	(error "Syntax error: Expected 'version'"))
    
    (setf vers-const (constantify-string (rpcgen-lex stream :word)))
    
    (rpcgen-lex stream #\{)

    (while (not (eq (rpcgen-peekchar stream) #\}))
      (push (rpcgen-function stream) funcs))
    
    (rpcgen-lex stream #\})
    (rpcgen-lex stream #\=)
    (setf versnum (parse-integer (rpcgen-lex stream :word)))
    (rpcgen-lex stream #\;)
    
    (list vers-const versnum (nreverse funcs))))

;; Returns list of the name, procnum, arg type, and return type
(defun rpcgen-function (stream)
  (let ((ret-type (rpcgen-parse-type stream))
	(func-name (lispify-string (rpcgen-lex stream :word)))
	arg-type procnum)
    (rpcgen-lex stream #\()
    
    (setf arg-type (rpcgen-parse-type stream))
    
    (rpcgen-lex stream #\))
    (rpcgen-lex stream #\=)
    
    (setf procnum (parse-integer (rpcgen-lex stream :word)))
    
    (rpcgen-lex stream #\;)

    (list func-name procnum arg-type ret-type)))

(defun lispify-string (string)
  (string-downcase (substitute #\- #\_ string)))

(defun constantify-string (string)
  (if (ignore-errors (parse-integer string))
      string
    (concatenate 'string "*" (lispify-string string) "*")))
