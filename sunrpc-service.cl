(in-package :user)

;; $Id: sunrpc-service.cl,v 1.4 2006/05/06 19:42:16 dancy Exp $

;; Service stuff

(defmacro with-rpc-socket ((service sym &rest params) &body body)
  (let ((c (gensym)))
    `(let ((,sym (handler-case (socket:make-socket ,@params)
		   (error (,c)
		     (bailout "
~a: Error while creating socket: ~a~%" ,service ,c)))))
       (unwind-protect
	   (progn ,@body)
	 (ignore-errors (close ,sym))
	 (ignore-errors (close ,sym :abort t))))))

(defmacro def-rpc-program-1 ((program prognum versions usock tsock) 
			     &body body)
  `(with-rpc-socket (,program ,tsock :type :hiper :connect :passive)
     (with-rpc-socket (,program ,usock :type :datagram)
       (logit "~a: Using UDP port ~d~%" 
	      ,program (socket:local-port ,usock))
       (logit "~a: Using TCP port ~d~%" 
	      ,program (socket:local-port ,tsock))
       (with-portmapper-mapping (,prognum (quote ,versions)
					  (socket:local-port ,usock) 
					  IPPROTO_UDP)
	 (with-portmapper-mapping (,prognum (quote ,versions)
					    (socket:local-port ,tsock)
					    IPPROTO_TCP)
	   ,@body)))))

;; 'program' is a string.
(defmacro def-rpc-program-main (program prognum proc-versions usock tsock
				lowest-version highest-version)
  (let ((server (gensym))
	(xdr (gensym))
	(peer (gensym))
	(msg (gensym))
	(cbody (gensym))
	(vers (gensym))
	(res (gensym))
	(params (gensym))
	(init-func (intern (concatenate 'string program "-init")))
	version-cases)

    (dolist (vdef proc-versions)
      (let (proc-cases)
	(dolist (procdef (cdr vdef))
	  (push
	   `(,(first procdef)
	     (setf func (quote ,(second procdef)))
	     (setf args-decoder (quote ,(xdr-prepend-xdr (third procdef))))
	     (setf res-encoder (quote ,(xdr-prepend-xdr (fourth procdef)))))
	   proc-cases))
	
	(push `(t 
		(logit "~
~a: ~a requested procedure ~d, version ~a, which is unavailable.~%" 
		       ,program
		       (socket:ipaddr-to-dotted (rpc-peer-addr ,peer))
		       (call-body-proc ,cbody) (call-body-vers ,cbody))
		       
		(rpc-send-proc-unavail ,peer (rpc-msg-xid ,msg)
				       *nullverf*)
		(return))
	      proc-cases)
	
	(setf proc-cases (nreverse proc-cases))
	(push 
	 `(,(car vdef)
	   (case procnum
	     ,@proc-cases))
	 version-cases)))
    
    (setf version-cases (nreverse version-cases))
    
    `(let ((,server (make-rpc-server :tcpsock ,tsock
				     :udpsock ,usock)))
       (if (fboundp ',init-func)
	   (funcall ',init-func))
       
       (loop
	 (block nil
	   (multiple-value-bind (,xdr ,peer)
	       (rpc-get-message ,server)
	     (let* ((,msg (create-rpc-msg ,xdr))
		    (,cbody (rpc-msg-cbody ,msg)))
	       ;; sanity checks first
	       (if* (null ,cbody)
		  then (logit "~a: Invalid message from ~A~%" 
			      ,program 
			      (socket:ipaddr-to-dotted 
			       (rpc-peer-addr ,peer)))
		       (return))
	       
	       (if* (/= (call-body-prog ,cbody) ,prognum)
		  then (logit "~
~a: Sending program unavailable response for prog=~D to ~A~%"
			      ,program
			      (call-body-prog ,cbody)
			      (socket:ipaddr-to-dotted 
			       (rpc-peer-addr ,peer)))
		       (rpc-send-prog-unavail ,peer (rpc-msg-xid ,msg) 
					      *nullverf*)
		       (return))
	       
	       (let ((,vers (call-body-vers ,cbody))
		     (procnum (call-body-proc ,cbody))
		     func args-decoder res-encoder)
		 (case ,vers
		   ,@version-cases)
		 
		 (if* (null func)
		    then (logit "~
~a: Sending program version mismatch response (requested version was ~D) to ~A~%" 
				,program
				,vers
				(socket:ipaddr-to-dotted 
				 (rpc-peer-addr ,peer)))
			 (rpc-send-prog-mismatch 
			  ,peer (rpc-msg-xid ,msg) *nullverf* 
			  ,lowest-version ,highest-version)
			 (return))
		 
		 ;; Let 'er rip.
		 (with-successful-reply (,res ,peer 
					      (rpc-msg-xid ,msg) 
					      *nullverf*)
		   (with-xdr-xdr ((call-body-params ,cbody) 
				  :name ,params)
		     (funcall res-encoder ,res 
			      (funcall func 
				       (funcall args-decoder ,params)  ;; arg
				       ,vers
				       ,peer
				       ,cbody))))))))))))

(defmacro def-rpc-program ((prgname prognum) definitions)
  (let ((program (symbol-name prgname))
	(tsock (gensym))
	(usock (gensym)))

    (let (all-versions ;; for use in portmapper call
	  proc-versions) ;; for use in main loop
      (dolist (vdef definitions)
	(let ((versions (first vdef)))
	  (if (not (listp versions))
	      (setf versions (list versions)))
	  
	  (setf all-versions (append all-versions versions))
	  
	  (push (cons versions (rest vdef)) proc-versions)))
      
      (setf all-versions (sort all-versions #'<))
      (setf proc-versions (nreverse proc-versions))
      
      `(defun ,prgname ()
	 (declare (optimize (speed 3)))
	 (def-rpc-program-1 (,program ,prognum ,all-versions ,usock ,tsock)
	     (def-rpc-program-main ,program ,prognum ,proc-versions 
				   ,usock ,tsock
				   ,(first all-versions)
				   ,(car (last all-versions))))))))
