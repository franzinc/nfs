;; portmapper

(defconstant *pmapport* 111)
(defconstant *pmapprog* 100000)
(defconstant *pmapvers* 2)


(defstruct mapping
  prog
  vers
  prot
  port
  )

(defconstant IPPROTO_TCP 6)
(defconstant IPPROTO_UDP 17)

(defparameter *mappings* nil)
(defparameter *pmap-tcp-socket* nil)
(defparameter *pmap-udp-socket* nil)

(defun make-pmap-sockets ()
  (unless *pmap-tcp-socket*
    (setf *pmap-tcp-socket*   
      (socket:make-socket :type :stream
                          :connect :passive
                          :local-port *pmapport*
                          :reuse-address t)))
  (unless *pmap-udp-socket*
    (setf *pmap-udp-socket*
      (socket:make-socket :type :datagram
                          :local-port *pmapport*))))
      

(defun close-pmap-sockets ()
  (when *pmap-tcp-socket*
    (close *pmap-tcp-socket*)
    (setf *pmap-tcp-socket* nil))
  (when *pmap-udp-socket*
    (close *pmap-udp-socket*)
    (setf *pmap-udp-socket* nil)))


(defun portmapper ()
  (make-pmap-sockets)
  (portmap-add-program *pmapprog* *pmapvers* *pmapport* IPPROTO_TCP)
  (portmap-add-program *pmapprog* *pmapvers* *pmapport* IPPROTO_UDP)
  (let ((server (make-rpc-server :tcpsock *pmap-tcp-socket* :udpsock *pmap-udp-socket*)))
    (loop
      (multiple-value-bind (xdr peer)
          (rpc-get-message server)
        (portmap-message-handler xdr peer)))))
      
(defun portmap-message-handler (xdr peer)
  (let (msg cbody)
    (setf msg (create-rpc-msg xdr))
    (setf cbody (rpc-msg-cbody msg))
    (pprint-cbody cbody)
    (unless (= (rpc-msg-mtype msg) 0)
      (error "Unexpected data!"))
    (when (and (= (call-body-prog cbody) *pmapprog*)
               (= (call-body-vers cbody) *pmapvers*))
      (cond
       ((= (call-body-proc cbody) 4)
        (portmap-dump peer (rpc-msg-xid msg)))
       ((= (call-body-proc cbody) 3)
        (portmap-getport peer (rpc-msg-xid msg) (call-body-params cbody)))
       (t 
        (format t "portmap: unhandled procedure ~D~%" (call-body-proc cbody))))))) ;; should send a negative response

(defun portmap-verf ()
  (let ((xdr (create-xdr :direction :build)))
    (xdr-auth-null xdr)
    xdr))

(defun portmap-dump (peer xid)
  (format t "portmap-dump~%")
  (let ((xdr (create-xdr :direction :build)))
    (dolist (mapping *mappings*)
      (xdr-int xdr 1) ;; indicate that data follows
      (xdr-int xdr (mapping-prog mapping))
      (xdr-int xdr (mapping-vers mapping))
      (xdr-int xdr (mapping-prot mapping))
      (xdr-int xdr (mapping-port mapping)))
    (xdr-int xdr 0) ;; no more data
    (send-successful-reply peer xid (portmap-verf) xdr)))

(defun portmap-getport (peer xid params)
  (let* ((m (make-mapping-from-xdr params))
         (m2 (locate-matching-mapping m))
         (xdr (create-xdr :direction :build)))
    (format t "portmap-getport: ~A~%" m)
    (if m2
        (progn
          (format t "Program found. Returning port ~D~%" 
            (mapping-port m2))
          (xdr-int xdr (mapping-port m2)))
      (progn
        (format t "Program not found.  Returning 0~%")
        (xdr-int xdr 0)))
    (send-successful-reply peer xid (portmap-verf) xdr)))
          
    
    
(defun locate-matching-mapping (m)
  (dolist (m2 *mappings*)
    (when (and
           (= (mapping-prog m) (mapping-prog m2))
           (= (mapping-vers m) (mapping-vers m2))
           (= (mapping-prot m) (mapping-prot m2)))
      (return-from locate-matching-mapping m2)))
  nil)
      
    
  
(defun make-mapping-from-xdr (xdr)
  (let ((m (make-mapping))) 
    (setf (mapping-prog m) (xdr-int xdr))
    (setf (mapping-vers m) (xdr-int xdr))
    (setf (mapping-prot m) (xdr-int xdr))
    (setf (mapping-port m) (xdr-int xdr))
    m))
  

(defun portmap-add-program (prog vers port proto)
  (let ((mapping (make-mapping
                  :prog prog
                  :vers vers
                  :prot proto
                  :port port)))
    (unless (find mapping *mappings* :test #'equalp)
      (push mapping *mappings*))))
  
(defstruct callargs
  prog
  vers
  proc
  args)

(defstruct callresult
  port
  res)

