;; mountd
;; $Id: mountd.cl,v 1.5 2001/06/07 19:09:38 dancy Exp $

(in-package :user)

(defconstant MNTPATHLEN 1024) ;; max number of bytes in a pathname argument
(defconstant MNTNAMLEN 255) ;; max number of bytes in a name argument

(defconstant *mountprog* 100005)
(defconstant *mountvers* 1)

(defparameter *mountd-tcp-socket* nil)
(defparameter *mountd-udp-socket* nil)


(defun make-mountdsockets ()
  (unless *mountd-tcp-socket*
    (setf *mountd-tcp-socket*
      (socket:make-socket :type :stream
                          :connect :passive
                          :reuse-address t)))
  (unless *mountd-udp-socket*
    (setf *mountd-udp-socket*
      (socket:make-socket :type :datagram))))

(defun close-mountdsockets ()
  (when *mountd-tcp-socket*
    (close *mountd-tcp-socket*)
    (setf *mountd-tcp-socket* nil))
  (when *mountd-udp-socket*
    (close *mountd-udp-socket*)
    (setf *mountd-udp-socket* nil)))


(defun mountd ()
  (make-mountdsockets)
  (portmap-add-program *mountprog* *mountvers* (socket:local-port *mountd-tcp-socket*) IPPROTO_TCP)
  (portmap-add-program *mountprog* *mountvers* (socket:local-port *mountd-udp-socket*) IPPROTO_UDP)
  (let ((server (make-rpc-server :tcpsock *mountd-tcp-socket* :udpsock *mountd-udp-socket*)))
    (loop
      (multiple-value-bind (xdr peer)
          (rpc-get-message server)
        (mountd-message-handler xdr peer)))))


(defun mountd-message-handler (xdr peer)
  (let (msg cbody)
    (setf msg (create-rpc-msg xdr))
    (setf cbody (rpc-msg-cbody msg))
    (pprint-cbody cbody)
    (unless (= (rpc-msg-mtype msg) 0)
      (error "Unexpected data!"))
    (when (and (= (call-body-prog cbody) *mountprog*)
               (= (call-body-vers cbody) *mountvers*))
      (cond
       ((= (call-body-proc cbody) 0)
        (mountd-null peer (rpc-msg-xid msg)))
       ((= (call-body-proc cbody) 1)
        (mountd-mount peer (rpc-msg-xid msg) cbody))
       ;;; right now, we don't do anything special for umounts
       ((= (call-body-proc cbody) 3)
        (mountd-null peer (rpc-msg-xid msg)))
       ((= (call-body-proc cbody) 4)
        (mountd-null peer (rpc-msg-xid msg)))
       ((= (call-body-proc cbody) 5)
        (mountd-export peer (rpc-msg-xid msg)))
       (t 
        (format t "mountd: unhandled procedure ~D~%" (call-body-proc cbody))))))) ;; should send a negative response

(defun mountd-null-verf ()
  (let ((xdr (create-xdr :direction :build)))
    (xdr-auth-null xdr)
    xdr))

(defun mountd-null (peer xid)
  (format t "mountd-null called.~%")
  (let ((xdr (create-xdr :direction :build)))
    (send-successful-reply peer xid (mountd-null-verf) xdr)))

(defun mountd-mount (peer xid cbody)
  (format t "mountd-mount~%")
  (let ((oa (call-body-cred cbody))
        (dirpath (with-xdr-xdr ((call-body-params cbody) :name x)
		   (xdr-string x)))
        au
        rootpathname
        )
    (unless (= (opaque-auth-flavor oa) 1)
      (return-from mountd-mount (rpc-send-auth-error-rejected-reply peer xid 2)))
    (setf au (xdr-opaque-auth-struct-to-auth-unix-struct oa))
    ;;(format t "Trying to mount w/ credetials: ~S~%" au)
    (format t "mountd-mount ~A by ~A~%" dirpath (auth-unix-machinename au))
    (setf rootpathname (locate-export dirpath))
    (if rootpathname
        (send-successful-reply peer xid 
                               (mountd-null-verf) 
                               (mountd-make-fhstatus 0 (pathname-to-fhandle rootpathname)))
      (mountd-reply-enoent peer xid))))
    

(defun mountd-reply-enoent (peer xid) ;; No such file or directory
  (send-successful-reply peer xid (mountd-null-verf) (mountd-make-fhstatus 2 nil)))
  
(defun mountd-make-fhstatus (status fhandle)
  (let ((xdr (create-xdr :direction :build :size (+ 4 32))))
    (xdr-int xdr status)
    (when (= 0 status)
      (xdr-opaque-fixed xdr :vec fhandle))
    xdr))

(defparameter *exports* 
    '(("/c" "c:/")
      ("/d" "d:/")))
  

(defun locate-export (dirpath)
  (let ((res (find dirpath *exports* :test (lambda (x pair) (string= (car pair) x)))))
    (when res
      (pathname (second res)))))

(defun mountd-export (peer xid)
  (format t "mountd-export~%")
  (let ((xdr (create-xdr :direction :build)))
    (dolist (export *exports*)
      (xdr-int xdr 1) ;; indicate that data follows
      (xdr-string xdr (first export))
      (xdr-int xdr 1) ;; group follows
      (xdr-string xdr "*") ;; group name
      (xdr-int xdr 0) ;; no more groups
      )
    (xdr-int xdr 0) ;; no more data
    (send-successful-reply peer xid (mountd-null-verf) xdr)))
 
