(in-package :user)

(defconstant *nfsprog* 100003)
(defconstant *nfsvers* 2)

;; returns nil if no answer
(defun get-nfs-server-config-file ()
  (when (ping-portmapper)
    (ignore-errors
     (callrpc "127.0.0.1" *nfsprog* *nfsvers* 10001234 :udp nil nil
              :outproc #'xdr-string))))

(defun reload-nfs-server-config ()
  (when (ping-portmapper)
    (ignore-errors
     (= 1 (callrpc "127.0.0.1" *nfsprog* *nfsvers* 10001235 :udp nil nil
                 :outproc #'xdr-unsigned-int)))))
  
           