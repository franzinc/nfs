(in-package :user)

;; $Id: nfs-server-io.cl,v 1.3 2006/01/30 16:33:50 dancy Exp $

(defconstant *nfsvers* 2)

;; returns nil if no answer
(defun get-nfs-server-config-file ()
  (when (ping-portmapper)
    (ignore-errors
     (callrpc "127.0.0.1" #.*nfsprog* 2 100 :udp nil nil
              :outproc #'xdr-string))))

(defun reload-nfs-server-config ()
  (when (ping-portmapper)
    (ignore-errors
     (= 1 (callrpc "127.0.0.1" #.*nfsprog* 2 101 :udp nil nil
                 :outproc #'xdr-unsigned-int)))))
  
           