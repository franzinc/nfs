(in-package :user)

;; $Id: nfs-server-io.cl,v 1.4 2006/05/11 21:58:59 dancy Exp $

;; returns nil if no answer
(defun get-nfs-server-config-file ()
  (ignore-errors
   (sunrpc:with-rpc-client (cli "127.0.01" #.gen-nfs:*nfs-program* 2 :udp)
     (sunrpc:callrpc cli 100 nil nil :outproc #'xdr:xdr-string))))

(defun reload-nfs-server-config ()
  (ignore-errors
   (sunrpc:with-rpc-client (cli "127.0.01" #.gen-nfs:*nfs-program* 2 :udp)
     (= 1 (sunrpc:callrpc cli 101 nil nil :outproc #'xdr:xdr-unsigned-int)))))
