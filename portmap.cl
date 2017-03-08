;; -*- mode: common-lisp -*-
;;
;; Copyright (C) 2001 Franz Inc, Berkeley, CA.  All rights reserved.
;; Copyright (C) 2002-2014 Franz Inc, Oakland, CA.  All rights reserved.
;;
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by 
;; the Free Software Foundation, as clarified by the Franz
;; preamble to the LGPL found in
;; http://opensource.franz.com/preamble.html.
;;
;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;
;; Version 2.1 of the GNU Lesser General Public License can be
;; found at http://opensource.franz.com/license.html.
;; If it is not present, you can access it from
;; http://www.gnu.org/copyleft/lesser.txt (until superseded by a newer
;; version) or write to the Free Software Foundation, Inc., 59 Temple
;; Place, Suite 330, Boston, MA  02111-1307  USA

;; portmapper daemon and support functions

(in-package :portmap)

(eval-when (compile load eval)
  (export '(portmapper ping-portmapper)))

(sunrpc:def-rpc-program (PMAP 100000 :port *pmap-port*)
  (
   (2 ;; version
     (0 pmapproc-null void void)
     (1 pmapproc-set mapping bool)
     (2 pmapproc-unset mapping bool)
     (3 pmapproc-getport mapping unsigned-int)
     (4 pmapproc-dump void pmaplist)
     ;;(5 pmapproc-callit call-args call-result)
     (5 pmapproc-callit call-args :ignore)
     )
   (3 ;; version
     (0 rpcbproc-null rpcb void)
     (1 rpcbproc-set rpcb bool)
     (2 rpcbproc-unset rpcb bool)
     (3 rpcbproc-getaddr rpcb string)
     (4 rpcbproc-dump void rpcblist-ptr)
     ;;(5 rpcbproc-callit rpcb-rmtcallargs rpcb-rmtcallres)
     (5 rpcbproc-callit rpcb-rmtcallargs :ignore)
     (6 rpcbproc-gettime void unsigned-int)
     (7 rpcbproc-uaddr2taddr string netbuf)
     (8 rpcbproc-taddr2uaddr netbuf string)
     )
   (4 ;; version
     (0 rpcbproc-null rpcb void)
     (1 rpcbproc-set rpcb bool)
     (2 rpcbproc-unset rpcb bool)
     (3 rpcbproc-getaddr rpcb string)
     (4 rpcbproc-dump void rpcblist-ptr)
     ;;(5 rpcbproc-bcast rpcb-rmtcallargs rpcb-rmtcallres)
     (5 rpcbproc-bcast rpcb-rmtcallargs :ignore)
     (6 rpcbproc-gettime void unsigned-int)
     (7 rpcbproc-uaddr2taddr string netbuf)
     (8 rpcbproc-taddr2uaddr netbuf string)
     (9 rpcbproc-getversaddr rpcb string)
     ;;(10 rpcbproc-indirect rpcb-rmtcallargs rpcb-rmtcallres)
     (10 rpcbproc-indirect rpcb-rmtcallargs :ignore)
     (11 rpcbproc-getaddrlist rpcb rpcb-entry-list-ptr)
     (12 rpcbproc-getstat void rpcb-stat-byvers)
   )
  ))

(defparameter *mappings* nil)

(defun PMAP-init ()
  (setf *mappings* nil)
  ;; Register self.
  (dolist (vers '(2 3 4))
    (dolist (proto `(,*ipproto-udp* ,*ipproto-tcp*))
      (push (make-mapping :prog *pmap-prog* 
			  :vers vers 
			  :prot proto
			  :port *pmap-port*) 
	    *mappings*))))

(defun ping-portmapper ()
  (sunrpc:with-rpc-client (cli "127.0.0.1" #.*pmap-prog* #.*pmap-vers* :udp
			       :port #.*pmap-port*)
    ;; the NULL procedure returns nil on success, so we can't do something
    ;; simple like (if (ignore-errors (call-pmapproc-null-2 cli nil)) t)
    ;; We have to check that there was no error.
    (multiple-value-bind (res err)
	(ignore-errors (call-pmapproc-null-2 cli nil))
      (declare (ignore res))
      ;; If no error, portmapper responded.
      (null err))))
      

;; Called by user::startem
(defun portmapper (start-gate)
  (when (eq *use-system-portmapper* :auto)
    (setf *use-system-portmapper* nil)
    (when (ping-portmapper)
      (user::logit-stamp "PMAP: Using system portmapper. ** A conflicting NFS server may be running**~%")
      (setf *use-system-portmapper* t)
      ;; Indicate readiness to caller
      (mp:open-gate start-gate)))
  
  (when (not *use-system-portmapper*)
    ;; Start local portmapper
    (PMAP start-gate))) ;; Won't return
  

;;;;;;;;; server procedures

(defmethod print-object ((obj mapping) stream)
  (format stream "[Prg: ~d, V: ~d, ~a, Port: ~d]"
	  (portmap:mapping-prog obj)
	  (portmap:mapping-vers obj)
	  (sunrpc:protocol-to-string (portmap:mapping-prot obj))
	  (portmap:mapping-port obj)))

;; 'format' must be a constant string.
(defmacro debuglog (vers peer format &rest args)
  (if (or (not (constantp format))
	  (not (stringp format)))
      (error "'format' must be a constant string"))
  (let ((xvers (gensym))
	(format-string (concatenate 'string "~a~a: ~a: "  format)))
    `(let ((,xvers ,vers))
       (if *portmap-debug* 
	   (user::logit-stamp ,format-string
			      (if (>= ,xvers 3) "RPCB" "PMAP")
			      ,xvers
			      (sunrpc:peer-dotted ,peer)
			      ,@args)))))

(defun pmapproc-null (args vers peer cbody)
  (declare (ignore args cbody))
  (debuglog vers peer "NULL~%"))

(defun rpcbproc-null (args vers peer cbody)
  (pmapproc-null args vers peer cbody))

(defun mapping-matches (m1 m2)
  (and (= (mapping-prog m1) (mapping-prog m2))
       (= (mapping-vers m1) (mapping-vers m2))
       (= (mapping-prot m1) (mapping-prot m2))))

;;; Ref: http://www.opengroup.org/onlinepubs/009629799/PMAPPROC_SET.htm
;;; The procedure refuses to establish a mapping if one already exists
;;; for the tuple "(prog, vers, prot)"

(defun pmapproc-set (m vers peer cbody)
  (declare (ignore cbody))

  (let (res)
    
    (without-interrupts
      (when (and (sunrpc:local-peer-p peer)
		 (not (find m *mappings* :test #'mapping-matches)))
	(push m *mappings*)
	(setf res t)))

    (debuglog vers peer "SET ~a ==> ~a~%" m res)
    
    res))

(defun netid-to-proto (id)
  (if* (string= id "udp")
     then #.*ipproto-udp*
   elseif (string= id "tcp")
     then #.*ipproto-tcp*
     else -1))

(defun proto-to-netid (proto)
  (case proto
    (#.*ipproto-udp* "udp")
    (#.*ipproto-tcp* "tcp")
    (t "unknown")))

;; Invalid characters will result in a crazy result.
(defun atoi (string pos end)
  (declare (optimize speed (safety 0))
	   (fixnum pos end))
  (let ((res 0))
    (declare (fixnum res))
    (while (< pos end)
      (setf res (+ (the fixnum (* res 10))
		   (- (char-code (schar string pos)) #.(char-code #\0))))
      (incf pos))
    res))

(defun univ-addr-to-port (addr)
  (declare (optimize speed (safety 0))
	   (simple-string addr))
  ;; Would have been simpler with a regular expression, but I don't
  ;; use them anywhere else so let's avoid adding in the regexp module.
  (let ((dot2pos (position #\. addr :from-end t)))
    (declare (fixnum dot2pos))
    (if (null dot2pos)
	(return-from univ-addr-to-port -1))
    (let ((dot1pos (position #\. addr :from-end t :end dot2pos)))
      (declare (fixnum dot1pos))
      (if (null dot1pos)
	  (return-from univ-addr-to-port -1))
      (macrolet ((xatoi (&rest args)
		   `(the (mod 256) (atoi ,@args))))
	(values 
	 (+ (* (xatoi addr (1+ dot1pos) dot2pos) 256)
	    (xatoi addr (1+ dot2pos) (length addr)))
	 dot1pos)))))
  

(defun merge-uaddr-with-port (addr port)
  (declare (optimize speed (safety 0))
	   (fixnum port)
	   (simple-string addr))
  ;; Scan back for the second dot.
  (let ((tail (format nil ".~d.~d" (ash port -8) (logand port #xff)))
	(pos (1- (length addr)))
	(dot-count 0))
    (declare (fixnum pos dot-count))
    (while (>= pos 0)
      (when (char= (schar addr pos) #\.)
	(incf dot-count)
	(if (= dot-count 2)
	    (return)))
      (decf pos))
    (if (< pos 0)
	(setf pos 0))
    (concatenate 'string (subseq addr 0 pos) tail)))
  
(defun port-to-univ-addr (uaddr port)
  (if* (null port)
     then ""
     else (merge-uaddr-with-port uaddr port)))

(defun rpcb-to-mapping (r)
  (make-mapping 
   :prog (rpcb-r-prog r)
   :vers (rpcb-r-vers r)
   :prot (netid-to-proto (rpcb-r-netid r))
   :port (univ-addr-to-port (rpcb-r-addr r))))

(defun mapping-to-rpcb (uaddr m)
  (make-rpcb 
   :r-prog (mapping-prog m)
   :r-vers (mapping-vers m)
   :r-netid (proto-to-netid (mapping-prot m))
   :r-addr (port-to-univ-addr uaddr (mapping-port m))
   :r-owner "superuser"))

(defun rpcbproc-set (rpcb vers peer cbody)
  (pmapproc-set (rpcb-to-mapping rpcb) vers peer cbody))

(defun mapping-matches-noproto (m1 m2)
  (and (= (mapping-prog m1) (mapping-prog m2))
       (= (mapping-vers m1) (mapping-vers m2))))

;; portmap2: only program and version are considered.
;; rpcbind3,4: program, vers, and netid (proto) are considered unless
;;             netid is null (in which case it is not considered)
(defun pmapproc-unset (m vers peer cbody)
  (declare (ignore cbody))
  
  (let ((test-func (if* (or (< vers 3) (= (mapping-prot m) -1))
		      then #'mapping-matches-noproto
		      else #'mapping-matches))
	res)
    (without-interrupts
      (when (and (sunrpc:local-peer-p peer)
		 (find m *mappings* :test test-func))
	(setf *mappings* (delete m *mappings* :test test-func))
	(setf res t)))
    
    
    (debuglog vers peer "UNSET ~a ==> ~a~%" m res)
    
    res))

(defun rpcbproc-unset (rpcb vers peer cbody)
  (pmapproc-unset (rpcb-to-mapping rpcb) vers peer cbody))


(defun getport-common (m uaddr vers peer getversaddr)
  (let (port)
    (without-interrupts
      (let ((entry (find m *mappings* :test #'mapping-matches)))
	(if entry
	    (setf port (mapping-port entry)))))
    
    (debuglog vers peer "~a ~a ==> ~a~%"
	      (if* (>= vers 3)
		 then (if* getversaddr 
			 then "GETVERSADDR"
			 else "GETADDR")
		 else "GETPORT")
	      m port)
    
    (if* (>= vers 3)
       then (port-to-univ-addr uaddr port)
       else (or port 0))))

(defun pmapproc-getport (m vers peer cbody)
  (declare (ignore cbody))
  (getport-common m nil vers peer nil))

(defun rpcbproc-getaddr (rpcb vers peer cbody)
  ;; RFC1833 says that the supplied protocol (r_netid) should be ignored
  ;; and the transport protocol that the request came from should be
  ;; used instead.
  (setf (rpcb-r-netid rpcb) 
    (ecase (sunrpc::rpc-peer-type peer)
      (:datagram "udp")
      (:stream "tcp")))
  (getport-common (rpcb-to-mapping rpcb) (rpcb-r-addr rpcb) vers peer 
		  (= (sunrpc:call-body-proc cbody) RPCBPROC-GETVERSADDR)))
		  

(defun pmapproc-dump (arg vers peer cbody)
  (declare (ignore arg cbody))

  (debuglog vers peer "DUMP~%")
  
  (let (head)
    (without-interrupts
      (dolist (m *mappings*)
	(setf head 
	  (if* (>= vers 3)
	     then (make-rp--list 
		   :rpcb-map (mapping-to-rpcb "0.0.0.0.0.0" m) 
		   :rpcb-next head)
	     else (make-pmapentry :map m :next head)))))

    head))

(defun rpcbproc-dump (args vers peer cbody)
  (pmapproc-dump args vers peer cbody))

;; Silently ignore 
(defun pmapproc-callit (args vers peer cbody)
  (declare (ignore args vers peer cbody)))

;; Silently ignore 
(defun rpcbproc-callit (args vers peer cbody)
    (declare (ignore args vers peer cbody)))

(defun rpcbproc-gettime (args vers peer cbody)
  (declare (ignore args cbody))
  (let ((res (excl.osi:universal-to-unix-time (get-universal-time))))
    (debuglog vers peer "GETTIME ==> ~d~%" res)
    res))

;; The details of that a 'taddr' is are not documented anywhere but
;; experimentation indicates that its a sockaddr.  We'll use
;; a sockaddr_in.
(defun rpcbproc-uaddr2taddr (uaddr vers peer cbody)
  (declare (ignore cbody))
  (debuglog vers peer "UADDR2TADDR ~a~%" uaddr)
  
  (multiple-value-bind (port pos)
      (univ-addr-to-port uaddr)
    (let ((vec (make-array 16 :element-type '(unsigned-byte 8)
			   :initial-element 0))
	  (addr (socket:dotted-to-ipaddr (subseq uaddr 0 pos))))
      (setf (aref vec 0) 0)
      (setf (aref vec 1) 2) ;; AF_INET
      (setf (aref vec 2) (ash port -8))
      (setf (aref vec 3) (logand port #xff))
      (setf (aref vec 4) (ash addr -24))
      (setf (aref vec 5) (logand (ash addr -16) #xff))
      (setf (aref vec 6) (logand (ash addr -8) #xff))
      (setf (aref vec 7) (logand addr #xff))
	
      (make-netbuf :maxlen 16 
		   :buf vec))))

(defun rpcbproc-taddr2uaddr (netbuf vers peer cbody)
  (declare (ignore cbody))
  (debuglog vers peer "TADDR2UADDR~%")

  (let* ((op (netbuf-buf netbuf))
	 (len (opaque-len op))
	 (vec (opaque-vec op))
	 (pos (opaque-offset op)))
    (block nil
      (if (or (< len 8)
	      (/= (aref vec pos) 0)
	      (/= (aref vec (1+ pos)) 2))
	  (return ""))
      (incf pos 2)
      (format nil "~d.~d.~d.~d.~d.~d" 
	      (aref vec (+ pos 2))
	      (aref vec (+ pos 3))
	      (aref vec (+ pos 4))
	      (aref vec (+ pos 5))
	      (aref vec pos)
	      (aref vec (1+ pos))))))

;; additional v4 procs

;; Silently ignore 
(defun rpcbproc-bcast (args vers peer cbody)
  (declare (ignore args vers peer cbody)))

;; The RFC is unclear on how this differs from getaddr.  It has a
;; statement about how it's different, but the statement doesn't
;; describe behavior that is different from getaddr.
(defun rpcbproc-getversaddr (rpcb vers peer cbody)
  (rpcbproc-getaddr rpcb vers peer cbody))

;; Silently ignore 
(defun rpcbproc-indirect (args vers peer cbody)
  (declare (ignore args vers peer cbody)))

(defun rpcbproc-getaddrlist (rpcb vers peer cbody)
  (declare (ignore cbody))
  
  (let ((m (rpcb-to-mapping rpcb))
	(uaddr (rpcb-r-addr rpcb))
	(count 0)
	head)

    (without-interrupts 
      (dolist (entry *mappings*)
	(when (mapping-matches-noproto m entry)
	  (incf count)
	  (setf head
	    (make-rpcb-entry-list 
	     :rpcb-entry-map
	     (make-rpcb-entry 
	      :r-maddr (port-to-univ-addr uaddr (mapping-port entry))
	      :r-nc-netid (proto-to-netid (mapping-prot entry))
	      :r-nc-semantics (if* (eq (mapping-prot entry) *ipproto-tcp*)
				 then 3 ;; NC_TPI_COTS_ORD
				 else 1) ;; NC_TPI_CLTS
	      :r-nc-protofmly "inet"
	      :r-nc-proto (proto-to-netid (mapping-prot entry)))
	     :rpcb-entry-next head)))))

    (debuglog vers peer "GETADDRLIST ~a ==> ~d entries.~%" m count)
    
    head))

;; We don't keep statistics.  Just report a bunch of zeros
(defun rpcbproc-getstat (arg vers peer cbody)
  (declare (ignore arg cbody))
  (debuglog vers peer "GETSTAT~%")
  (let (res)
    (dotimes (n 3)
      (push
       (make-rpcb-stat 
	:info '(0 0 0 0 0 0 0 0 0 0 0 0 0) ;; 13 entries = RPCBSTAT_HIGHPROC
	:setinfo 0
	:unsetinfo 0
	:addrinfo nil
	:rmtinfo nil)
       res))
    res))
