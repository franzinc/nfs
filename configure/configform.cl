;;; Code for the form named :configform of class configform.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)

(defparameter *nfs-debug* nil)
(defparameter *nfs-debug-filter* #x0fffffff)
(defparameter *nfs-set-mtime-on-write* nil)

(eval-when (compile)
  (defparameter *nfs-debug-types* 
    '(read write lookup access setattr getattr link symlink
           mkdir rmdir remove rename create mknod commit null
           statfs fsstat fsinfo pathconf readdir readlink)))
  

(defparameter *nfs-gc-debug* nil)

(in-package :portmap)
(defparameter *portmap-debug* nil)
(defparameter *use-system-portmapper* :auto)
(eval-when (compile load eval)
  (export '(*portmap-debug* *use-system-portmapper*)))

(in-package :mount)
(defparameter *mountd-debug* nil)
(defparameter *mountd-port-number* nil)
(defparameter *showmount-disabled* nil)
(eval-when (compile load eval)
  (export '(*mountd-debug* *mountd-port-number* *showmount-disabled*)))

(in-package :nsm)
(defparameter *nsm-debug* nil)
(defparameter *nsm-port* nil)
(eval-when (compile load eval)
  (export '(*nsm-debug* *nsm-port*)))

(in-package :nlm)
(defparameter *nlm-debug* nil)
(defparameter *nlm-port* nil)
(eval-when (compile load eval)
  (export '(*nlm-debug* *nlm-port*)))

(in-package :common-graphics-user)

(defparameter *host-lists* nil)
(defparameter *user-lists* nil)

(defparameter *configfile* nil)
(defparameter *server-running* nil)
(defparameter *progpath* nil)

(defparameter *subprogram-ports*
  '((:mountd-port-number mount:*mountd-port-number*)
    (:nsm-port-number nsm:*nsm-port*)
    (:nlm-port-number nlm:*nlm-port*)))


(defun my-find-component (comp form)
  (let ((res (find-component comp form)))
    (if res
        res
      (error "could not find component ~S on form ~S" comp form))))


(defun process-config (configfile form)
  (let ((config (read-from-string (file-contents configfile)))
        cmd name)
    (setf *exports* nil)
    (setf *host-lists* (make-hash-table :test #'equalp))
    (setf *user-lists* (make-hash-table :test #'equalp))
    (dolist (entry config)
      (when (not (listp entry))
        (error "Invalid configuration entry: ~S~%" entry))
      (setf cmd (pop entry))
      (case cmd
        (define-host-list
            (setf name (pop entry))
            (setf (gethash name *host-lists*) entry))
        (define-user-list
            (setf name (pop entry))
            (setf (gethash name *user-lists*) entry))
        (define-export
            (apply #'define-export entry))
        (t
         (set cmd (pop entry))))))
  (sort-exports)
  (populate-form form))

(defun sort-exports ()
  (setf *exports* (sort *exports* #'string< :key #'nfs-export-name)))

(defun generate-config-expression ()
  (let (config)
    ;; The list is built up in reverse so 
    ;; do export definitions first... then user and host
    ;; lists..   
    ;; the order of parameters (*use-system-portmapper*, etc) doesn't matter
    
    (dolist (export *exports*)
      (push
       (list 'define-export 
             :name (nfs-export-name export)
             :path (nfs-export-path export)
             :uid (nfs-export-uid export)
             :gid (nfs-export-gid export)
             :umask (nfs-export-umask export)
             :set-mode-bits (nfs-export-set-mode-bits export)
             :hosts-allow (nfs-export-hosts-allow export)
             :rw-users (nfs-export-rw-users export)
             :ro-users (nfs-export-ro-users export))
       config))
    
    (maphash #'(lambda (key value)
                 (push
                  (append (list 'define-host-list key) value)
                  config))
             *host-lists*)
    (maphash #'(lambda (key value)
                 (push
                  (append (list 'define-user-list key) value)
                  config))
             *user-lists*)
    
    ;; parameters
    (push `(portmap:*portmap-debug* ,portmap:*portmap-debug*) config)
    (push `(portmap:*use-system-portmapper* ,portmap:*use-system-portmapper*) config)
    (push `(mount:*mountd-debug* ,mount:*mountd-debug*) config)
    (push `(mount:*mountd-port-number* ,mount:*mountd-port-number*) config)
    (push `(nsm:*nsm-debug* ,nsm:*nsm-debug*) config)
    (push `(nsm:*nsm-port* ,nsm:*nsm-port*) config)
    (push `(nlm:*nlm-debug* ,nlm:*nlm-debug*) config)
    (push `(nlm:*nlm-port* ,nlm:*nlm-port*) config)
    (push `(*nfs-debug* ,*nfs-debug*) config)
    (push `(*nfs-debug-filter* ,*nfs-debug-filter*) config)
    (push `(*nfs-gc-debug* ,*nfs-gc-debug*) config)
    (push `(*nfs-set-mtime-on-write* ,*nfs-set-mtime-on-write*) config)
    
    config))

(defun user-list-names ()
  (let (res)
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (push key res))
             *user-lists*)
    (sort res #'string<)))
     
(defun host-list-names ()
  (let (res)
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (push key res))
             *host-lists*)
    (sort res #'string<)))

(defun export-names ()
  (mapcar #'(lambda (exp) (nfs-export-name exp)) *exports*))


(defun populate-form (form)
  ;; Debugging options
  (setf (value (my-find-component :portmap-debug-checkbox form))
    portmap:*portmap-debug*)
  (setf (value (my-find-component :nfs-debug-checkbox form))
    *nfs-debug*)
  (setf (value (my-find-component :gc-debug-checkbox form))
    *nfs-gc-debug*)
  (setf (value (my-find-component :mountd-debug-checkbox form))
    mount:*mountd-debug*)
  (setf (value (my-find-component :nsm-debug-checkbox form))
    nsm:*nsm-debug*)
  (setf (value (my-find-component :nlm-debug-checkbox form))
    nlm:*nlm-debug*)
  (setf (value (my-find-component :set-mtime-on-write-checkbox form))
    *nfs-set-mtime-on-write*)
  
  (setf (value (my-find-component :combo-box-port-mapper form))
    (if* (null portmap:*use-system-portmapper*)
       then :no
     elseif (eq portmap:*use-system-portmapper* :auto)
       then :auto
       else :yes))
  
  (macrolet ((nfs-debug-filters (types)
                                (let (res)
                                  (dolist (type types)
                                    (let ((component-name (intern (format nil "debug-nfs-~a" type) :keyword))
                                          (filter-constant (intern (format nil "*nfs-debug-~a*" type) :user)))
                                      (push `(let ((wij (my-find-component ,component-name form)))
                                               (setf (value wij)
                                                 (if (/= 0 (logand *nfs-debug-filter* ,filter-constant)) 
                                                     t))
                                               (setf (available wij) *nfs-debug*))
                                            res)))
                                      
                                  (setf res (nreverse res))
                                  `(progn ,@res))))
    
    (nfs-debug-filters #.*nfs-debug-types*))
  
  ;; Subprogram port numbers.
  (dolist (entry *subprogram-ports*) 
    (let ((wij (my-find-component (first entry) form))
          (value (symbol-value (second entry))))
      (setf (value wij) 
        (if* value
           then (format nil "~d" value)
           else ""))))
  
  ;; user lists
  (let ((user-list-names (user-list-names))
        (combo (my-find-component :user-list-combo form)))
    (setf (range combo) user-list-names)    
    (setf (value combo) (first user-list-names))
    (setf (value (my-find-component :new-user-id-edit form)) ""))
  
  ;; host lists
  (let ((host-list-names (host-list-names))
        (combo (my-find-component :host-list-combo form)))
    (setf (range combo) host-list-names)
    (setf (value combo) (first host-list-names))
    (setf (value (my-find-component :new-address form)) ""))
  
  ;; exports
  (update-export-combo form (first (export-names)))
 
  (update-user-lists form)
  (update-host-allowed-lists form))

(defun update-export-combo (form selected)
  (let ((combo (my-find-component :export-selection-combo form))
        (names (export-names)))
    (setf (range combo) names)
    (setf (value combo) selected)
    (setf (available (my-find-component :remove-export-button form)) names)))
  
  

(defun special-user-list-p (name)
  (or (string= name "everyone")
      (string= name "root")))

(defun special-host-list-p (name)
  (string= name "all"))

;; when a user list is selected:
;;  entries in :user-list-multi are filled in and all are deselected.. therefore the
;;  remove button is disabled.
(defun select-user-list (widget new-value old-value)
  (declare (ignore old-value))
  (let* ((form (parent widget))
         (multi (my-find-component :user-list-multi form)))
    (setf (range multi) (gethash new-value *user-lists*))
    (setf (value multi) nil) ;; deselects all
    (update-user-remove-button form)
    (setf (available (my-find-component :user-list-remove-button form)) (not (special-user-list-p new-value)))
    (update-user-add-button form))
  t)

(defun select-host-list (widget new-value old-value)
  (declare (ignore old-value))
  (let* ((form (parent widget))
         (multi (my-find-component :host-list-multi form)))
    (setf (range multi) (gethash new-value *host-lists*))
    (setf (value multi) nil) ;; deselects all
    (update-host-remove-button form)
    (setf (available (my-find-component :host-list-remove-button form)) (not (special-host-list-p new-value)))
    (update-host-add-button form))
  t)


  
(defun extract-number (string &key (radix 10))
  (multiple-value-bind (matched whole digits)
      (match-regexp "^\\b*\\([0-9]+\\)\\b*$" string)
    (declare (ignore whole))
    (when matched
      (ignore-errors (parse-integer digits :radix radix)))))

(defun valid-network-address-p (string)
  (ignore-errors (parse-addr string)))


  
(defun configform-new-user-id-edit-on-change (widget
                                              new-value
                                              old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (update-user-add-button (parent widget))
  t) ; Accept the new value

;; Button is available if we're not on a special user list
;; and the input is good.
(defun update-user-add-button (form)
  (setf (available (my-find-component :user-add-button form)) 
    (and (not (special-user-list-p (value (my-find-component :user-list-combo form))))
         (extract-number (value (my-find-component :new-user-id-edit form))))))  


(defun update-host-add-button (form)
  (setf (available (my-find-component :host-add-button form)) 
    (and (not (special-host-list-p (value (my-find-component :host-list-combo form))))
         (valid-network-address-p (value (my-find-component :new-address form))))))  
  
      
(defun configform-new-address-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (update-host-add-button (parent widget))
  t) ; Accept the new value

;; Button is available if we're not on a special user list..
;; and there is at least one entry selected in :user-list-multi
(defun update-user-remove-button (form)
  (setf (available (my-find-component :user-remove-button form))
    (and (not (special-user-list-p (value (my-find-component :user-list-combo form))))
         (value (my-find-component :user-list-multi form)))))

(defun update-host-remove-button (form)
  (setf (available (my-find-component :host-remove-button form))
    (and (not (special-host-list-p (value (my-find-component :host-list-combo form))))
         (value (my-find-component :host-list-multi form)))))



(defun configform-user-list-multi-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (update-user-remove-button (parent widget))
  t) ; Accept the new value

(defun configform-host-list-multi-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (update-host-remove-button (parent widget))
  t) ; Accept the new value

;; Won't add duplicates, but will highlight the existing entry.
;; For new entries, they'll be added in a sorted manner and the added
;; entry will be highlighted.
;; new-user-id-edit will be cleared.
;; assumes data verification has already been done (since the add button wouldn't
;; have been enabled otherwise).
(defun configform-user-add-button-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (listname (value (my-find-component :user-list-combo form)))
         (list (gethash listname *user-lists*))
         (new-user-id-edit (my-find-component :new-user-id-edit form))
         (newid (extract-number (value new-user-id-edit)))
         (multi (my-find-component :user-list-multi form)))
    (pushnew newid list)
    ;; Using copy-list so that the new list will definitely be not eq
    ;; to the old list.  Without this, sometimes the sorted list might
    ;; be eq.. and sometimes not.  For the times when it is eq, setfing
    ;; the range of the multi list would cause no screen update because
    ;; it didn't think the list really changed.
    (setf list (sort (copy-list list) #'<))
    (setf (gethash listname *user-lists*) list)
    ;; Update the multi
    (setf (range multi) list)
    ;; highlight the new entry
    (setf (value multi) (list newid))
    ;; clear the edit box 
    (setf (value new-user-id-edit) "")
    (refresh-apply-button form))
  t) ; Accept the new value

(defun configform-host-add-button-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (listname (value (my-find-component :host-list-combo form)))
         (list (gethash listname *host-lists*))
         (new-address-edit (my-find-component :new-address form))
         (new-address (string-trim '(#\space) (value new-address-edit)))
         (multi (my-find-component :host-list-multi form)))
    (pushnew new-address list :test #'string=)
    (setf (gethash listname *host-lists*) list)
    ;; Update the multi
    (setf (range multi) list)
    ;; highlight the new entry
    (setf (value multi) (list new-address))
    ;; clear the edit box 
    (setf (value new-address-edit) "")
    (refresh-apply-button form))
  t) ; Accept the new value

;; Remove the items selected in user-list-multi.
;; clears selections.
(defun configform-user-remove-button-on-change (widget
                                                new-value
                                                old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (multi (my-find-component :user-list-multi form))
         (listname (value (my-find-component :user-list-combo form)))
         (list (gethash listname *user-lists*))
         (selections (value multi)))
    (setf list (remove-if #'(lambda (entry) (member entry selections)) list))
    (setf (gethash listname *user-lists*) list)
    (setf (value multi) nil)
    (setf (range multi) list)
    (refresh-apply-button form))
  t) ; Accept the new value

(defun configform-host-remove-button-on-change (widget
                                                new-value
                                                old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (multi (my-find-component :host-list-multi form))
         (listname (value (my-find-component :host-list-combo form)))
         (list (gethash listname *host-lists*))
         (selections (value multi)))
    (setf list (remove-if #'(lambda (entry) (member entry selections :test #'string=)) list))
    (setf (gethash listname *host-lists*) list)
    (setf (value multi) nil)
    (setf (range multi) list)
    (refresh-apply-button form))
  
  t) ; Accept the new value

;; Checks for duplicates.
;; updates the combo and selects the new entry.
(defun new-list-common (form type)
  (multiple-value-bind (combo lists-hash list-func update-func)
      (ecase type
        (:user
         (values (my-find-component :user-list-combo form)
                 *user-lists*
                 #'user-list-names
                 #'update-user-lists))
        (:host
         (values (my-find-component :host-list-combo form)
                 *host-lists*
                 #'host-list-names
                 #'update-host-allowed-lists)))
    (tagbody
      askforname
      (multiple-value-bind (listname dummy1 dummy2 ok)
          (ask-user-for-string 
           (format nil "Provide a name for this new ~A list" type)
           "" "OK" "Cancel" nil nil 
           (format nil "New ~A list" type))
        (declare (ignore dummy1 dummy2))
        (when ok
          (setf listname (string-trim '(#\space) listname))
          ;; Check for duplicates
          (multiple-value-bind (val found) (gethash listname lists-hash)
            (declare (ignore val))
            (when found
              (pop-up-message-dialog form "New list" "That name is already in use" error-icon "OK")
              (go askforname)))
          ;; All is well.
          ;; Make fresh list.
          (setf (gethash listname lists-hash) nil)
          (setf (range combo) (funcall list-func))
          (setf (value combo) listname)
          ;; update the list choices in the exports tab
          (funcall update-func form)
          (refresh-apply-button form))))))
    
(defun configform-new-user-list-button-on-change (widget
                                                  new-value
                                                  old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (new-list-common (parent widget) :user)
  t) ; Accept the new value

(defun configform-new-host-list-button-on-change (widget
                                                  new-value
                                                  old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (new-list-common (parent widget) :host)
  t) ; Accept the new value

(defun get-export (name)
  (find name *exports* :test #'string= :key #'nfs-export-name))

;; update the selections on the multis.
;; update the edit boxes.
(defun configform-export-selection-combo-on-change (widget
                                                    new-value
                                                    old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (block nil 
    (let ((form (parent widget))
          (exp (get-export new-value)))
      ;; disable a bunch of stuff if all the exports have been deleted
      (when (null new-value)
        (clear-and-disable-export-controls form)
        (return))
      
      ;; export selected
      (let ((wij (my-find-component :export-host-allowed-lists form)))
        (without-on-change (wij)
          (setf (value wij) (nfs-export-hosts-allow exp))
          (setf (available wij) t)))
      
      (let ((wij (my-find-component :export-rw-users-lists form)))
        (without-on-change (wij)
          (setf (value wij) (nfs-export-rw-users exp))
          (setf (available wij) t)))
        
      (let ((wij (my-find-component :export-ro-users-lists form)))
        (without-on-change (wij)
          (setf (value wij) (nfs-export-ro-users exp))
          (setf (available wij) t)))
      
      (let ((wij (my-find-component :export-path form)))
        (without-on-change (wij)
          (setf (value wij) (nfs-export-path exp))
          (setf (available wij) t)))
      
      (update-export-uid form exp)
      (update-export-gid form exp)
      (update-export-umask form exp)
      (update-export-set-mode-bits form exp)
      
      (setf (available (my-find-component :directory-browse-button form)) t)))
  t) ; Accept the new value

(defun clear-and-disable-export-controls (form)
  (let ((text-widgets '(:export-path :export-uid :export-gid :export-umask
			:export-set-bits))
        (buttons '(:remove-export-button :directory-browse-button))
        (lists '(:export-host-allowed-lists :export-rw-users-lists
		 :export-ro-users-lists)))
    (let (wij)
      (dolist (wij-name text-widgets)
        (setf wij (my-find-component wij-name form))
        (setf (available wij) nil)
        (without-on-change (wij)
          (setf (value wij) ""))) ;; because blank is not accepted.
      (dolist (wij-name buttons)
        (setf (available (my-find-component wij-name form)) nil))
      (dolist (wij-name lists)
        (setf wij (my-find-component wij-name form))
        (setf (available wij) nil)
        (setf (value wij) nil)))))
        

  
(defun update-export-uid (form exp)
  (let ((wij (my-find-component :export-uid form)))
    (without-on-change (wij)
      (setf (value wij) (format nil "~d" (nfs-export-uid exp)))
      (setf (available wij) t))))

(defun update-export-gid (form exp)
  (let ((wij (my-find-component :export-gid form)))
    (without-on-change (wij)
      (setf (value wij) (format nil "~d" (nfs-export-gid exp)))
      (setf (available wij) t))))

(defun update-export-umask (form exp)
  (let ((wij (my-find-component :export-umask form)))
    (without-on-change (wij)
      (setf (value wij) (format nil "~3,'0o" (nfs-export-umask exp)))
      (setf (available wij) t))))
  
(defun update-export-set-mode-bits (form exp)
  (let ((wij (my-find-component :export-set-bits form)))
    (without-on-change (wij)
      (setf (value wij) (format nil "~3,'0o" (nfs-export-set-mode-bits exp)))
      (setf (available wij) t))))
  
(defun update-host-allowed-lists (form)
  (setf (range (my-find-component :export-host-allowed-lists form))
    (host-list-names)))

(defun update-user-lists (form)
  (dolist (multi '(:export-rw-users-lists :export-ro-users-lists))
    (setf (range (my-find-component multi form)) (user-list-names))))
  
  
(defun configform-directory-browse-button-on-change (widget
                                                     new-value
                                                     old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (path (ask-user-for-directory :prompt "Select a directory to export"))
         (expname (value (my-find-component :export-selection-combo form)))
         (exp (get-export expname)))
    (when path
      (setf path (user::cleanup-dir (namestring path)))
      (setf (nfs-export-path exp) path)
      (setf (value (my-find-component :export-path form)) path)
      (refresh-apply-button form)))
  t) ; Accept the new value

(defun numeric-on-change-common (form value type)
  (multiple-value-bind (radix name)
      (ecase type
        (:uid
         (values 10 "Uid"))
        (:gid
         (values 10 "Gid"))
        (:umask
         (values 8 "Umask"))
        (:set-mode-bits
         (values 8 "Set mode bits")))
  (let ((value (extract-number value :radix radix))
        (exp (get-export (value
			  (my-find-component :export-selection-combo
					     form)))))
    (if* value
       then (ecase type
              (:uid
               (setf (nfs-export-uid exp) value)
               (update-export-uid form exp))
              (:gid
               (setf (nfs-export-gid exp) value)
               (update-export-gid form exp))
              (:umask
               (setf (nfs-export-umask exp) value)
               (update-export-umask form exp))
              (:set-mode-bits
               (setf (nfs-export-set-mode-bits exp) value)
               (update-export-set-mode-bits form exp)))
            (refresh-apply-button form)
            t
       else (pop-up-message-dialog
	     form "Invalid entry" 
	     (concatenate 'string name " field must be a non-negative "
			  (if (= radix 8) "octal " "")
			  "number")
	     error-icon "OK")
            nil))))
            

(defun configform-export-uid-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (numeric-on-change-common (parent widget) new-value :uid))

(defun configform-export-gid-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (numeric-on-change-common (parent widget) new-value :gid))

(defun configform-export-umask-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (numeric-on-change-common (parent widget) new-value :umask))

(defun configform-export-set-bits-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (numeric-on-change-common (parent widget) new-value :set-mode-bits))

(defun configform-export-host-allowed-lists-on-change (widget
                                                       new-value
                                                       old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (exp (get-export (value (my-find-component :export-selection-combo
						    form)))))
    (when exp
      (setf (nfs-export-hosts-allow exp) new-value)
      (refresh-apply-button form)))
  t) ; Accept the new value

(defun configform-export-rw-users-lists-on-change (widget
                                                   new-value
                                                   old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (exp (get-export (value (my-find-component :export-selection-combo
						    form)))))
    (when exp
      (setf (nfs-export-rw-users exp) new-value)
      (refresh-apply-button form)))
  t) ; Accept the new value

(defun configform-export-ro-users-lists-on-change (widget
                                                   new-value
                                                   old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (exp (get-export (value (my-find-component :export-selection-combo
						    form)))))
    (when exp
      (setf (nfs-export-ro-users exp) new-value)
      (refresh-apply-button form)))
  t) ; Accept the new value

(defun configform-export-path-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let ((form (parent widget))
        (path (ignore-errors (user::cleanup-dir new-value))))
    (if* path
       then (setf (nfs-export-path
		   (get-export
		    (value (my-find-component :export-selection-combo form))))
              path)
            (refresh-apply-button form)
            t
       else (pop-up-message-dialog
	     form "Invalid input" 
	     (format nil "~A is not a valid directory specification" new-value)
	     error-icon "OK")
            nil)))
          

(defun configform-cancel-button-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (user-close (parent widget))
  t) ; Accept the new value

;; return t if sure, nil if not.
(defun ask-user-if-sure (form warning)
  (= 1 (pop-up-message-dialog form "Saving configuration..."
			      warning warning-icon "Yes" "No")))


;; if there is a potential problem, prompt the user.
;; returns:  t if good, nil if no-save.
(defun sanity-check-export (form exp)
  (block nil
    (if (and (null (nfs-export-hosts-allow exp))
             (not (ask-user-if-sure 
                   form 
                   (format nil 
                       "The export ~A has no allowed hosts selected.  This means the export will not be accessible at all.  Are you sure you want to save?"
                     (nfs-export-name exp)))))
        (return nil))
    ;; This test isn't quite right.. because a client user that has an id number
    ;; that matches the export's uid will have read/write permission.  
    #+ignore(if (and (null (nfs-export-rw-users exp))
                     (null (nfs-export-ro-users exp))
                     (not (ask-user-if-sure
                           form
                           (format nil 
                               "The export ~A has no read-only or read/write users assigned.  This means that the export will be unusable by any remote user.  Are you sure you want to save?"
                             (nfs-export-name exp)))))
                (return nil))
    ;; Make sure root has read access (otherwise mounts will likely fail).
    (if (and (not (root-has-read-access-p exp))
             (not (ask-user-if-sure 
                   form
                   (format nil 
                       "The export ~A does not grant read access to user id 0 (root).  This may make it impossible for remote NFS clients to mount the export.  Are you sure you want to save?"
                     (nfs-export-name exp)))))
        (return nil))
    t))
        
        
  
  

;; returns the cleaned-up name.
;; or returns nil after complaining.
(defun check-export-name (form name)
  (block nil
    (setf name (string-trim '(#\space) name))
    ;; Check for duplicates
    (when (member name (export-names) :test #'string=)
      (pop-up-message-dialog form "New export" "That name is already in use" error-icon "OK")
      (return nil))
    name))
    

(defun configform-new-export-button-on-change (widget
                                               new-value
                                               old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (old-export-name (value (my-find-component :export-selection-combo form))))
    (tagbody
      askforname
      (multiple-value-bind (exportname dummy1 dummy2 ok)
          (ask-user-for-string 
           "Provide a name for this new export.  
This is path that remote clients will use to connect." "/export" "OK" "Cancel" nil nil "New export")
        (declare (ignore dummy1 dummy2))
        (when ok
          (setf exportname (check-export-name form exportname))
          (if (null exportname)
              (go askforname))
          (let ((path (ask-user-for-directory :prompt "Select a directory to export")))
            (when path
              (setf path (namestring path))
              (if* (and old-export-name
                        (= 1
                           (pop-up-message-dialog 
                            form "Copy settings?" 
                            (format nil "Would you like to copy the remaining settings from the ~A export?" old-export-name)
                            question-icon "Yes" "No")))
                 then (let ((oldexp (get-export old-export-name)))
                        (define-export 
                            :name exportname
                          :path path
                          :uid (nfs-export-uid oldexp)
                          :gid (nfs-export-gid oldexp)
                          :umask (nfs-export-umask oldexp)
                          :set-mode-bits (nfs-export-set-mode-bits oldexp)
                          :hosts-allow (nfs-export-hosts-allow oldexp)
                          :rw-users (nfs-export-rw-users oldexp)
                          :ro-users (nfs-export-ro-users oldexp)))
                 else (define-export :name exportname :path path))
              (sort-exports)
              ;; Update the combo.
              (update-export-combo form exportname)
              (refresh-apply-button form)))))))
  t) ; Accept the new value

(defun configform-remove-export-button-on-change (widget
                                                  new-value
                                                  old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let* ((form (parent widget))
         (combo (find-component :export-selection-combo form))
         (name (value combo)))
    (setf *exports* (remove name *exports*
                            :test #'string=
                            :key #'nfs-export-name))
    (update-export-combo form (first (export-names)))
    (refresh-apply-button form))
  t) ; Accept the new value

;; Don't allow removal if the list is in use by an export
(defun configform-host-list-remove-button-on-change (widget
                                                     new-value
                                                     old-value)   
  (declare (ignore-if-unused widget new-value old-value))
  (block outer
    (let* ((form (parent widget))
           (combo (my-find-component :host-list-combo form))
           (listname (value combo)))
      (dolist (exp *exports*)
        (when (member listname (nfs-export-hosts-allow exp) :test #'string=)
          (pop-up-message-dialog 
           form "Remove host list" 
           (format nil "This host list is in use by export ~A." (nfs-export-name exp))
           error-icon "OK")
          (return-from outer)))
      
      ;; It's all good
      (remhash listname *host-lists*)
      (setf (range combo) (remove listname (range combo)))
      (setf (value combo) (first (range combo)))
      (update-host-allowed-lists form)
      (refresh-apply-button form)))
  t) ; Accept the new value

(defun configform-user-list-remove-button-on-change (widget
                                                     new-value
                                                     old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (block outer
    (let* ((form (parent widget))
           (combo (my-find-component :user-list-combo form))
           (listname (value combo)))
      (dolist (exp *exports*)
        (when (or (member listname (nfs-export-rw-users exp) :test #'string=)
                  (member listname (nfs-export-ro-users exp) :test #'string=))
          (pop-up-message-dialog 
           form "Remove user list" 
           (format nil "This user list is in use by export ~A." (nfs-export-name exp))
           error-icon "OK")
          (return-from outer)))
      
      ;; It's all good
      (remhash listname *user-lists*)
      (setf (range combo) (remove listname (range combo)))
      (setf (value combo) (first (range combo)))
      (update-user-lists form)
      (refresh-apply-button form)))
  t) ; Accept the new value


(defun init-func ()
  (let ((window (default-init-function)))
    (move-window window (box-top-left
                         (center-box-on-screen (width window)(height window))))
    
    (setf *progpath* (first sys::*application-command-line-arguments*))
    (setf *configfile* (user::get-nfs-server-config-file))
    (if* (null *configfile*)
       then
            (setf *server-running* nil)
            (setf *configfile* (merge-pathnames "..\\nfs.cfg" *progpath*))
            ;; for testing during development
            (if (not (probe-file *configfile*))
                (setf *configfile* "c:\\devel\\nfs\\nfs.cfg"))
       else
            (setf *server-running* t))
    (process-config *configfile* window)
    window))

;; returns nil if cancelled.
(defun save-config-common (form)
  ;; sanity checks.
  (dolist (exp *exports*)
    (when (not (sanity-check-export form exp))
      (setf (value (my-find-component :export-selection-combo form)) (nfs-export-name exp))
      (setf (value (my-find-component :tab-control form)) :exports)
      (return-from save-config-common)))
  
  ;; Finalize port numbers
  (dolist (entry *subprogram-ports*)
    (let ((value (value (my-find-component (first entry) form))))
      (setf (symbol-value (second entry)) 
        (if* (string= value "")
           then nil
           else (parse-integer value)))))
  
  (write-config-file *configfile*)
  (if *server-running* 
      (user::reload-nfs-server-config))
  t)

  

;; the OK button
(defun configform-okay-button-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let ((form (parent widget)))
    (if (save-config-common form)
        (user-close form)))
  t) ; Accept the new value

;; apply button.
(defun configform-apply-button-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (if (save-config-common (parent widget))
      (setf (available widget) nil))
  t) ; Accept the new value

(eval-when (eval load) (require :pprint))

(defun write-config-file (filename)
  (setf filename (namestring filename))
  (let ((tmpname (concatenate 'string filename ".new")))
    (with-open-file (f tmpname :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
      (let ((*print-right-margin* 55)
	    (*print-pretty* nil))
	(pprint (generate-config-expression) f)))
    (delete-file filename)
    (rename-file tmpname filename)))

(defun configform-nfs-debug-checkbox-on-change (widget
                                                new-value
                                                old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let ((form (parent widget)))
    (setf *nfs-debug* new-value)
    
    (macrolet ((set-filters-availability (types)
                                         (let (res)
                                           (dolist (type types)
                                             (let ((component-name (intern (format nil "debug-nfs-~a" type) :keyword)))
                                               (push `(let ((wij (my-find-component ,component-name form)))
                                                        (setf (available wij) *nfs-debug*))
                                                     res)))
                                           
                                           (setf res (nreverse res))
                                           `(progn ,@res))))
      
      (set-filters-availability #.*nfs-debug-types*))
    
    (refresh-apply-button (parent widget))
    t)) ; Accept the new value

(defun configform-mountd-debug-checkbox-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setf mount:*mountd-debug* new-value)
  (refresh-apply-button (parent widget))
  t) ; Accept the new value

(defun configform-showmount-disable-checkbox-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setf mount:*showmount-disabled* new-value)
  (refresh-apply-button (parent widget))
  t)

(defun configform-gc-debug-checkbox-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setf *nfs-gc-debug* new-value)
  (refresh-apply-button (parent widget))
  t) ; Accept the new value

(defun configform-combo-box-port-mapper-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setq portmap:*use-system-portmapper*
    (if* (eq :auto new-value)
       then :auto
     elseif (eq :yes new-value)
       then t
       else nil))
  (refresh-apply-button (parent widget))
  t) ; Accept the new value



(defun root-has-read-access-p (exp)
  (block nil
    (if (= (nfs-export-uid exp) 0)
        (return t))
    (dolist (ulist-name (append (nfs-export-rw-users exp)
				(nfs-export-ro-users exp)))
      (let ((list (gethash ulist-name *user-lists*)))
        (if (or (member t list) (member 0 list))
            (return t))))))

(defun refresh-apply-button (form)
  (when *server-running*
    (setf (available (my-find-component :apply-button form)) t)))

(defun configform-help-button-on-change (widget new-value old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (do-help widget)
  t) ; Accept the new value

  
(defun on-port-number-change-common (widget new-value old-value)
  (declare (ignore old-value))
  (setf new-value (string-trim '(#\space) new-value))
  
  (if* (string= new-value "")
     then (setf (value widget) new-value)
          (return-from on-port-number-change-common t))
  
  (let ((port (extract-number new-value)))
    (if* (and port (> port 0) (< port 65536))
       then (setf (value widget) (format nil "~d" port))
            t ;; accept
       else (pop-up-message-dialog (parent widget)
                                   "Invalid port number" 
                                   "The number number must be an integer between 1 and 65535" error-icon "OK")
            nil ;; don't accept
            )))
    
(defun configform-portmap-debug-checkbox-on-change (widget
                                                    new-value
                                                    old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setf portmap:*portmap-debug* new-value)
  (refresh-apply-button (parent widget))  
  t) ; Accept the new value

(defun configform-nfs-debug-filter-on-change (widget
                                              new-value
                                              old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (let ((code (symbol-value (intern (format nil "*nfs-debug-~a*" (string-downcase (title widget))) :user))))
    (if* new-value
       then (setf *nfs-debug-filter* (logior *nfs-debug-filter* code))
       else (setf *nfs-debug-filter* (logand *nfs-debug-filter* (lognot code)))))
  (refresh-apply-button (parent widget))
  t) ; Accept the new value

(defun configform-nsm-debug-checkbox-on-change (widget
                                                new-value
                                                old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setf nsm:*nsm-debug* new-value)
  (refresh-apply-button (parent widget))
  t) ; Accept the new value

(defun configform-nlm-debug-checkbox-on-change (widget
                                                new-value
                                                old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setf nlm:*nlm-debug* new-value)
  (refresh-apply-button (parent widget))
  t) ; Accept the new value

(defun configform-set-mtime-on-write-checkbox-on-change (widget
                                                         new-value
                                                         old-value)
  (declare (ignore-if-unused widget new-value old-value))
  (setf *nfs-set-mtime-on-write* new-value)
  (refresh-apply-button (parent widget))
  t) ; Accept the new value
