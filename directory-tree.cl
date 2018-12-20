;; This file implements a tree of nodes, each of which may contain a piece of
;; data.  The tree starts with an unnamed root node.  Children of a node are
;; addressed by name.

;; A path to a node in the tree can be specified as a list of strings or as a
;; single string which will be parsed into a list of strings.

(defpackage :directory-tree
  (:use :cl :excl)
  (:export
   #:make-directory-tree
   #:insert-directory-tree
   #:map-directory-tree
   #:find-nearest-data
   #:find-data
   #:canonicalize-path
   ))

(in-package :directory-tree)

(defstruct node
  name ;; a string, or nil for the root node
  data ;; Any Lisp object.  nil indicates no data.
  children ;; alist mapping from string to node.
  parent ;; backlink to parent node.  Will be nil for the root node
  )

;; Exported
(defun make-directory-tree ()
  ;; Return the root node
  (make-node))

;; Exported
;; Inserts/updates DATA at location PATH in the directory tree rooted at NODE,
;; creating intermediate nodes as needed.
;; Returns DATA.
(defmethod insert-directory-tree ((node node) (path list) data)
  (multiple-value-bind (node path)
      (search-directory-tree node path)
    ;; Add new nodes for the remaining path components.
    (dolist (child-name path)
      (setf node (add-child node child-name)))
    
    (setf (node-data node) data)
    
    data))

;; Exported
(defmethod insert-directory-tree ((node node) (path string) data)      
  (insert-directory-tree node (parse-directory-path path) data))

;; Exported
;; CALLBACK will be called once for each node in the tree rooted at NODE, for
;; each node that has non-nil data.  CALLBACK will be passed two arguments:
;; 1) The canonical node path (a string starting with a slash)
;; 2) The node data
(defun map-directory-tree (node callback)
  (declare (dynamic-extent callback))
  (map-directory-tree-1 node "/" callback))
  
;; Exported
;; Finds the deepest node along PATH which has non-nil data.
;; If such a node is found, returns
;; 1) The node data
;; 2) A string represending the remainder of PATH which is below the selected
;;    node.  This will be nil if PATH matches a non-nil data node exactly.
;; Otherwise returns nil.
(defun find-nearest-data (node path)
  (multiple-value-bind (node tail)
      (search-directory-tree node path)
    
    ;; Now walk back up the tree until we meet a node with
    ;; data
    (while (and node (null (node-data node)))
      ;; We must update TAIL as we walk back up the tree.
      (push (node-name node) tail)
      (setf node (node-parent node)))

    (when node
      (values 
       ;; 1
       (node-data node) 
       ;; 2
       (when tail
         (list-to-delimited-string tail "/"))))))

;; Exported 
;; Finds the node matching PATH (a string, or a list of path component strings).
;; If the node is found and it has non-nil data, the data is returned.
;; Otherwise nil is returned.
(defun find-data (node path)
  (multiple-value-bind (node tail)
      (search-directory-tree node path)
    (when (and node (null tail))
      (node-data node))))

;; Exported
(defmethod canonicalize-path ((path list))
  (let ((res "/"))
    (dolist (component path)
      (setf res (append-name res component)))
    res))

;; Exported
(defmethod canonicalize-path ((path string))
  (canonicalize-path (parse-directory-path path)))


;;;;;;;;;;;;;
;; Innards ;;
;;;;;;;;;;;;;

;; Return the child of NODE which is named CHILD-NAME, or nil if 
;; no such child.
(defun get-node-child (node child-name)
  (cdr (assoc child-name (node-children node) :test #'string=)))

;; Repeatedly evaluates BODY with CHILD-NODE-VAR bound each
;; child of NODE. 
(defmacro do-children ((child-node-var node) &body body)
  (let ((entry (gensym "entry")))
    `(dolist (,entry (node-children ,node))
       (let ((,child-node-var (cdr ,entry)))
         ,@body))))

(defmethod print-object ((node node) stream)
  (let (child-names)
    (do-children (child-node node)
      (push (node-name child-node) child-names))
      
    (format stream "[node ~s with data ~a, and children ~s]"
            (node-name node)
            (node-data node)
            child-names)))

(defmethod search-directory-tree ((node node) (path list))
  (if* path
     then (let ((child (get-node-child node (first path))))
            (if* child
               then ;; Good to descend
                    (search-directory-tree child (rest path))
               else ;; No child by that name.  Can't go any further.
                    ;; Return the node that we stopped on and the remaining path components.
                    (values node path)))
     else ;; Path exhausted.  Return the node that we stopped on.
          node))

(defmethod search-directory-tree ((node node) (path string))
  (search-directory-tree node (parse-directory-path path)))

;; Makes a new node and adds it as a child (named CHILD-NAME) of NODE.
(defun add-child (node child-name)
  (let ((child-node (make-node :name child-name)))
    (setf (node-parent child-node) node)
    (push (cons child-name child-node) (node-children node))
    child-node))

(defun append-name (node-path child-name)
  (if* (match-re "/$" node-path)
     then ;; current node name ends with a slash.
          (concatenate 'string node-path child-name)
     else ;; current node names does not end with a slash.
          ;; add one.
          (concatenate 'string node-path "/" child-name)))

(defun map-directory-tree-1 (node node-path callback)
  ;; Call the callback for this node if it has data.
  (let ((data (node-data node)))
    (when data
      (funcall callback node-path data)))

  (do-children (child-node node)
    (map-directory-tree-1 child-node (append-name node-path (node-name child-node)) callback)))

;; Parses PATH (a string) and returns
;; a list of component names.
;; Note that this does not do special things with components
;; like "." and ".." (though it could).
(defun parse-directory-path (path)
  (check-type path string)
  
  (let ((res (split-re "/+" path))) ;; split on sequences of one or more slashes
    ;; Discard any leading blank string component (which will occur
    ;; if PATH begins with a slash).
    (when (and (first res) (string= (first res) ""))
      (pop res))
    
    res))

#+ignore
(defun test-parse-directory-path ()
  (let ((cases '(
                 (""      nil)
                 ("/"     nil)
                 ("/a"    ("a"))
                 ("a"     ("a"))
                 ("a/"    ("a"))
                 ("/a/b"  ("a" "b"))
                 ("a/b"   ("a" "b"))
                 ("/a/b/" ("a" "b"))
                 ("a/b/"  ("a" "b"))
                 )))
    (loop for (path expected-output) in cases
        do (let ((output (parse-directory-path path)))
             (when (not (equalp output expected-output))
               (error "Expected ~s to parse to ~s, but got ~s instead"
                      path expected-output output))))))
