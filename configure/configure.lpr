;; -*- lisp-version: "6.2 [Windows] (Jan 27, 2004 11:41)"; common-graphics: "1.389.2.105.2.14"; -*-

(in-package :common-graphics-user)

(defpackage :common-graphics-user (:export))

(define-project :name :configure
  :application-type (intern "exe" (find-package :keyword))
  :modules (list (make-instance 'module :name
                                "..\\xdr")
                 (make-instance 'module :name
                                "..\\sunrpc")
                 (make-instance 'module :name
                                "..\\portmap")
                 (make-instance 'module :name "nfs-server-io")
                 (make-instance 'module :name "export")
                 (make-instance 'module :name "ipaddr")
                 (make-instance 'form-module :name "configform"
                                :finder-function 'configform
                                :has-pixmap-file nil :create-on-open t)
                 (make-instance 'form-module :name "help-form"
                                :finder-function 'help-form
                                :has-pixmap-file nil :create-on-open
                                t))
  :projects nil
  :libraries nil
  :distributed-files '("configuration.txt")
  :project-package-name :common-graphics-user
  :main-form 'configform
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:carets :cg :choose-list :color-dialog
                     :common-control :common-status-bar :directory-list
                     :drag-and-drop :drawable :edit-in-place
                     :find-dialog :font-dialog :grid :group-box
                     :header-control :hotspots :lisp-widget
                     :list-view-control :mci :menu-selection
                     :multi-picture-button :outline
                     :progress-indicator-control :rich-edit
                     :string-dialog :tab-control :trackbar-control
                     :up-down-control :www :yes-no-list-dialog)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:local-name-info :top-level)
  :build-flags '(:purify :allow-runtime-debug)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+cx +t \"Initializing\""
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'init-func
  :on-restart 'do-default-restart)

;; End of Project Definition
