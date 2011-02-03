;; -*- lisp-version: "8.2 [Windows] (Jan 25, 2010 15:06)"; cg: "1.134"; -*-

(in-package :cg-user)

(define-project :name :configure
  :modules (list (make-instance 'module :name "../xdr")
                 (make-instance 'module :name "../sunrpc-common")
                 (make-instance 'module :name "../portmap-common")
                 (make-instance 'module :name "../mount-common")
                 (make-instance 'module :name "../nsm-common")
                 (make-instance 'module :name "../nlm-common")
                 (make-instance 'module :name "../gen-nfs-common")
                 (make-instance 'module :name "../nfs-common")
                 (make-instance 'module :name "../nfs-shared")
                 (make-instance 'module :name "../portmap-client")
                 (make-instance 'module :name "../sunrpc")
                 (make-instance 'module :name "nfs-server-io")
                 (make-instance 'module :name "export")
                 (make-instance 'module :name "ipaddr")
                 (make-instance 'form-module :name "configform" :finder-function
                                'configform :has-pixmap-file nil)
                 (make-instance 'form-module :name "help-form" :finder-function
                                'help-form :has-pixmap-file nil))
  :projects nil
  :libraries nil
  :editable-files nil
  :distributed-files (list "configuration.txt")
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form 'configform
  :compilation-unit t
  :verbose nil
  :runtime-modules (list :cg-dde-utils :cg.base :cg.button :cg.caret :cg.check-box
                         :cg.clipboard :cg.clipboard-stack :cg.clipboard.pixmap
                         :cg.combo-box :cg.common-control :cg.comtab :cg.dialog-item
                         :cg.directory-dialog-os :cg.editable-text :cg.group-box :cg.icon
                         :cg.item-list :cg.keyboard-shortcuts :cg.lisp-widget
                         :cg.message-dialog :cg.multi-line-editable-text :cg.os-widget
                         :cg.picture-widget :cg.pixmap :cg.pixmap-widget
                         :cg.pixmap.file-io :cg.radio-button :cg.scroll-bar-mixin
                         :cg.static-text :cg.string-dialog :cg.tab-control
                         :cg.text-edit-pane :cg.text-or-combo :cg.text-widget
                         :cg.toggling-widget :cg.up-down-control :cg.utility-dialog)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags (list :local-name-info)
  :build-flags (list :purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :include-manifest-file-for-visual-styles t
  :default-command-line-arguments "+R +t \"Initializing\""
  :additional-build-lisp-image-arguments (list :read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :build-number 1
  :run-with-console nil
  :project-file-version-info nil
  :on-initialization 'init-func
  :default-error-handler-for-delivery 'report-unexpected-error-and-exit
  :on-restart 'do-default-restart)

;; End of Project Definition
