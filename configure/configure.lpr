;; -*- lisp-version: "7.0 [Windows] (Sep 13, 2005 10:22)"; cg: "1.54.2.17"; -*-

(in-package :cg-user)

(defpackage :common-graphics-user)

(define-project :name :configure
  :modules (list (make-instance 'module :name "..\\xdr")
                 (make-instance 'module :name "..\\sunrpc")
                 (make-instance 'module :name "..\\portmap")
                 (make-instance 'module :name "nfs-server-io")
                 (make-instance 'module :name "export")
                 (make-instance 'module :name "..\\nfs-shared")
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
  :internally-loaded-files nil
  :project-package-name :common-graphics-user
  :main-form 'configform
  :compilation-unit t
  :verbose nil
  :runtime-modules '(:cg-dde-utils :cg.base :cg.button :cg.caret
                     :cg.check-box :cg.clipboard :cg.clipboard-stack
                     :cg.clipboard.pixmap :cg.combo-box
                     :cg.common-control :cg.comtab :cg.dialog-item
                     :cg.directory-dialog-os :cg.editable-text
                     :cg.group-box :cg.icon :cg.item-list
                     :cg.keyboard-shortcuts :cg.lisp-widget
                     :cg.message-dialog :cg.multi-line-editable-text
                     :cg.os-widget :cg.picture-widget :cg.pixmap
                     :cg.pixmap-widget :cg.pixmap.file-io
                     :cg.radio-button :cg.static-text :cg.string-dialog
                     :cg.tab-control :cg.text-edit-pane
                     :cg.text-or-combo :cg.text-widget
                     :cg.toggling-widget :cg.utility-dialog)
  :splash-file-module (make-instance 'build-module :name "")
  :icon-file-module (make-instance 'build-module :name "")
  :include-flags '(:local-name-info)
  :build-flags '(:purify)
  :autoload-warning t
  :full-recompile-for-runtime-conditionalizations nil
  :default-command-line-arguments "+R +t \"Initializing\""
  :additional-build-lisp-image-arguments '(:read-init-files nil)
  :old-space-size 256000
  :new-space-size 6144
  :runtime-build-option :standard
  :on-initialization 'init-func
  :on-restart 'do-default-restart)

;; End of Project Definition
