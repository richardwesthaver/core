;;; skel.lisp --- skeleton library

;; A hacker's project compiler.

;;; Commentary:

;;; Code:

(uiop:define-package :skel
    (:use-reexport :skel/core :skel/comp))

(uiop:define-package :skel/core
    (:use :cl :cl-ppcre :std :sb-mop :sb-bsd-sockets :sb-unix)
  (:import-from :uiop :read-file-forms :ensure-absolute-pathname)
  (:import-from :uiop/pathname :pathname-parent-directory-pathname)
  (:import-from :sb-ext :run-program)
  (:shadowing-import-from :uiop :pathname-parent-directory-pathname :read-file-forms)
  (:export 
   ;; err
   :skel-syntax-error
   :skel-fmt-error
   :skel-compile-error
   ;; proto
   :rehash-object
   :sk-run :sk-new :sk-save
   :sk-tangle :sk-weave
   :sk-call :sk-print :sk-load
   :sk-compile :sk-transform
   :sk-write :sk-writeln
   :sk-write-string :sk-write-file
   :sk-read-file :sk-install-user-config
   ;; header
   :make-file-header 
   :make-shebang-file-header 
   :make-source-file-header 
   :file-header-kind
   :make-source-header-comment 
   :make-shebang-comment
   ;; vc
   :*hg-program* :*git-program*
   :run-hg-command :run-git-command
   :repo :hg-repo :git-repo
   :vc-meta :hg-meta :git-meta
   :hg-client :make-hg-client
   ;; virt
   :containerfile
   :*default-containerfile*
   ;; obj
   :*skel-project* :*skel-user-config* :*default-skelrc* :*skel-project-registry* 
   :*default-skelfile* :*default-skel-user* :*default-skel-cache* :*default-user-skel-config* 
   :*default-user-skelrc* :*default-system-skel-config* :*skelfile-extension* :*skelfile-boundary*
   :*default-skel-stash* :*default-skel-shed* :*default-system-skelrc*
   :file-read-forms :load-ast
   :sk-author :sk-path :sk-shed :sk-stash :sk-user
   :skel :sk-meta :def-sk-class :sk-project :sk-target :sk-source :sk-vc
   :sk-rule :sk-rule-target :sk-rule-source :sk-rule-recipe :make-sk-rule 
   :sk-description :sk-kind :sk-rules :sk-id :sk-version :sk-name :sk-docs :sk-document 
   :sk-command :sk-scripts :sk-script :sk-config :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   :sk-user-config
   ;; util
   :init-skelrc :load-skelrc
   :init-skel-user-config
   :init-skelfile
   :load-skelfile
   :find-skelfile
   :find-project-root
   :describe-skeleton
   :describe-project
   ;; vm
   :make-stack-slot :make-sk-vm :sks-ref :sks-pop :sks-push))

(defpackage :skel/comp
  (:use :cl :std :skel/core)
  (:export
   ;; asd
   :sk-asd
   ;; ignore
   ;; containerfile
   ;; makefile
   :*default-makefile* :*makefile-extension* 
   :*mk-magic-vars* :*mk-command-prefixes*
   :mk-val-designator 
   :mk-val :mk-var
   :makefile :push-rule :push-directive :push-var))

(defpackage :skel/tools
  (:use :cl :std :dot :skel/core :skel/comp)
  (:export
   ;; deploy
   ;; viz
))
