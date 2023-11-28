;;; skel.lisp --- skeleton library

;; A hacker's project compiler.

;;; Commentary:

;;; Code:
(uiop:define-package :skel/pkg
    (:nicknames :skel)
  (:use :cl :std :sb-mop :cl-ppcre)
  (:import-from :sb-unix :uid-username)
  (:import-from :uiop :ensure-absolute-pathname)
  (:shadowing-import-from :uiop :pathname-parent-directory-pathname :read-file-forms)
  (:export
   ;; cfg
   :*skel-project* :*skel-user-config* :*default-skelrc* :*skel-project-registry* 
   :*default-skelfile* :*default-skel-user* :*default-skel-cache* :*default-user-skel-config* 
   :*default-system-skel-config* :*skelfile-extension* :*skelfile-boundary*
   ;; proto
   :sk-run
   :sk-new
   :sk-save
   :sk-tangle
   :sk-weave
   :sk-call
   :sk-print
   :sk-load
   :sk-compile
   :rehash-object
   :sk-transform
   :sk-read-file
   :sk-write
   :sk-writeln
   :sk-write-string
   :sk-write-file
   :sk-read-file
   :sk-install-user-config
   ;; obj
   :skel :sk-meta :def-sk-class :sk-project :sk-target :sk-source
   :sk-rule :sk-rule-target :sk-rule-source :sk-rule-recipe :make-sk-rule 
   :sk-description :sk-kind :sk-rules :sk-id :sk-version :sk-name :sk-docs :sk-document 
   :sk-command :sk-scripts :sk-script :sk-config :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   ;; accesors
   :sk-vc
   ;; header
   :make-file-header :make-shebang-file-header :make-source-file-header :file-header-kind
   :make-source-header-comment :make-shebang-comment 
   ;; utils
   :init-skelfile :init-skelrc :load-skelrc :find-skelfile :load-skelfile
   :describe-skeleton :describe-project :init-skel-user-config
   ;; vm
   :make-stack-slot :make-sk-vm :sks-ref :sks-pop :sks-push))

(in-package :skel/pkg)
(defvar *skel-version* "0.1.0")
(provide :skel)
