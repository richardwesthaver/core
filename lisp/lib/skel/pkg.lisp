;;; skel/pkg.lisp --- skeletons

;; Project composition library.

;;; Commentary:

;; The SKEL system consists of a core package and a compiler package -
;; SKEL/CORE and SKEL/COMP respectively. The core contains all of the
;; low-level bits and a CLOS API for interacting with SKEL
;; objects. The compiler package depends on the core and provides
;; primitive compilers for translating SKEL objects into foreign
;; formats. For example, SK-RULE objects may be translated into a
;; corresponding GNU Make Rule. The compiler packages implement the
;; CLOS API defined in the core and may serve as a useful guide for
;; further extending the system yourself.

;; There are some built-in extensions available in addition to the
;; core and compiler - SKEL/VIZ provides an API for generating
;; visualizations of SKEL objects, and SKEL/DEPLOY introduces CI,
;; Release and packaging features.

;;; TODO: 

;;  IMPL 2024-02-12: viz

;;  IMPL 2024-02-12: deploy

;;  IMPL 2024-02-12: ext api

;; 

;;; Code:
(defpackage :skel/core
  (:use :cl :cl-ppcre :std :sb-mop :obj/id :sb-bsd-sockets :sb-unix :sxp :log :cli :obj)
  (:import-from :cli :find-exe)
  (:import-from :uiop :read-file-forms :ensure-absolute-pathname)
  (:import-from :uiop/pathname :pathname-parent-directory-pathname)
  (:import-from :uiop :with-current-directory)
  (:import-from :sb-ext :run-program)
  (:import-from :std :when-let)
  (:import-from :sxp :form)
  (:export
   ;; err
   :skel-error
   :skel-syntax-error
   :skel-fmt-error
   :skel-compile-error
   ;; proto
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
   ;; virt
   :containerfile
   :*default-containerfile*
   ;; obj
   :*user-skelrc* :*system-skelrc*
   :*skel-project* :*skel-user-config* :*default-skelrc* :*skel-registry* :*skel-cache*
   :*default-skelfile* :*default-skel-user* :*default-skel-cache* :*default-user-skel-config* 
   :*default-user-skelrc* :*default-system-skel-config* :*skelfile-extension* :*skelfile-boundary*
   :*default-skel-stash*
   :*default-system-skelrc*
   :file-read-forms :load-ast
   :sk-author :sk-path :sk-stash :sk-cache :sk-registry :sk-user
   :sk-push :sk-pull
   :edit-skelrc
   :skel :sk-meta :def-sk-class :sk-project :sk-target :sk-source :sk-vc
   :sk-rule :sk-rule-target :sk-rule-source :sk-rule-recipe :make-sk-rule 
   :sk-description :sk-kind :sk-rules :sk-version :sk-name :sk-docs :sk-document 
   :sk-command :sk-scripts :sk-script :sk-config :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   :sk-user-config
   :sk-system-config
   ;; util
   :init-skelrc :load-skelrc
   :init-user-skelrc :load-user-skelrc
   :init-system-skelrc :load-system-skelrc
   :init-skelfile
   :load-skelfile
   :find-skelfile
   :find-project-root
   :describe-skeleton
   :describe-project
   ;; vm
   :make-stack-slot :make-sk-vm :sks-ref :sks-pop :sks-push))

(defpackage :skel/comp
  (:use :cl :std :skel/core :sxp)
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

(uiop:define-package :skel
    (:use :cl :std :skel/core :skel/comp)
    (:use-reexport :skel/core :skel/comp))

;;; Tools
(defpackage :skel/viz
  (:use :cl :std :skel)
  (:export))

(defpackage :skel/deploy
  (:use :cl :std :skel)
  (:export))

;;; Extensions
(defpackage :skel/asdf
  (:use :cl :std :skel :asdf)
  (:shadowing-import-from :asdf :circular-dependency)
  (:export))
