;;; pkg.lisp --- SKEL/COMP packages

;; Skel Components

;;; Code:

(defpackage :skel/comp/makefile
  (:use :cl :std :skel/core :project)
  (:export
   :*default-makefile* :*makefile-extension* 
   :*mk-magic-vars* :*mk-command-prefixes*
   :push-mk-rule :push-mk-var :push-mk-directive
   :mk-val-designator 
   :mk-val :mk-var
   :makefile))

(defpackage :skel/comp/cargo
  (:use :cl :std :skel/core :toml :build :config :cli/tools/rust)
  (:export :sk-rust-system :parse-sk-rust-system))

(defpackage :skel/comp/sys
  (:use :cl :skel/core :std/defsys))

(pkg:defpkg :skel/comp/asd
  (:shadowing-import-from :std :version)
  (:import-from :std :defmethods :when-let)
  (:use :cl :skel/core :std/macs :asdf)
  (:import-from :asdf :system :coerce-name 
   :system-source-file :parse-component-form :file-component :component-relative-pathname
   :component-if-feature :component-depends-on :module-components :component-name
   :component-version :system-depends-on :system-description :system-long-description
   :system-author :system-maintainer :system-mailto :system-license
   :system-homepage :system-bug-tracker :system-source-control :component-in-order-to
   :component-build-pathname :component-build-operation :component-entry-point)
  (:export :sk-lisp-system :read-system-definitions :parse-sk-lisp-system :sk-write-asd-components))

(defpackage :skel/comp/lisp
  (:import-from :skel/core :*skel-project*)
  (:shadowing-import-from :std :version)
  (:use :cl :std :skel/core :id)
  (:import-from :ast :ast :read-ast :write-ast :load-ast)
  (:export :sk-lisp-file))

(defpackage :skel/comp/container
  (:use :cl :std :pod :skel/core :dat/proto :obj/id)
  (:export :sk-containerfile))

(defpackage :skel/comp/dir-locals
  (:use :cl :std :skel/core)
  (:export :*dir-locals-file* :dir-local-var-designator :sk-dir-locals))

(defpackage :skel/comp/org
  (:use :cl :std :skel/core :organ :obj/id)
  (:export :sk-org-file))

(defpackage :skel/comp/box
  (:use :cl :std :skel/core :box :obj/id)
  (:export :sk-box-file))

(defpackage :skel/comp/infer
  (:use :cl :std :skel/core :srv :id :ast :dat :config :vc :nlp)
  (:export :sk-infer))
