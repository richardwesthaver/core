;;; pkg.lisp --- SKEL/COMP packages

;; Skel Components

;;; Code:

(defpackage :skel/comp/makefile
  (:use :cl :std :skel/core/obj :skel/core/proto :skel/core/header :skel/core/util)
  (:export
   :*default-makefile* :*makefile-extension* 
   :*mk-magic-vars* :*mk-command-prefixes*
   :push-mk-rule :push-mk-var :push-mk-directive
   :mk-val-designator 
   :mk-val :mk-var
   :makefile))

(defpackage :skel/comp/cargo
  (:use :cl :std :skel/core/obj :skel/core/proto :toml :build :config :cli/tools/rust :skel/core/util)
  (:export :sk-rust-system :parse-sk-rust-system))

(defpackage :skel/comp/sys
  (:use :cl :skel/core/obj :skel/core/proto :skel/core/util :std/defsys))

(pkg:defpkg :skel/comp/asd
  (:shadowing-import-from :std :version)
  (:import-from :std :defmethods :when-let)
  (:use :cl :skel/core/obj :skel/core/proto :skel/core/util :std/macs :asdf)
  (:import-from :asdf :system :coerce-name 
   :system-source-file :parse-component-form :file-component :component-relative-pathname
   :component-if-feature :component-depends-on :module-components :component-name
   :component-version :system-depends-on :system-description :system-long-description
   :system-author :system-maintainer :system-mailto :system-license
   :system-homepage :system-bug-tracker :system-source-control :component-in-order-to
   :component-build-pathname :component-build-operation :component-entry-point)
  (:export :sk-lisp-system :read-system-definitions :parse-sk-lisp-system :sk-write-asd-components))

(defpackage :skel/comp/lisp
  (:import-from :skel/core/int :*skel-project*)
  (:shadowing-import-from :std :version)
  (:use :cl :std :skel/core/obj :skel/core/proto :id :skel/core/util)
  (:import-from :ast :ast :read-ast :write-ast :load-ast)
  (:export :sk-lisp-file))

(defpackage :skel/comp/container
  (:use :cl :std :pod :skel/core/obj :skel/core/proto :dat/proto :obj/id :skel/core/util)
  (:export :sk-containerfile))

(defpackage :skel/comp/ignition
  (:use :cl :std :box :skel/core/obj :skel/core/proto :dat/proto :obj/id :skel/core/util)
  (:export :sk-ignition))

(defpackage :skel/comp/dir-locals
  (:use :cl :std :skel/core/obj :skel/core/proto :skel/core/util)
  (:export :*dir-locals-file* :dir-local-var-designator :sk-dir-locals))

(defpackage :skel/comp/org
  (:use :cl :std :skel/core/obj :skel/core/proto :organ :obj/id :skel/core/int :skel/core/util)
  (:export :sk-org-file))
