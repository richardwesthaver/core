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

(defpackage :skel/comp/asd
  (:use :cl :std :skel/core/obj :skel/core/proto :asdf :skel/core/util)
  (:shadow :circular-dependency)
  (:export :sk-lisp-system :read-system-definitions :parse-sk-lisp-system :sk-write-asd-components))

(defpackage :skel/comp/lisp
  (:use :cl :std :skel/core/obj :skel/core/proto :id :asdf :skel/core/util)
  (:import-from :ast :ast :read-ast :write-ast :load-ast)
  (:import-from :skel/core/int :*skel-project*)
  (:export :sk-lisp-file))

(defpackage :skel/comp/container
  (:use :cl :std :pod :skel/core/obj :skel/core/proto :dat/proto :obj/id :skel/core/util)
  (:export :sk-containerfile))

(defpackage :skel/comp/ignition
  (:use :cl :std :box :skel/core/obj :skel/core/proto :dat/proto :obj/id :skel/core/util)
  (:export :sk-ignition))

(defpackage :skel/comp/dir-locals
  (:use :cl :std :skel/core/obj :skel/core/proto :dat/sxp :skel/core/util)
  (:export :*dir-locals-file* :dir-local-var-designator :sk-dir-locals))

(defpackage :skel/comp/org
  (:use :cl :std :skel/core/obj :skel/core/proto :organ :obj/id :skel/core/int :skel/core/util)
  (:export :sk-org-file))
