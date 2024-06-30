(defpackage :skel/comp/makefile
  (:use :cl :std :skel/core/obj :skel/core/proto :skel/core/header)
  (:export
   :*default-makefile* :*makefile-extension* 
   :*mk-magic-vars* :*mk-command-prefixes*
   :push-mk-rule :push-mk-var :push-mk-directive
   :mk-val-designator 
   :mk-val :mk-var
   :makefile))

(defpackage :skel/comp/cargo
  (:use :cl :std :skel/core/obj :skel/core/proto)
  (:export :sk-rust-system :parse-sk-rust-system))

(defpackage :skel/comp/asd
  (:use :cl :std :skel/core/obj :skel/core/proto :asdf)
  (:shadow :circular-dependency)
  (:export :sk-lisp-system :read-system-definitions :parse-sk-lisp-system :sk-write-asd-components))

(defpackage :skel/comp/container
  (:use :cl :std :pod :skel/core/obj :skel/core/proto :dat/proto :obj/id)
  (:export :sk-containerfile))

(defpackage :skel/comp/dir-locals
  (:use :cl :std :skel/core/obj :skel/core/proto :dat/sxp)
  (:export :*dir-locals-file* :dir-local-var-designator :sk-dir-locals))

(defpackage :skel/comp/org
  (:use :cl :std :skel/core/obj :skel/core/proto :organ :obj/id)
  (:export :sk-org-file))
