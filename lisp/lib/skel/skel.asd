#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem "skel"
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/ellis/skel/issues"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :std :organ :cl-ppcre :skel/pkg)
  :in-order-to ((test-op (test-op skel/tests)))
  :perform (test-op (op c) (uiop:symbol-call '#:rt '#:do-tests))
  :build-operation "program-op"
  :build-pathname "skel"
  :entry-point "skel/cli:main")
