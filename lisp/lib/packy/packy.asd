#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem "packy"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :version "0.1.0"
  :maintainer "ellis <ellis@rwest.io>"
  :bug-tracker "https://lab.rwest.io/ellis/packy/issues"
  :depends-on (:uiop :asdf :sb-posix :sb-bsd-sockets :sb-concurrency :std :cl-ppcre :packy/cli)
  :build-operation "program-op"
  :build-pathname "packy"
  :entry-point "packy/cli:main"
  :in-order-to ((test-op (test-op :packy/tests)))
  :perform (test-op (o c) (symbol-call :std/rt :do-tests :packy)))
