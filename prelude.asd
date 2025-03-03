(pushnew :prelude *features*)
(defsystem :prelude
  :depends-on (:core :user :core/tests :core/bench)
  :build-operation monolithic-compile-bundle-op
  :build-pathname "prelude")
