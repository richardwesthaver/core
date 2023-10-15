(defsystem "rt"
  :version "0.1.0"
  :author "ellis <ellis@rwest.io>"
  :description "regression test framework"
  :bug-tracker "https://lab.rwest.io/ellis/macs/issues"
  :source-control (:hg "https://lab.rwest.io/ellis/macs")
  :depends-on (:macs :sxp)
  :in-order-to ((test-op (test-op "macs/tests")))
  :components ((:file "rt")))
