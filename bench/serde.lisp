;;; serde.lisp --- Serialization Benchmarks

;;; Code:
(defpackage :core/bench/json
  (:nicknames :bench/json)
  (:use :cl :std :rt :log :dat/proto :dat/json))

(in-package :core/bench/json)
(defsuite :json-bench)
(in-suite :json-bench)

(deftest json-stress (:bench 100 :profile t) 
  "Stress test DAT/JSON."
  (is t))
