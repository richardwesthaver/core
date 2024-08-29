;;; json.lisp --- DAT/JSON Benchmarks

;;; Code:
(defpackage :core/bench/json
  (:nicknames :bench/json)
  (:use :cl :std :rt :rt/bench :rt/cover :log :dat/proto :dat/json))

(in-package :core/bench/json)
(defsuite :json-bench)
(in-suite :json-bench)
