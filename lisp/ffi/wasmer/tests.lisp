;;; wasmer/tests.lisp --- libwasmer tests

;;; Code:
(defpackage :wasmer/tests
  (:use :cl :std :rt :wasmer))

(in-package :wasmer/tests)

(defsuite :wasmer)
(in-suite :wasmer)

(load-wasmer)

(deftest sanity ()
  (is (stringp (wasmer::wasmer-version))))

(deftest basic ()
  (let* ((engine (wasmer::wasm-engine-new))
         (store (wasmer::wasm-store-new engine)))
    (isnt (wasmer::wasm-store-delete store))
    (isnt (wasmer::wasm-engine-delete engine))))
