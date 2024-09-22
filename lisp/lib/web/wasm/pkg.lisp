;;; pkg.lisp --- Wasm Packages

;; https://webassembly.github.io/spec/

;;; Code:
(defpackage :web/wasm/text
  (:use :cl :std :dat/proto :dat/sxp))

(defpackage :web/wasm/binary
  (:use :cl :std :dat/proto))

;; (defpackage :web/wasm/vm
;;   (:use :cl :std :web/sys :web/wasm/binary))
