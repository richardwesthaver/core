;;; proto.lisp --- PARSE Protocols

;; Parsing Protocols

;;; Code:
(in-package :parse/proto)

;; note that PARSE-ERROR is defined by CL
(define-condition parser-condition () ())

(deferror parser-error (parser-condition) () (:auto t))

(deferror simple-parser-error (parser-error simple-error) () (:auto t))

(defgeneric parse (self &optional precedence))
