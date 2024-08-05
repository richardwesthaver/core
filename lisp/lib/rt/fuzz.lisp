;;; fuzz.lisp --- RT Fuzz

;; FUZZER API

;;; Code:
(in-package :rt/fuzz)

(defclass fuzzer ()
  ((state)))

(defgeneric fuzz (self &optional n))
(defgeneric fuzz* (self &key &allow-other-keys))
