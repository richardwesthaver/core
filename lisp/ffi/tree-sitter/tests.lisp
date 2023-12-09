(defpackage :tree-sitter/tests
  (:use :cl :rt :tree-sitter))

(in-package :tree-sitter/tests)

(defsuite :tree-sitter)

(in-suite :tree-sitter)

(load-tree-sitter)

(deftest ts-basic ()
  (load-tree-sitter-json)
  (let ((parser (ts-parser-new)))
    (ts-parser-set-language parser (tree-sitter-json))))
