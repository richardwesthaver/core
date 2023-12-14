(defpackage :tree-sitter/tests
  (:use :cl :rt :tree-sitter :std))

(in-package :tree-sitter/tests)

(defsuite :tree-sitter)

(in-suite :tree-sitter)

(load-tree-sitter)

(deftest ts-json ()
  (load-tree-sitter-json)
  (let ((parser (ts-parser-new))
        (lang (tree-sitter-json)))
    (is (= (ts-language-version lang) 14))
    (is (ts-parser-set-language parser lang))))

(deftest ts-rust ()
  (load-tree-sitter-rust)
  (let ((parser (ts-parser-new))
        (lang (tree-sitter-rust)))
    ;; (is (= (ts-language-version lang) 14))
    (is (ts-parser-set-language parser lang))))


