(defpackage :tree-sitter/tests
  (:use :cl :rt :tree-sitter :std))

(in-package :tree-sitter/tests)

(defsuite :tree-sitter)

(in-suite :tree-sitter)

(load-tree-sitter)
(load-tree-sitter-wrapper)
(load-tree-sitter-json)
(load-tree-sitter-rust)

(deftest ts-json ()
  (load-tree-sitter-json)
  (let ((parser (ts-parser-new))
        (lang (tree-sitter-json)))
    (is (= (ts-language-version lang) 14))
    (is (ts-parser-set-language parser lang))
    (let ((new-tree (ts-parser-parse-string parser nil "[1, null]" 9)))
      ;; (ts-tree-root-node new-tree)
      )))

;; to compile the parser/scanner:
#|

-o
|#
(deftest ts-rust ()
  (load-tree-sitter-rust)
  (let ((parser (ts-parser-new))
        (lang (tree-sitter-rust))
        root)
    (is (= (ts-language-version lang) 14))
    (is (ts-parser-set-language parser lang))
    (let ((new-tree (ts-parser-parse-string parser nil "
pub fn main {} " 15)))
      (is (= (ts-language-version (ts-tree-language new-tree)) 14))
      (ts-tree-delete new-tree))))



