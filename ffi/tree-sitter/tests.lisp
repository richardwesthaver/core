(defpackage :tree-sitter/tests
  (:use :cl :rt :tree-sitter :std))

(in-package :tree-sitter/tests)

(defsuite :tree-sitter)

(in-suite :tree-sitter)

(load-tree-sitter)
(load-tree-sitter-alien)
(load-tree-sitter-json)
(load-tree-sitter-rust)

;; the following tests require the TREE-SITTER-LANGS pack to be installed
(deftest ts-json ()
  (let ((parser (ts-parser-new))
        (lang (tree-sitter-json)))
    (is (= (ts-language-version lang) 14))
    (is (ts-language-field-count lang))
    (is (ts-language-symbol-count lang))
    (is (ts-language-symbol-name lang (sb-alien:make-alien sb-alien:unsigned-int)))
    (is (ts-parser-set-language parser lang))
    (let ((new-tree (ts-parser-parse-string parser nil "[1, null]" 9)))
      (print (ts-tree-language new-tree))
      (ts-tree-delete new-tree))))

(deftest ts-rust ()
  (let ((parser (ts-parser-new))
        (lang (tree-sitter-rust)))
    (is (= (ts-language-version lang) 14))
    (is (ts-parser-set-language parser lang))
    (let ((new-tree (ts-parser-parse-string parser nil "
pub fn main {} " 15)))
      (is (= (ts-language-version (ts-tree-language new-tree)) 14))
      (let ((root-node (ts-tree-root-node-pointer new-tree)))
        (is (string= "source_file" (ts-node-type-pointer root-node)))
        (let ((cursor (ts-tree-cursor-new-pointer root-node)))
          (ts-tree-cursor-delete cursor)
          (ts-tree-delete new-tree))))))
