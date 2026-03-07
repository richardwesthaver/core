(defpackage :tree-sitter/tests
  (:use :cl :rt :tree-sitter :std))

(in-package :tree-sitter/tests)

(defsuite :tree-sitter)

(in-suite :tree-sitter)

(load-tree-sitter)

;; the following tests require the TREE-SITTER-LANGS pack to be installed
(deftest ts-json ()
  (load-tree-sitter-json)
  (let ((parser (ts-parser-new))
        (lang (tree-sitter-json)))
    (is (= (ts-language-abi-version lang) 14))
    (is (ts-language-field-count lang))
    (is (ts-language-symbol-count lang))
    (is (ts-parser-set-language parser lang))
    (let ((new-tree (ts-parser-parse-string parser nil "[1, null]" 9)))
      (is (ts-tree-language new-tree))
      (ts-tree-delete new-tree))))

(deftest ts-rust ()
  (load-tree-sitter-rust)
  (let ((parser (ts-parser-new))
        (lang (tree-sitter-rust)))
    (is> (ts-language-abi-version lang) 14)
    (is (ts-parser-set-language parser lang))
    (let ((new-tree (ts-parser-parse-string parser nil "
pub fn main {} " 15)))
      (is> (ts-language-abi-version (ts-tree-language new-tree)) 14)
      (let ((root-node (ts-tree-root-node new-tree)))
        (is (string= "source_file" (ts-node-type root-node)))
        (let ((cursor (ts-tree-cursor-new root-node)))
          (ts-tree-delete new-tree))))))

(deftest ts-query-c ()
  (load-tree-sitter-c)
  (let ((src "#define __bitwise__ __bitwise
"))
    (declare (ignore src))
    (with-ts-query :c (q '(_))
      (istype 'sb-alien::alien-value q)
      (with-ts-query-cursor c
        (istype 'sb-alien::alien-value c)
        (let ((m (sb-alien:make-alien tree-sitter::ts-query-match)))
          (declare (ignore m))
          (ts-query-cursor-delete c))
        (is= 1 (tree-sitter::ts-query-pattern-count q))
        (iszero (tree-sitter::ts-query-capture-count q))
        (ts-query-delete q)))))

