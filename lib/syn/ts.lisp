;;; ts.lisp --- Tree-sitter API

;; High-level Tree-sitter API

;;; Commentary:

;; Tree-sitter is currently the most well-supported syntax parsing backend in
;; use by IDEs, and we can generally rely on the various language grammars
;; defined via tree-sitter. As we progress we may adapt other more direct
;; methods of building ASTs by querying compilers directly, but for now we
;; have some catching up to do :).

;;; Code:
(in-package :syn/ts)
(load-alien :tree-sitter)

(defun parse-file (lang path &key (start 0) end)
  (let ((str (read-file path)))
    (parse-lang-string 
     lang
     str
     :start start
     :end (or end (length str)))))

(defun ts-file-query (lang path query)
  (let ((input (with-output-to-string (s) (write-file-into-stream path s))))
    (with-ts-query lang (q query)
      (with-ts-query-cursor c
        (let ((tree (parse-lang-string lang input)))
          (tree-sitter::ts-query-cursor-exec c q (tree-sitter::ts-tree-root-node tree))
          c)))))
