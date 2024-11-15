;;; ts.lisp --- Treesitter API

;; 

;;; Code:
(in-package :syn/ts)
(load-tree-sitter)
(load-tree-sitter-alien)
;; (load-tree-sitter-c)
(load-tree-sitter-cpp)

;; (with-ts-parser (p)
;;   (parse-string-with-language :c "//foo" p))

;; (with-ts-parser (p
;;     (values
;;      (ts-language-version c)
;;      (ts-language-symbol-count c)
;;      (ts-language-field-count c))))
