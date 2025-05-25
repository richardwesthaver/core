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
(load-tree-sitter)
(load-tree-sitter-alien)

;; (setq syn/lang:*language* :rust)
(defmacro with-lang (lang &body body)
  `(with-ts-lang syn/lang:*language* ,lang
     ,@body))

(defun lang-stats (lang)
  (with-ts-lang lang l
    (cons (ts-language-symbol-count l)
          (ts-language-field-count l))))

(defun parse-file (lang path &key (produce-cst t) (consume t) (start 0) (end))
  (parse-string 
   lang
   (with-output-to-string (s)
     (write-file-into-stream path s))
   :consume consume
   :produce-cst produce-cst
   :start start
   :end end))

(defun ts-file-query (lang path query)
  (let ((input (with-output-to-string (s) (write-file-into-stream path s))))
    (with-ts-query (q lang query (length query))
      (with-ts-query-cursor c
        (let ((tree (parse-string lang input :consume nil)))
          (tree-sitter::ts-query-cursor-exec-pointer c q (tree-sitter::ts-tree-root-node-pointer tree))
          c)))))
