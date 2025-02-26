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

(defun parse-file (lang path)
  (parse-string 
   lang
   (with-output-to-string (s)
     (write-file-into-stream path s))
   :produce-cst nil))

(defun ts-file-query (path)
  (with-ts-query (q :rust 
                    (with-output-to-string (s)
                      (write-file-into-stream path s))
                  0)
    (print q)))
