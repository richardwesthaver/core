;;; ffi/tree-sitter/lang.lisp --- Tree-sitter Languages

;; Tree-sitter language bindings.

;; in subdirs of *TREE-SITTER-LANGUAGE-DIRECTORY*, there are two json
;; files: node-types.json and grammar.json.

;; node-types: https://tree-sitter.github.io/tree-sitter/using-parsers#static-node-types

;; parsers: https://tree-sitter.github.io/tree-sitter/#available-parsers

;; ref: https://github.com/death/cl-tree-sitter

;;; Code:
(in-package :tree-sitter)

(defun language-module (name)
  (funcall 
   (or (gethash (sb-int:keywordicate name) *ts-langs*) ;; symbol -> keyword, string must be UPCASE
       (error "tree-sitter language module not found: ~s." name))))

(defvar *ts-langs* (make-hash-table))

(macrolet ((def-ts-lang-loader (lang)
             (let ((name (symbolicate 'tree-sitter- lang)))
               (let ((fname (symbolicate 'load- name)))
                 `(prog1
                      (defun ,fname (&optional save)
                        (prog1 (sb-alien:load-shared-object ,(format nil "/usr/local/lib/libtree-sitter-~a.so" lang)
                                                            :dont-save (not save))
                          (pushnew ,(sb-int:keywordicate name) *features*)))
                    (define-alien-routine ,name (* ts-language))
                    (setf (gethash ,(sb-int:keywordicate lang) *ts-langs*) ',name)
                    (export '(,fname ,name)))))))
  (def-ts-lang-loader rust)
  (def-ts-lang-loader json)
  (def-ts-lang-loader c)
  (def-ts-lang-loader bash)
  (def-ts-lang-loader commonlisp)
  (def-ts-lang-loader cpp)
  (def-ts-lang-loader css)
  (def-ts-lang-loader go)
  (def-ts-lang-loader html)
  (def-ts-lang-loader javascript)
  (def-ts-lang-loader jsdoc)
  (def-ts-lang-loader python)
  (def-ts-lang-loader regex)
  (def-ts-lang-loader typescript-tsx)
  (def-ts-lang-loader typescript-typescript)
  (def-ts-lang-loader yaml))

(defun list-ts-langs () (loop for name being each hash-key of *ts-langs* collect name))

(defvar *tree-sitter-language-directory* #P"/usr/local/share/tree-sitter/")

(defun tree-sitter-language-files ()
  (let ((res))
    (sb-ext:map-directory 
     (lambda (dir) (push (uiop:directory-files dir "*.json") res)) 
     *tree-sitter-language-directory*)
    (flatten res)))
