;;; c.lisp --- C Lang Tests

;; 

;;; Code:
(in-package :syn/tests/lang)

(in-suite :syn)
(load-tree-sitter-c)
(deftest c-src ()
  (istype '(alien (* ts-tree)) (parse-file :c #p"/usr/include/tree_sitter/api.h")))
