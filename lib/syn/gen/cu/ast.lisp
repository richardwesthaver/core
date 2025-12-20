;;; ast.lisp --- CUDA AST Nodes

;; 

;;; Code:
(in-package :syn/gen/cu)
(defstmt cuda-funcall () (name blocks threads shared))
(defnode cuda-alignment () (size name))

(defmacro cu-syntax (tags lambda-list &body body)
  `(defsyntax ,tags (:cu) ,lambda-list ,@body))

(cu-syntax launch (kernel (&key blocks threads shared) &rest args)
  "Launch cuda kernels"
  `(function-call
    ;; put block threads an shard in name slot
    (cuda-funcall
     ;; kernel name
     (make-node ,kernel)
     ;; grid config
     (make-node ,blocks)
     ;; block config
     (make-node ,threads)
     ;; shared mem
     ,(when shared
        `(make-node ,shared)))
    ;; function agrs
    (make-nodes ,args)))

(cu-syntax struct (name alignment &body body)
  "Struct with alignment"
  `(struct-definition
    ;; put name an alignment in name slot
    (cuda-alignment
     (make-node ,alignment)
     (make-node ,name))
    ;; struct body
    (compound-statement
     ;; curly braces: t
     t
     ;; build subnodes
     (make-nodes ,body))))
