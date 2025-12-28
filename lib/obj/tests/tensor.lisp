;;; tensor.lisp --- Tensor Tests

;; 

;;; Code:
(in-package :obj/tests)

(deftest basic-tensors ()
  "Test basic tensor functionality."
  (is (car-eql 10 (tensor:range 10 20 1 t)))
  (is= 10 (length (linspace 1 100 10 t)))
  (isequalp '(simple-array real (*)) (tensor::store-type (tensor 'real)))
  (isequalp '(simple-bit-vector *) (tensor::store-type (tensor 'boolean)))
  (iseql 'hash-table (store-type (tensor 'double-float 'hash-tensor))))




