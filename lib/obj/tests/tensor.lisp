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
  (go= (ones 2) (ones 2))
  (isnt (ref (tensor::ge= (ones 2) (zeros 2)) 1))
  (iseql 'hash-table (store-type (tensor 'double-float 'hash-tensor)))
  ;; (indices (zeros '(2 2) (tensor 'double-float 'coordinate-tensor) 4))
  ;; FIX 2025-12-31: 
  #+nil(store (zeros '(2 2) (tensor 'double-float 'coordinate-tensor) 4))
  )

(define-tensor-generic copy!-test (x y))

(deftest tensor-method ()
  (define-tensor-method copy!-test ((x dense-tensor :a) (y dense-tensor :b t))
    `(tensor::t.copy! (,(cl :a) ,(cl :b)) x y))
  ;; FIX 2025-12-31: 
  #+nil(tensor::print-tensor (zeros 10) nil)
  (is= 10 (total-size (copy!-test (zeros 100) (zeros 10)))))
