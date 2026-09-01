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
  (is (go= (ones 2) (ones 2)))
  (isnt (ref (tensor::ge= (ones 2) (zeros 2)) 1))
  (iseql 'hash-table (store-type (tensor 'double-float 'hash-tensor)))
  (is= 2 (array-rank (indices (zeros '(2 2) (tensor 'double-float 'coordinate-tensor) 4))))
  (is= 2 (length (store (zeros '(20 20) (tensor 'double-float 'coordinate-tensor) 2)))))

(define-tensor-generic copy!-test (x y))
(define-tensor-method copy!-test ((x dense-tensor :a) (y dense-tensor :b t))
  `(tensor::t.copy! (,(cl :a) ,(cl :b)) x y))

(deftest tensor-method ()
  ;; FIX 2025-12-31: 
  (tensor::print-tensor (zeros 10) t)
  (is= 10 (store-size (copy!-test (zeros 10) (zeros 10)))))

;; (t.strict-coerce (number (complex double-float)) 0) -> (COERCE X '(COMPLEX DOUBLE-FLOAT))
;; (t.strict-coerce (complex (complex double-float)) 0) -> (COERCE X '(COMPLEX DOUBLE-FLOAT))
;; (t.strict-coerce (real (complex double-float)) 0) -> (COERCE X '(COMPLEX DOUBLE-FLOAT))
;; (t.strict-coerce (real complex) 0) -> error: template not defined ; NOPE: X
;; (t.strict-coerce (fixnum double-float) 0) -> (COERCE X 'DOUBLE-FLOAT)
;; (t.strict-coerce (fixnum fixnum) 0) -> error: template not defined ; NOPE: X
;; (t.strict-coerce (fixnum real) 0) -> (COERCE X 'REAL)
;; (t.strict-coerce (double-float t) 0) -> X
