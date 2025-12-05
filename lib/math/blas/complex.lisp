;;; complex.lisp --- Complex Tensor Support

;; 

;;; Code:
(in-package :obj/tensor)

(definline tensor-realpart~ (tensor)
  "
  Syntax
  ======
  (tensor-realpart~ tensor)

  Purpose
  =======
  Returns a new tensor object which points to  the real part of TENSOR.
  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its real part.
"
  (etypecase tensor
    (number (cl:realpart tensor))
    (dense-tensor (if (eql (realified-tensor (type-of tensor)) (type-of tensor)) tensor
                      (without-tensor-safety
                          (make-instance (realified-tensor (type-of tensor))
                                         :parent tensor :store (store tensor)
                                         :dimensions (dimensions tensor)
                                         :strides (map 'index-store-vector #'(lambda (x) (* 2 x)) (the index-store-vector (strides tensor)))
                                         :head (the index-type (* 2 (head tensor)))))))))

(definline tensor-imagpart~ (tensor)
  "
  Syntax
  ======
  (tensor-imagpart~ tensor)

  Purpose
  =======
  Returns a new tensor object which points to the imaginary part of the TENSOR, if
  it is complex valued, otherwise returns NIL.

  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its imaginary part.
"
  (etypecase tensor
    (number (cl:imagpart tensor))
    (dense-tensor (if (eql (realified-tensor (type-of tensor)) (type-of tensor)) nil
                      (without-tensor-safety
                          (make-instance (realified-tensor (type-of tensor))
                                         :parent tensor :store (store tensor)
                                         :dimensions (dimensions tensor)
                                         :strides (map 'index-store-vector #'(lambda (x) (* 2 x)) (the index-store-vector (strides tensor)))
                                         :head (1+ (the index-type (* 2 (head tensor))))))))))

(definline tensor-realpart (tensor)
  "
  Syntax
  ======
  (tensor-realpart tensor)

  Purpose
  =======
  Returns a new tensor object which points to  the real part of TENSOR.
  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its real part.
"
  (etypecase tensor
    (number (cl:realpart tensor))
    (dense-tensor (tensor-copy (tensor-realpart~ tensor)))))

(definline tensor-imagpart (tensor)
  "
  Syntax
  ======
  (tensor-imagpart tensor)

  Purpose
  =======
  Returns a new tensor object which points to  the real part of TENSOR.
  Store is shared with TENSOR.

  If TENSOR is a scalar, returns its real part.
"
  (etypecase tensor
    (number (cl:imagpart tensor))
    (dense-tensor (if-let ((ip (tensor-imagpart~ tensor)))
                    (tensor-copy ip)
                    (zeros (dimensions tensor) (tensor (field-type (type-of tensor))))))))
