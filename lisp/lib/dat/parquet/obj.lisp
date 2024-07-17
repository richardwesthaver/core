;;; obj.lisp --- Parquet Objects

;; Parquet class and type definitions generated from parquet.json.

;;; Code:
(in-package :dat/parquet)

(defclass parquet-object () ())

(defclass parquet-struct-object (parquet-object) ())

(eval-always
  (dat/parquet/gen::load-parquet))

(macrolet ((def-parquet-type (name)
             (let ((var-name (symbolicate "*" name "S*"))
                   (name1 (symbolicate name "*")))
               `(progn
                  (deftype ,name () `(member ,,var-name))
                  (defun ,name (d) (position d ,var-name :test 'eql))
                  (defun ,name1 (n) (elt ,var-name n))))))
  (def-parquet-type parquet-compression-codec)
  (def-parquet-type parquet-boundary-order)
  (def-parquet-type parquet-encoding)
  (def-parquet-type parquet-field-repetition-type)
  (def-parquet-type parquet-type)
  (def-parquet-type parquet-converted-type)
  (def-parquet-type parquet-page-type))

