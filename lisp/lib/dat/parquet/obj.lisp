;;; obj.lisp --- Parquet Objects

;; Parquet class and type definitions generated from parquet.json.

;;; Code:
(in-package :dat/parquet)

(eval-always
  (dat/parquet/gen::load-parquet))

(deftype parquet-compression-codec () `(member ,*parquet-compression-codecs*))

(deftype parquet-boundary-order () `(member ,*parquet-boundary-orders*))

(deftype parquet-encoding () `(member ,*parquet-encodings*))

(deftype parquet-field-repetition () `(member ,*parquet-field-repetition-types*))

(deftype parquet-type-designator () `(member ,*parquet-types*))

(deftype parquet-converted-type-designator () `(member ,*parquet-converted-types*))

(deftype parquet-page-type () `(member ,*parquet-page-types*))
