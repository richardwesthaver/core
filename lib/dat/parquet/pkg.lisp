;;; pkg.lisp --- Apache Parquet Packages

;; Common Lisp Parquet Implementation

;;; Commentary:

#|
https://github.com/apache/parquet-format
https://github.com/apache/parquet-format/blob/master/src/main/thrift/parquet.thrift
https://github.com/apache/parquet-testing
https://github.com/apache/parquet-java
https://github.com/apache/arrow-rs
https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/36632.pdf
https://thrift.apache.org/docs/types
|#

#|
    4-byte magic number "PAR1"
    <Column 1 Chunk 1>
    <Column 2 Chunk 1>
    ...
    <Column N Chunk 1>
    <Column 1 Chunk 2>
    <Column 2 Chunk 2>
    ...
    <Column N Chunk 2>
    ...
    <Column 1 Chunk M>
    <Column 2 Chunk M>
    ...
    <Column N Chunk M>
    File Metadata
    4-byte length in bytes of file metadata (little endian)
    4-byte magic number "PAR1"
|#

;; In this file we're being as lazy as possible. To generate our base objects
;; we depend on the file parquet.thrift in the parquet-format repo. The core
;; skelfile includes a script to download it and convert it to parquet.json
;; (requires the thirft cli tool). We then decode it with DAT/JSON and
;; generate lisp classes, and types.

;; NOTE: there is actually a Common Lisp code generate for Thrift. It seems to
;; work but it requires an ASDF system named thrift which I couldn't find
;; anywhere. Granted I didn't look that hard, but I don't think it matters
;; because we ultimately don't want to depend on the Thrift CLI tool for
;; codegen.

;;; Code:
(in-package :dat/parquet)

(define-constant +parquet-magic-number+ "PAR1" :test 'equal)

(defconstant +default-parquet-page-size+ (* 8 1024)) ;; 8kb
(defconstant +default-parquet-row-group-size (expt 1024 3)) ;; 1gb

(defvar *parquet-creator* "dat/parquet version 0.1.0")
