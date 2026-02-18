;;; io.lisp --- IO API

;; 

;;; Code:
(pkg:defpkg :io
  (:use :cl)
  (:use-reexport :io/proto :io/uring :io/flate 
   :io/zstd :io/stream :io/socket :io/chunky 
   :io/smart-buffer :io/static :io/xsubseq))

(defpackage :io-user
  (:use :cl :std :std-user :io))
