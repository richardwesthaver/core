;;; types.lisp --- Sys Types

;; 

;;; Code:
(in-package :sys)

;; convenience enums for errors and signals (grovelled by sb-posix/sys)
(std/alien:define-alien-enum (errno :type int))
(std/alien:define-alien-enum (signo :type int))
