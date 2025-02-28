;;; tests.lisp --- CUDA Tests

;; Requires an NVIDIA GPU

;;; Code:
(defpackage :cuda/tests
  (:use :cl :log :std :rt :cuda))
(in-package :cuda/tests)
(defsuite :cuda)
(in-suite :cuda)
(load-cuda)
(deftest init ()
  ;; error returned due to loading stub
  (is= 0 (cuda::cu-init 0)))


