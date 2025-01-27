;;; pod.lisp --- Container CLI

;; 

;;; Code:
(pkg:defpkg :bin/pod
  (:use :cl :std :cli :log :pod))
(in-package :bin/pod)

(defmain start-pod ()
  (let ((sb-debug:*backtrace-frame-count* 8))
    (with-cli ((package-cli :pod) :args (cli:args))
      (do-opts *cli*)
      (do-cmd *cli*))))
