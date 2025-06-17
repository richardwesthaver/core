;;; pod.lisp --- Container CLI

;; 

;;; Code:
(pkg:defpkg :bin/pod
  (:use :cl :std :cli :clap :log :pod))
(in-package :bin/pod)

(defmain start-pod ()
  (let ((sb-debug:*backtrace-frame-count* 8))
    (with-cli ((package-cli :pod) :args (args))
      (do-opts *cli*)
      (do-cmd *cli*))))
