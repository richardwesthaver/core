;;; organ.lisp --- Org-mode utility

;;

;;; Code:
(defpackage :bin/organ
  (:use :cl :organ :std :log :organ/cli :cli :clap))

(in-package :bin/organ)

(defun run ()
  (let ((*log-level* :info))
    (with-cli (*organ-cli* :args (args))
      (do-cmd *cli*)
      (debug-opts *cli*))))

(defmain start-organ ()
  (run))
