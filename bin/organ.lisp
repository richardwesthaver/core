;;; organ.lisp --- Org-mode utility

;;

;;; Code:
(defpackage :bin/organ
  (:use :cl :organ :std :log :organ/cli)
  (:import-from :cli :with-cli :do-cmd :*cli* :debug-opts))
(in-package :bin/organ)
(defun run ()
  (let ((*log-level* :info))
    (with-cli (*organ-cli* :args (cli:args))
      (do-cmd *cli*)
      (debug-opts *cli*))))
(defmain start-organ ()
  (run))
