;;; homer.lisp --- homectl utility

;;; Code:
(defpackage :bin/homer
  (:use :cl :std :log :ast :sxp :rdb :skel :packy :cli :obj/id :krypt :vc)
  (:export :*home-config*))

(in-package :bin/homer)

(defun run ()
  (let ((*log-level* :info))
    (with-cli (*homer-cli* :args (cli:args))
      (init-homer-vars)
      (load-homerc)
      (do-cmd *cli*)
      (debug-opts *cli*))))

(defmain start-homer ()
  (let ((*print-readably* t))
    (run)
    (sb-ext:exit :code 0)))
