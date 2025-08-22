;;; homer.lisp --- homectl utility

;;; Code:
(defpackage :bin/homer
  (:use :cl :std :log :ast :sxp :rdb :skel :skel/packy :cli :clap :id :skel/krypt :vc :skel/homer :skel/homer/cli))

(in-package :bin/homer)

(load-package-cli :skel/homer)

(defun run ()
  (in-package :skel/homer)
  (homer-user-init)
  (load-homerc)
  (with-cli (*homer-cli* :args (args))
    (do-cmd *cli*)
    (debug-opts *cli*)))

(defmain start-homer ()
  (in-readtable :shell)
  (let ((*print-readably* t))
    (run)
    (sb-ext:exit :code 0)))
