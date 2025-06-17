;;; homer.lisp --- homectl utility

;;; Code:
(defpackage :bin/homer
  (:use :cl :std :log :ast :sxp :rdb :skel :packy :cli :clap :id :krypt :vc :homer :homer/cli))

(in-package :bin/homer)

(load-package-cli :homer)
                  
(defun run ()
  (in-package :homer)
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
