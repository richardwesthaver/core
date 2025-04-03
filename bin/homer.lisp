;;; homer.lisp --- homectl utility

;;; Code:
(defpackage :bin/homer
  (:use :cl :std :log :ast :sxp :rdb :skel :packy :cli :id :krypt :vc :homer :homer/cli))

(in-package :bin/homer)

(load-package-cli :homer)
                  
(defun run ()
  (in-package :homer)
  (homer-user-init)
  (load-homerc)
  (cli:with-cli (*homer-cli* :args (cli:args))
    (cli:do-cmd cli:*cli*)
    (cli:debug-opts cli:*cli*)))

(defmain start-homer ()
  (in-readtable :shell)
  (let ((*print-readably* t))
    (run)
    (sb-ext:exit :code 0)))
