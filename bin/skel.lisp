;;; Code:

;;  TODO 2024-05-09: add shell configurables to rules - maybe at sk-command
;;  level. :INPUT :WAIT :OUTPUT
(in-package :std-user)
(defpkg :bin/skel
  (:use :cl :std :cli :cli/clap/obj
   :vc :sb-ext :skel :log :cli/clap/util
   :obj/ast #+clim :skel/tools/view
   :db :rdb :schema :config :build :packy :krypt :skel/cli)
  (:import-from :cli/shell :*shell-input* :*shell-directory*)
  (:use :cli/tools/sbcl :cli/prompt))

(in-package :bin/skel)
(in-readtable :shell)

;;; Nested Commands
(defcmd skc-vc* ()
  (with-cli (*vc-cli* :args (cdr (cli:args)))
    (with-current-vc-root (*repo* *default-pathname-defaults*)
      (do-opts *vc-cli*)
      (do-cmd *vc-cli*))))

(defcmd skc-pk* ()
  (with-cli (*packy-cli* :args (cdr (cli:args)))
    (do-opts *packy-cli*)
    (do-cmd *packy-cli*)))

(defcmd skc-kr* ()
  (with-cli (*krypt-cli* :args (cdr (cli:args)))
    (do-opts *krypt-cli*)
    (blake3::load-blake3)
    (do-cmd *krypt-cli*)))

(load-package-cli 
 :skel
 :opts ((:name "interactive" :description "enter the lisp image after running commands"))
 :cmds
 ((:name vc
   :description "version control"
   :thunk skc-vc*)
  (:name pk
   :description "packages"
   :thunk skc-pk*)
  (:name kr
   :description "cryptography"
   :thunk skc-kr*)))

(defmain start-skel ()
  (in-package :sk-user)
  (in-readtable :shell)
  (let ((sb-debug:*backtrace-frame-count* 8))
    (with-cli ((package-cli :bin/skel) :args (cli:args))
      (do-opts *cli*)
      ;; (rocksdb:load-rocksdb)
      (init-skel)
      (unwind-protect 
           (progn
             ;; (setq *db* (make-db :skel))
             (do-cmd *cli*)
             (when (getopt "interactive" nil)
               (sk-shell)))))))
        
