;;; skel.lisp --- Skel Daemon

;; The Skel Daemon CLI

;;; Code:

;;  TODO 2024-05-09: add shell configurables to rules - maybe at sk-command
;;  level. :INPUT :WAIT :OUTPUT
(in-package :std-user)
(defpkg :bin/skel
  (:use :cl :std :cli :clap
   :vc :sb-ext :skel :log
   :ast :db :rdb :schema :config :build :skel/packy :skel/krypt :skel/cli)
  (:import-from :cli/shell :*shell-input*)
  (:use :cli/tools/sbcl))

(in-package :bin/skel)

(defmain start-skel (:debug nil)
  (in-package :sk-user)
  (in-readtable :shell)
  (let ((sb-debug:*backtrace-frame-count* 8))
    (with-cli ((cli :skel))
      (init :skel)
      (funcall (kernel *cli*)))))
