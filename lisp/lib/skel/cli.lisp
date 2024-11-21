;;; cli.lisp --- Skel CLI Definitions

;; CLI implementation of Skel

;;; Code:
(pkg:defpkg :skel/cli
  (:nicknames :sk-cli)
  (:use :cl :std :skel :sb-ext :cli/clap/obj))
(in-package :skel/cli)

(define-cli *skel-cli*)
(set-package-cli *skel-cli*)
