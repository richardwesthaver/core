;;; packy.lisp --- Packy API

;; 

;;; Code:
(in-package :skel/packy)

#+cli
(clap:load-package-cli skel/packy/cli:*packy-cli*)
