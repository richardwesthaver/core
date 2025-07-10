;;; packy.lisp --- Packy API

;; 

;;; Code:
(pkg:defpkg :packy-user
  (:nicknames :pk-user)
  (:use :cl :std-user :log :packy))

(in-package :packy)

#+cli
(clap:load-package-cli packy/cli:*packy-cli*)
