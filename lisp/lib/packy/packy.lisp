;;; packy.lisp --- Packy API

;; 

;;; Code:
(pkg:defpkg :packy
  (:nicknames :pk)
  (:use :cl :std)
  (:use-reexport :packy/client :packy/server :packy/core #+cli :packy/cli))

(pkg:defpkg :pk-user
  (:use :cl :std-user :log)
  (:use :packy :packy/core))

(in-package :packy)

#+cli
(cli:load-package-cli *packy-cli*)
