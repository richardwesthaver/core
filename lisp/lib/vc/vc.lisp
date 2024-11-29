;;; vc/vc.lisp --- VC API

;; High-level API for working with VC objects.

;;; Code:
(pkg:defpkg :vc
  (:use :cl :std)
  (:use-reexport :vc/proto :vc/hg :vc/git #+cli :vc/cli :vc/util))  

(in-package :vc)
#+cli
(cli:load-package-cli *vc-cli*)
