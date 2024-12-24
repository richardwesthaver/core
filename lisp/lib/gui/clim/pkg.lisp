;;; pkg.lisp --- CLIM Packages

;; 

;;; Code:
#+dbg
(pkg:defpkg :gui/clim/dbg
  (:use :cl :std :gui/clim)
  (:use-reexport :clim-debugger))
