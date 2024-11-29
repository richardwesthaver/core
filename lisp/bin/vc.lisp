;;; vc.lisp --- Generic VC Tool

;; 

;;; Code:
(in-package :std-user)
(defpkg :bin/vc
  (:use :cl :std :cli
   :vc :sb-ext :log :cli/clap/util
   :obj/ast #+tools :skel/tools/viz)
  (:use :cli/tools/sbcl :cli/prompt))

(in-package :bin/vc)

