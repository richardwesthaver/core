;;; gui.lisp --- EDI GUI

;; 

;;; Code:
(defpackage :edi/gui
  (:use :cl :std :edi :gui)
  (:import-from :cli/clap :defmain)
  (:export :edi-gui))

(in-package :edi/gui)
(defmain edi-gui ())
