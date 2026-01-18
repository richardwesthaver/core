;;; mpk.lisp --- MPK Main

;; 

;;; Code:
(defpackage :bin/mpk
  (:use :cl :std :log :cli :clap :mpk :skel/homer/core)
  (:export
   #:start-mpk))
(in-package :bin/mpk)
(defmain start-mpk ()
  (mpk-ensure-directories)
  (load-mpkrc)
  (with-cli ((cli :mpk))
    (funcall (kernel *cli*))))

  
