;;; mpk.lisp --- MPK Main

;; 

;;; Code:
(defpackage :bin/mpk
  (:use :cl :std :log :cli :clap :mpk :skel/homer/core)
  (:export
   #:start-mpk))
(in-package :bin/mpk)
(in-readtable :core)
(load-package-cli :mpk)

(defmain start-mpk ()
  (mpk-ensure-directories)
  (load-mpkrc)
  (with-cli (mpk/cli:*mpk-cli* :args (args))
    (do-cmd *cli*)))

  
