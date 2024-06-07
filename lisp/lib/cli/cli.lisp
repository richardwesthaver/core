;;; cli.lisp --- CLI Top-level Packages

;; 

;;; Code:
(in-package :std-user)

(defpkg :cli
  (:use :cl :std)
  (:use-reexport :cli/shell :cli/ansi :cli/prompt
   :cli/progress :cli/spark :cli/prompt :cli/ed
   :cli/env :cli/repl :cli/clap))

(defpkg :cli-user (:use :cl :std :cli))
