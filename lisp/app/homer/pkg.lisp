;;; pkg.lisp --- HOMER Packages

;; 

;;; Code:
(defpackage :homer
  (:use :cl :std :log))

(defpackage :homer/cli
  (:use :cl :std :log :homer :cli))

(defpackage :homer/gui
  (:use :cl :std :log :homer :gui))
