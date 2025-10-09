;;; sys.lisp --- Lisp System Loggers

;; 

;;; Code:
(in-package :log)

(defprovider :logger (name)
  `(or (find-symbol ,name) ,name))
