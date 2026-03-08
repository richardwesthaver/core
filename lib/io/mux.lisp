;;; mux.lisp --- Multiplexer

;; Based on IOLib (iomux)

;;; Code:
(in-package :io/mux)

(defun set-io-handler (base fd &rest args))
(defun event-dispatch (base &rest args))
