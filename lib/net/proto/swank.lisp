;;; net/proto/swank.lisp --- Swank Protocol Support

;; The undocumented wire protocol of SLIME fame and fortune.

;;; Commentary:

;; ref: https://github.com/astine/swank-client/blob/master/swank-description.markdown

;;; Code:
(std:defpkg :net/proto/swank
  (:use :cl :sb-bsd-sockets :std :net/core :net/tcp)
  (:use-reexport :swank-client)
  (:export))

(in-package :net/proto/swank)

;;; Vars
(defvar *swank-connections* nil)

(defvar *swank-connections-lock* (make-mutex :name "swank-connections"))

;;; Remote Execution (RDP)

;;;; Messages

;; (:emacs-rex form package thread cont)

;; (:return return-expression cont)

;; :write-string

;; :new-package

;; :debug

;; :debug-activate

;; :indentation-update
