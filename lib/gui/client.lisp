;;; gui/client.lisp --- GUI Client API

;; This package provides a high-level client protocol for
;; communication with a running GUI.

;;; Code:
(in-package :gui/core)

(defgeneric gui-client-p (obj)
  (:method ((obj t)) nil))
