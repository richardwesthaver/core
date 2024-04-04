;;; gui/client.lisp --- GUI Client API

;; This package provides a high-level client protocol for
;; communication with a running GUI.

;; It should be used to provide user-scripting/remote controller APIs
;; similar to Selenium.

;;; Code:
(in-package :gui/core)

(defgeneric gui-client-p (obj)
  (:method ((obj t)) nil))

