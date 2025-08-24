;;; net.lisp --- Net Top-level

;; 

;;; Code:
(in-package :std-user)

(eval-when (:load-toplevel)
  (pushnew :net *features*))

(defpkg :net
  (:use :cl :std)
  (:use-reexport . #.(remove "NET/REQ" net/int:*net-packages* :test 'string=))
  (:import-from :net/req :http-client-config :http-client)
  (:export :http-client-config :http-client))

(defpkg :net-user
  (:use :cl :std :net :uri :url))
