;;; log.lisp --- logging facade for lisp

;; this package contains a simple logging facade for lisp applications
;; and libraries.

;;; Commentary:

;; Use `*log-level*' to set the current level of logging. Value is
;; either a bool or one of the following keywords: :warn :debug :info
;; :trace.

;; top-level macros: info! trace! warn! debug!

;; inspired by rust-lang/log https://crates.io/crates/log

;; I intend to keep things simple for a while and then work out a DSL
;; for configuring logging. The DSL will be embedded in skelfiles.

;;; Code:
(defpackage :log
  (:use :cl :std)
  (:export :*log-level* :log-level-designator :log-timestamp-source 
   :log! :warn! :info! :debug! :trace!  :dbg!
   :debug-p))
