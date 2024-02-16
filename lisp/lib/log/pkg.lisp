;;; log.lisp --- logging facade for lisp

;; this package contains a simple logging facade for lisp applications
;; and libraries.

;;; Commentary:

;; Use *LOG-LEVEL* to set the current level of logging. Value is
;; either a boolean or one of the following keywords: :warn :info
;; :debug :trace.

;; top-level macros: info! trace! warn! debug!

;; inspired by rust-lang/log https://crates.io/crates/log

;; I intend to keep things simple for a while and then work out a DSL
;; for configuring logging. The DSL will be embedded in skelfiles.

;; the following shell environment variables may be queried by this
;; package:

;; - LOG_LEVEL : corresponds to a value for *LOG-LEVEL*. value may be
;; - empty or one of the following string values: WARN INFO DEBUG TRACE

;;; Code:
(defpackage :log
  (:use :cl :std)
  (:export :*log-level* :log-level-designator :log-timestamp-source 
   :log! :warn! :info! :debug! :trace!  :dbg!
   :debug-p))
