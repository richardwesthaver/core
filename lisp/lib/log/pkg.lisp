;;; log.lisp --- logging facades for lisp

;; this package contains logging facilities for lisp applications and libraries.

;;; Commentary:

;; The simple (global) interface works as follows:

;; Use *LOG-LEVEL* to set the current level of logging. Value is
;; either a boolean or one of the following keywords: :warn :info
;; :debug :trace.

;; top-level macros: info! trace! warn! debug!

;; inspired by rust-lang/log https://crates.io/crates/log

;; the following shell environment variables may be queried by this
;; package:

;; - LOG_LEVEL : corresponds to a value for *LOG-LEVEL*. value may be
;; - empty or one of the following string values: WARN INFO DEBUG TRACE

;; The app logging interface is based on Shinmera's VERBOSE which implements
;; basically all of the functionality we would expect in a logging framework.

;;; Code:
(defpackage :log
  (:use :cl :std)
  (:export :*log-level* :*log-router* :*logger*
   :*default-log-router* :log-router :make-log-router :log-router-p
   :get-real-time-since :init-log-timestamp
   :*log-timestamp* :log-level-designator :log-timestamp-source :logger
   :logger-p :make-logger :log-error
   :define-log-level :log! :warn! :info! :debug! :trace! :fatal! :error!
   :log-p :warn-p :info-p :debug-p :trace-p :error-p :fatal-p
   :log-describe :warn-describe :info-describe :debug-describe :trace-describe :fatal-describe :error-describe
   :with-log-stream
   :with-fast-log-stream
   :*log-timestamp-format*
   :*log-indent*
   :log-message
   :level
   :simple-log-message
   :message-thread
   :*log-levels*
   :%log-object
   :log-object
   :rotating-file-sink
   :level-filter
   :tag-filter
   :tag-tree-filter
   :*tag-separator*
   :matching-tree-tag))
