;;; log.lisp --- logging facades for lisp

;; this package contains logging facilities for lisp applications and libraries.

;;; Commentary:


;;; Simple Logging

;; The simple logging interface works as follows:

;; Use *LOG-LEVEL* to set the current level of logging. Value is
;; either a boolean or one of the following keywords: :warn :info
;; :debug :trace.

;; top-level macros: info! trace! warn! debug!

;; inspired by rust-lang/log https://crates.io/crates/log

;; the following shell environment variables may be queried by this
;; package:

;; - LOG_LEVEL : corresponds to a value for *LOG-LEVEL*. value may be
;; - empty or one of the following string values: WARN INFO DEBUG TRACE

;;; Advanced Logging

;; The advanced logging interface is based on Shinmera's VERBOSE which
;; implements basically all of the functionality we would expect in a logging
;; framework.

;; VERBOSE is built on top of another library from Shinmera's collection
;; called PIPING which provides a message-passing CLOS API. We have taken the
;; liberty to port over most of this functionality into a STD/PIPE package and
;; use it to build a logging framework using the same methodology.

;; In our case, the LOGGER class inherits from the STD/PIPE:PIPE class which
;; encapsulates one slot consisting of an array (the pipeline or PIPE) and
;; another slot providing an INDEX of cached lookup values into the array.

;; The LOGGER object is multi-threaded by default and handles marshalling of
;; MESSAGE objects through the ELEMENTs of the PIPE. A MESSAGE is handled by
;; each ELEMENT in the PIPE via the MSG method.

;; There are various ELEMENT implementations provided including level and tag
;; filters, condition handlers, as well as SINK elements which are responsible
;; for printing the final output of a MESSAGE to a stream or file.

;;; Code:
(defpackage :log
  (:use :cl :std :std/meta :std/thread :time :db)
  (:export :*log-level* :*logger* :log-router
   :make-log-router :log-router-p
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
   :matching-tree-tag
   :restart-logger
   :remove-logger
   :default-logger
   :add-pipe
   :with-logger
   :log-rotate
   :database-logger
   :db-sink
   :alien-logger
   :alien-source
   :alien-sink
   :ilevel
   :octets-to-log-message
   :log-message-to-octets
   :*log-show-backtrace*))
