;;; log.lisp --- Bench DB-LOGGER instances

;; 

;;; Code:
(pkg:defpkg :core/bench/db/log
  (:nicknames :bench/db/log)
  (:use :cl :std :rt :rt/bench :rt/cover :log :query :schema :config :rdb :db)
  (:export :bench-log-db))

(in-package :core/bench/db/log)

(defsuite :db-log)
(in-suite :db-log)
