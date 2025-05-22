;;; stress.lisp --- Perform stress testing on DB instances

;; 

;;; Code:
(pkg:defpkg :core/bench/db/stress
  (:nicknames :bench/db/stress :db/stress)
  (:use :cl :std :rt :rt/bench :rt/cover :log :schema :config :rdb :db)
  (:export :stressed-out-db-config :bench-db-stress))
(in-package :core/bench/db/stress)
(defsuite :db-stress)
(in-suite :db-stress)

(defconfig stressed-out-db-config () 
  ())

(deftest db-stress (:bench t :profile t))
