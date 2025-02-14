;;; stress.lisp --- Perform stress testing on DB instances

;; 

;;; Code:
(in-package :std-user)
(defpkg :core/bench/db/stress
  (:nicknames :bench/db/stress :db/stress)
  (:use :cl :std :rt :rt/bench :rt/cover :log :query :schema :config :rdb :db)
  (:export :tpc-h-schema :*tpc-h-data-directory*
           :start-tpc-h-benchmark))

(in-package :core/bench/db/stress)
(defsuite :db-stress)
(in-suite :db-stress)

(defconfig db-stress-config () 
  ())

(deftest db-stress (:bench t :profile t))
