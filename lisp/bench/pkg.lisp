;;; bench.lisp --- Core Benchmarks

;; 

;;; Code:
(in-package :std-user)

(defpkg :core/bench
  (:use :std-lisp :rt :log :rt/bench :rt/cover)
  (:export :*bench-directory* :bench-path))

(in-package :core/bench)

(unless (sb-impl::find-logical-host "CORE" nil)
  (setf (logical-pathname-translations "CORE")
        `(("tmp;*.*.*"   "/tmp/core/")
          ("tmp;bench;*.*.*"   "/tmp/core/bench/")
          ("tmp;test;*.*.*"   "/tmp/core/test/")
          ("src;*.*.*"   ,(merge-pathnames "comp/core/" (user-homedir-pathname)))
          ("misc;*.*.*"   ,(bench-path "misc/"))
          ("tmp;bench;result;*.*.*" "/tmp/core/bench/result/")
          ("tmp;data;*.*.*" "/tmp/core/data/")
          ("tmp;db;*.*.*" "/tmp/core/db/"))))

(setf (sb-ext:bytes-consed-between-gcs) 25000000)
