;;; rocksdb/tests.lisp --- RocksDB tests

;;; Code:
(defpackage :rocksdb.tests
  (:use :cl :rt :rocksdb :rdb :sb-alien :alien :sb-ext)
  (:export :rocksdb-test-dir))

(in-package :rocksdb.tests)

(defun rocksdb-test-dir ()
  (format nil "/tmp/~A/" (gensym "rocksdb-tests-")))

(defun test-opts () 
  (let ((opts (rocksdb-options-create)))
    (rocksdb-options-set-create-if-missing opts t)
    opts))

(defsuite :rocksdb)

(in-suite :rocksdb)

(deftest set-opts ()
  (let ((opts (rocksdb-options-create))
        (wopts (rocksdb-writeoptions-create))
        (ropts (rocksdb-readoptions-create))
        (bopts (rocksdb-block-based-options-create)))
    (rocksdb-options-set-create-if-missing opts t)
    (rocksdb-options-destroy opts)
    (rocksdb-writeoptions-destroy wopts)
    (rocksdb-readoptions-destroy ropts)
    (rocksdb-block-based-options-destroy bopts)))

(deftest db ()
  (let* ((opts (test-opts))
         (path (rocksdb-test-dir))
         (db (rocksdb-open opts path nil)))
    (let ((k "key")
          (v "val")
          (wopts (rocksdb-writeoptions-create))
          (ropts (rocksdb-readoptions-create)))
      (with-alien ((key (* char) (make-alien-string k))
                   (val (* char) (make-alien-string v :external-format :ascii :null-terminate t))
                   (errptr rocksdb-errptr nil)
                   (vlen (* size-t) (make-alien size-t 1))
                   (ar (array char)))
        (rocksdb-put db 
                     wopts
                     key
                     (length k) 
                     val
                     (length v)
                     errptr)
        (is (string= v (rocksdb-get db ropts key (length k) vlen errptr)))

        (rocksdb-delete db wopts key (length k) errptr)
        (rocksdb-writeoptions-destroy wopts)
        (rocksdb-readoptions-destroy ropts)))
    (rocksdb-close db)
    (rocksdb-options-destroy opts)
    (sb-ext:delete-directory path :recursive t)))
