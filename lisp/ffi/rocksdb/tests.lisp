;;; rocksdb/tests.lisp --- RocksDB tests

;;; Code:
(defpackage :rocksdb/tests
  (:use :cl :std :std/rt :std/fu :rocksdb :std/alien :sb-ext))

(in-package :rocksdb/tests)
(defsuite :rocksdb)
(in-suite :rocksdb)
(load-rocksdb)
(eval-always
  (defun rocksdb-test-dir ()
    (format nil "/tmp/~A/" (gensym "rocksdb-tests-")))

  (defun test-opts () (let ((default (rocksdb-options-create)))
			(rocksdb-options-set-create-if-missing default t)
			default)))

;; not thread safe (gensym-counter)
(defun genkey (&optional prefix) (string-to-octets (symbol-name (gensym (or prefix "key")))))
(defun genval (&optional prefix) (string-to-octets (symbol-name (gensym (or prefix "val")))))

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
    (let* ((key (genkey))
           (val (genval))
	   (klen (length key))
	   (vlen (length val))
           (wopts (rocksdb-writeoptions-create))
           (ropts (rocksdb-readoptions-create)))
      (with-alien ((k (* char) (make-alien char klen))
                   (v (* char) (make-alien char vlen))
                   (errptr rocksdb-errptr nil))
	(loop for x across key
	      for i from 0 below klen
	      do (setf (deref k i) x))
	(loop for y across val
	      for i from 0 below vlen
	      do (setf (deref v i) y))
        (rocksdb-put db 
                     wopts
                     k
                     klen
                     v
                     vlen
                     errptr)
	(is (null-alien errptr))
        (rocksdb-get db ropts k klen (make-alien size-t vlen) errptr)
	(is (null-alien errptr))
	(let ((rval (make-array vlen :element-type 'unsigned-byte)))
	  (loop for i from 0 below vlen do (let ((x (deref v i))) (setf (aref rval i) x)))
	  (is (string= (octets-to-string val) (concatenate 'string (map 'vector #'code-char rval)))))
        (rocksdb-delete db wopts k klen errptr)
	(is (null-alien errptr))
        (rocksdb-writeoptions-destroy wopts)
        (rocksdb-readoptions-destroy ropts)))
    (rocksdb-close db)
    (rocksdb-options-destroy opts)
    (sb-ext:delete-directory path :recursive t)))
