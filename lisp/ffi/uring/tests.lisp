(defpackage :uring/tests 
  (:use :cl :rt :std :uring :sb-alien)
  (:import-from :obj/build :build))
(in-package :uring/tests)
(defsuite :uring)
(in-suite :uring)
(load-uring)

(deftest sanity ()
  (is (= 1 (io-uring-check-version (io-uring-major-version) (io-uring-minor-version)))))

(deftest nop-sqe ()
  (let ((op (build (uring::make-io-op-nop))))
    (is (typep op '(alien uring::io-uring-sqe)))
    (is (= (slot op 'uring::fd) -1))
    (is (= (slot op 'uring::user-data) 0))))

(deftest simple-vectored ()
  (let ((rop (build (uring::make-io-op-readv)))
        (wop (build (uring::make-io-op-writev))))
    (is (typep rop '(alien uring::io-uring-sqe)))
    (is (typep wop '(alien uring::io-uring-sqe)))))

(deftest init1 ()
  (with-new-io-uring r1
    (io-uring-queue-init 8 (addr r1) 0)
    (is (typep r1 '(alien io-uring)))
    (log::debug! (slot (slot r1 'uring::sq) 'uring::ring-entries))
    (is (= 0 (io-uring-queue-exit (addr r1))))
    (with-io-uring (r2 (addr r1))
      (io-uring-queue-init-params 16 r2 (uring::allocate-io-uring-params))
      (is (typep r2 '(alien io-uring)))
      (is (= 0 (io-uring-queue-exit r2))))))

(deftest submit ()
  (with-new-io-uring r1
    (io-uring-queue-init 16 (addr r1) 0)
    (is (typep (io-uring-get-sqe r1) '(alien io-uring-sqe*)))
    (is (= 0 (io-uring-submit (addr r1))))
    (is (= 0 (io-uring-queue-exit (addr r1))))))

(deftest register ())
