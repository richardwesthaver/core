;;; uring/tests.lisp --- URING Alien Tests
(defpackage :uring/tests 
  (:use :cl :rt :std :uring :sb-alien)
  (:import-from :obj/build :build :build-from))

(in-package :uring/tests)

(defsuite :uring)
(in-suite :uring)

(load-uring)

(deftest sanity ()
  ;; why is this 0 instead of 1..
  (isnt (uring::io-uring-check-version (uring::io-uring-major-version) (uring::io-uring-minor-version))))

(deftest params ())

(deftest setup ()
  ;; (io-uring-setup
  )

(deftest nop-sqe ()
  (with-new-io-sqe sqe
    (let ((op (uring::build-from (uring::make-io-op-nop) (alien-sap sqe))))
      (is (typep op '(alien uring::io-uring-sqe)))
      (is (= (slot op 'uring::fd) -1)))))

(deftest simple-vectored ()
  (with-new-io-sqe rop
    (with-new-io-sqe wop
      (let ((rop (uring::build-from (uring::make-io-op-readv) (alien-sap rop)))
            (wop (uring::build-from (uring::make-io-op-writev) (alien-sap wop))))
        (istype '(alien uring::io-uring-sqe) rop)
        (istype '(alien uring::io-uring-sqe) wop)))))

(deftest init1 ()
  (with-new-io-uring r1
    (uring::io-uring-queue-init 8 (alien-sap r1) 0)
    (is (typep r1 '(alien io-uring)))
    (log::trace! (slot (slot r1 'uring::sq) 'uring::ring-entries))
    (with-io-uring (r2 (addr r1))
      (uring::io-uring-queue-init-params 16 (alien-sap r2) (uring::allocate-io-uring-params))
      (is (typep r2 '(alien (* io-uring))))
      (is (= 0 (uring::io-uring-queue-exit (alien-sap r2)))))))

(deftest submit ()
  (with-new-io-uring r1
    (uring::io-uring-queue-init 16 (alien-sap r1) 0)
    (let ((sqe (io-uring-get-sqe r1)))
      (is (typep sqe '(alien (* io-uring-sqe))))
      (is (= 0 (uring::io-uring-submit (alien-sap r1))))
      (uring::build-from (make-io-op-nop) (alien-sap (deref sqe)))
      (is (= 0 (uring::io-uring-queue-exit (alien-sap r1)))))))

(deftest register ()
  (is (io-restriction-p (make-io-restriction))))
