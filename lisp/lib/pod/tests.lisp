;;; lib/pod/tests.lisp --- Pod tests

;;

;;; Code:
(defpackage :pod/tests
  (:use :cl :rt :pod :sb-bsd-sockets))

(in-package :pod/tests)
;; (start-podman-service)

(defsuite :pod)
(in-suite :pod)

(defmacro with-libpod-client ((cvar &optional c) &body body)
  `(let ((,cvar ,(or c (make-instance 'libpod-client))))
     (socket-connect ,cvar)
     (unwind-protect (progn ,@body)
       (socket-close ,cvar))))

(deftest poke-and-prod ()
  (with-libpod-client (c)
    (is (string= "OK" (libpod-request c "_ping")))
    (is (libpod-request-json c "info"))
    ;; (is (libpod-request-json c "events")) ;; hangs
    (is (libpod-request c "version"))
    (is (libpod-request-json c "containers/json"))))
