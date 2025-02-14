;;; lib/pod/tests.lisp --- Pod tests

;;

;;; Code:
(defpackage :pod/tests
  (:use :cl :rt :pod :sb-bsd-sockets)
  (:import-from :std :serde :serialize))

(in-package :pod/tests)

(defsuite :pod)
(in-suite :pod)

(deftest podman-api ()
  "Test the podman API over a local unix socket."
  (let ((local-socket (podman-local-user-socket)))
    (unless (probe-file local-socket)
      (start-podman-service local-socket)))
  (with-libpod-client (c)
    (is (string= "OK" (libpod-request c "_ping" :get)))
    (is (libpod-request-json c "info"))
    ;; (is (libpod-request-json c "events")) ;; hangs
    (is (libpod-request c "version"))
    (is (libpod-request-json c "containers/json"))))

(defvar *test-containerfile* "ARG FOO=bar
ARG BAR=foo
FROM ${FOO}/${BAR}
RUN sbcl
")

(deftest containerfile ()
  "Roundtrip CONTAINERFILE tests."
  (let ((cf (serde *test-containerfile* (make-instance 'containerfile))))
    (is (= 2 (length (containerfile-args cf))))
    (is (string= "${FOO}/${BAR}" (containerfile-base cf)))
    (is (= 1 (length (containerfile-steps cf))))
    (is (string= *test-containerfile* (serialize cf :string)))))
