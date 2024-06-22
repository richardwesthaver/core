;;; lib/pod/tests.lisp --- Pod tests

;;

;;; Code:
(defpackage :pod/tests
  (:use :cl :rt :pod :sb-bsd-sockets)
  (:import-from :dat/proto :serde))

(in-package :pod/tests)

(defsuite :pod)
(in-suite :pod)

(deftest podman-api ()
  "Test the podman API over a local unix socket."
  (unless (probe-file *podman-local-user-socket*)
    (start-podman-service *podman-local-user-socket*))
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
    (is (string= *test-containerfile* (dat/proto:serialize cf :string)))))
