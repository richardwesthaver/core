;;; rustls/tests.lisp --- librustls tests

;;; Code:
(defpackage :rustls/tests
  (:use :cl :std :rt :rustls))

(in-package :rustls/tests)

(defsuite :rustls)
(in-suite :rustls)

(load-rustls)

(deftest sanity ()
  (is (stringp (rustls::rustls-version))))

(deftest basic ()
  (let ((acceptor (rustls::rustls-acceptor-new))
        (scbuilder (rustls::rustls-server-config-builder-new))
        (sbuilder (rustls::rustls-root-cert-store-builder-new)))
    (sb-alien:with-alien ((sc (* rustls::rustls-root-cert-store))
                          (accepted (* rustls::rustls-accepted))
                          (accepted-alert (* rustls::rustls-accepted-alert)))
      (rustls::rustls-root-cert-store-builder-build sbuilder (sb-alien:addr sc))
      (rustls::rustls-acceptor-accept acceptor (sb-alien:addr accepted) (sb-alien:addr accepted-alert))
      (rustls::rustls-server-config-builder-free scbuilder)
      (rustls::rustls-root-cert-store-builder-free sbuilder)
      (rustls::rustls-acceptor-free acceptor)
      (rustls::rustls-client-config-builder-new))))
      
;; (rustls::rustls-error res buf len out-n)
