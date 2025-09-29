;;; web/dash.lisp --- local user dashboard

;;; Code:
(defpkg :web/dash
  (:use :cl :std :cli/clap :net/srv :net/srv/http)
  (:export :*dash-port* :*dash-directory* :*dash-server* :dash))

(in-package :web/dash)

(defparameter *dash-port* 8800)
(defparameter *dash-directory* #P"/tmp/web/dash/static/")

;; self-signed PEM cert/key generated via 'skel make ssl-certs'
;; (defvar *server* (make-instance 'https-service 
;;                    :key-file (asdf:system-relative-pathname 
;;                               :core "../.stash/private-key.pem")
;;                    :cert-file (asdf:system-relative-pathname
;;                                :core "../.stash/private-key.pem")))

(defvar *dash-server* (make-instance 'https-service :port *dash-port* :path *dash-directory* :name :dash))

(defun dash (&key (output *standard-output*) (port *dash-port*))
  (let ((*standard-output* output))
    (log:info! "starting dash server on ~A" port)
    (start *dash-server*)
    (handler-case (sb-thread:join-thread (find-if (lambda (th)
                                                    (search "service" (sb-thread:thread-name th)))
                                                  (sb-thread:list-all-threads)))
      ;; Catch a user's C-c
      (sb-sys:interactive-interrupt ()
        (progn
          (format *error-output* "Aborting.~&")
          (stop *dash-server*)
          (uiop:quit)))
      (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))
