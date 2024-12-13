;;; web/dash.lisp --- local user dashboard

;;; Code:
(uiop:define-package :web/dash
  (:use :cl :std #+nil :lass #+nil :spinneret :cli/clap :net/srv :net/srv/http)
  ;; (:import-from :clack :clackup)
  (:export 
   :main
   :serve-static-assets
   :*web-dash-port*))

(in-package :web/dash)

(defparameter *web-dash-port* 8800)
(defparameter *web-dash-static-directory* #P"/tmp/web/dash/static/")

;; self-signed PEM cert/key generated via 'skel make ssl-certs'
(defvar *server* (make-instance 'https-service 
                   :key-file (asdf:system-relative-pathname 
                              :core "../.stash/private-key.pem")
                   :cert-file (asdf:system-relative-pathname
                               :core "../.stash/private-key.pem")))

(defun main (&key  (output *standard-output*) (port *web-dash-port*))
  (let ((*standard-output* output))
    (print "starting dash server on ~A" port)
    (start *server*)
    (handler-case (sb-thread:join-thread (find-if (lambda (th)
                                                    (search "service" (sb-thread:thread-name th)))
                                                  (sb-thread:list-all-threads)))
      ;; Catch a user's C-c
      (sb-sys:interactive-interrupt () 
        (progn
          (format *error-output* "Aborting.~&")
          (stop *server*)
          (uiop:quit)))
      (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))
