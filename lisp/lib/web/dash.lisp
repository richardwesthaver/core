;;; web/dash.lisp --- local user dashboard

;;; Code:
(uiop:define-package :web/dash
  (:use :cl :std #+nil :lass #+nil :spinneret :cli/clap)
  ;; (:import-from :clack :clackup)
  (:export 
   :main
   :serve-static-assets
   :*web-dash-port*))

(in-package :web/dash)

(defparameter *web-dash-port* 8800)
(defparameter *web-dash-static-directory* #P"/tmp/web/dash/static/")

(defvar *server*)

(defun main (&key  (output *standard-output*) (port *web-dash-port*))
  (let ((*standard-output* output))
    (print "starting dash server on ~A" port)
    (handler-case (sb-thread:join-thread (find-if (lambda (th)
                                                    (search "hunchentoot" (sb-thread:thread-name th)))
                                                  (sb-thread:list-all-threads)))
      ;; Catch a user's C-c
      (#+sbcl sb-sys:interactive-interrupt
       () (progn
            (format *error-output* "Aborting.~&")
            ;; (clack:stop *server*)
            (uiop:quit)))
      (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))
