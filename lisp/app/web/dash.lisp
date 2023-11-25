;;; web/dash.lisp --- local user dashboard

;;; Code:
(uiop:define-package :app/web/dash
    (:use :cl :std :lack :lass :spinneret)
  (:import-from :clack :clackup)
  (:export 
   :main
   :serve-static-assets
   :*web-dash-port*))

(in-package :app/web/dash)

(defparameter *web-dash-port* 8800)
(defparameter *web-dash-static-directory* #P"/tmp/web/dash/static/")

(defvar *server*)

(defun main (&key  (output *standard-output*) (port *web-dash-port*))
  (let ((*standard-output* output))
    (print "starting dash server on ~A" port)
    (handler-case (bt:join-thread (find-if (lambda (th)
                                             (search "hunchentoot" (bt:thread-name th)))
                                           (bt:all-threads)))
      ;; Catch a user's C-c
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl  ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       () (progn
            (format *error-output* "Aborting.~&")
            (clack:stop *server*)
            (uiop:quit)))
      (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))
