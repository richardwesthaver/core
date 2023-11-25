;;; web/dash.lisp --- local user dashboard

;;; Code:
(uiop:define-package :app/web/dash
    (:use :cl :std :hunchentoot :lass :spinneret)
  (:export 
   :main
   :serve-static-assets
   :*web-dash-port*))

(in-package :app/web/dash)

(defparameter *web-dash-port* 8800)
(defparameter *web-dash-static-directory* #P"/tmp/web/dash/static/")

(defun serve-static-assets ()
  "Serve static assets under the /src/static/ directory when called with the /static/ URL root."
  (push (create-folder-dispatcher-and-handler
         "/static/" (merge-pathnames *web-dash-static-directory*
                                     (asdf:system-source-directory :app) ;; => NOT src/
                                     ))
        *dispatch-table*))

(defvar *server* (serve-static-assets))

(defun main (&key  (output *standard-output*))
  (let ((*standard-output* output))
    (print "starting dash server on ~A" *web-dash-port*)
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
            (hunchentoot:stop *server*)
            (uiop:quit)))
      (error (c) (format t "Woops, an unknown error occured:~&~a~&" c)))))
