;;; app.lisp --- CLIM Application Protocol

;; 

;;; Code:
(in-package :gui/clim)

(defun run-app (&optional (app *application*))
  (let ((frame (make-application-frame app)))
    (values 
     frame
     (clim-sys:make-process
      (lambda () (run-frame-top-level frame))))))
