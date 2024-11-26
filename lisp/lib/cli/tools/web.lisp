;;; web.lisp --- Web Browsers

;; 

;;; Code:
(in-package :cli/tools/web)

(deferror simple-browser-error (simple-error) () (:auto t))

(defparameter *browser* (or (find-exe "chromium") (find-exe "firefox")))

(defun run-browser (&rest args)
  (let ((proc (sb-ext:run-program *browser* (or args nil) :output :stream)))
    (with-open-stream (s (sb-ext:process-output proc))
      (loop for l = (read-line s nil nil)
            while l
            do (write-line l)))
    (if (eq 0 (sb-ext:process-exit-code proc))
        nil
        (simple-browser-error "browser command failed: ~A ~A" args))))

(defun browse-url (url)
  (run-browser (render-uri url)))
