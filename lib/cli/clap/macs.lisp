;;; cli/clap/macs.lisp --- Clap Macros

;; 

;;; Code:
(in-package :cli/clap)

(defun schar0 (name)
  "Return the first char of symbol or string NAME."
  (schar (string name) 0))

(defmacro with-cli-handlers (&body body)
  "A wrapper which handles common cli errors that may occur during
evaluation of BODY."
  `(progn
     (if *no-debug*
         (sb-ext:disable-debugger)
         (sb-ext:enable-debugger))
     (unwind-protect
          (restart-case 
              (handler-case (progn ,@body)
                (sb-sys:interactive-interrupt (c)
                  (if *no-debug*
                      (sb-ext:exit :code 130)
                      c))
                (error (c)
                  (println c)
                  (sb-ext:exit :code 1)))
            (abort ()
              :report (lambda (s)
                        (write-string
                         "Skip to toplevel READ/EVAL/PRINT loop."
                         s)
                        (log:debug! "CONTINUEing from pre-REPL RESTART-CASE")
                        (values)))
            (exit ()
              :report "Exit SBCL (calling #'EXIT, killing the process)."
              ;; :test (lambda (c) (declare (ignore c)) t)
              (log:debug! "falling through to EXIT from pre-REPL RESTART-CASE~&")
              (exit :code 1))))
     (sb-impl::flush-standard-output-streams)
     (unless *no-exit*
       (exit :code 0))
     ;; reset terminal state
     #+nil (.ris)))
