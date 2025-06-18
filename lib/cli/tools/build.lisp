;;; build.lisp --- Build tools

;; 

;;; Code:
(in-package :cli/tools/build)

(define-cli-tool :make (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *make* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (make-error "MAKE command failed: ~A ~A" *make* (or args "")))))

(define-cli-tool :cmake (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *cmake* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (cmake-error "CMAKE command failed: ~A ~A" *cmake* (or args "")))))

(define-cli-tool :ninja (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *ninja* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (ninja-error "NINJA command failed: ~A ~A" *ninja* (or args "")))))

(define-cli-tool :meson (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *meson* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (meson-error "MESON command failed: ~A ~A" *meson* (or args "")))))
