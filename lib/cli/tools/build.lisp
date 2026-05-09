;;; build.lisp --- Build tools

;; 

;;; Code:
(in-package :cli/tools/build)

(define-cli-tool :make (args &key (wait t) (output t) (input))
  (let ((proc (sb-ext:run-program *make* args :wait wait :output output :input input)))
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

;;; Rust
(define-cli-tool :cargo (&rest args)
  (let ((proc (sb-ext:run-program *cargo* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (cargo-error "CARGO command failed: ~A ~A" *cargo* (or args "")))))

(define-cli-tool :rustup (&rest args)
  (let ((proc (sb-ext:run-program *rustup* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (cargo-error "RUSTUP command failed: ~A ~A" *rustup* (or args "")))))

(defun cargo-install (crate &key force git path)
  (let ((args `(,@(etypecase crate
                    (string `(,crate))
                    (symbol `(,(string-downcase crate)))
                    (list crate))
                ,@(when force '("--force"))
                ,@(when git `("--git" ,git))
                ,@(when path `("--path" ,path)))))
    (apply 'run-cargo "install" args)))

(defun cargo-clean (&rest args)
  (apply 'run-cargo "clean" args))

;;; Python
(define-cli-tool :uv (&rest args)
  (let ((proc (sb-ext:run-program *uv* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (uv-error "UV command failed: ~A ~A" *uv* (or args "")))))
