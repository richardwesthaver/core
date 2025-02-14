;;; wasmer/types.lisp --- Wasmer FFI Types

;;

;;; Code:
(in-package :wasmer)

;; WASMER_COMPILER_ENABLED
;; WASMER_WASI_ENABLED
;; wasmer-name-extern
;; (define-alien-type wasi-env
;;   (struct wasi-env-t))
;; wasi-filesystem

(define-opaque wasm-message)
(define-opaque wasm-name wasm-message)
(define-opaque wasm-byte-vec wasm-name)
(define-opaque wasm-extern-vec wasm-name)
(define-alien-type wasm-module (struct wasm-module-t))
(define-alien-type wasm-store (struct wasm-store-t))
(define-alien-type wasm-config (struct wasm-config-t))
(define-alien-type wasm-engine (struct wasm-engine-t))
(define-alien-type wasm-trap (struct wasm-trap-t))
(define-alien-type wasm-instance (struct wasm-instance-t))

(define-alien-type wasi-config
  (struct wasi-config-t))

(define-alien-type wasmer-cpu-features
  (struct wasmer-cpu-features-t))

(define-alien-type wasmer-features
  (struct wasmer-features-t))

(define-alien-type wasmer-metering
  (struct wasmer-metering-t))

(define-alien-type wasmer-middleware
  (struct wasmer-middleware-t))

(define-alien-type wasmer-middleware
  (struct wasmer-middleware-t))

(define-alien-type wasmer-target
  (struct wasmer-target-t))

(define-alien-type wasmer-triple
  (struct wasmer-triple-t))

(define-alien-type functioncenv
  (struct functioncenv
          (inner (* t))))

(define-alien-type wasmer-funcenv
  (struct wasmer-funcenv-t
          (inner functioncenv)))

(define-alien-enum (wasmer-compiler int)
                   :cranelift 0
                   :llvm 1
                   :singlepass 2)

(define-alien-enum (wasmer-engine int)
                   :universal 0)
