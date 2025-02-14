;;; wasmer/wasmer.lisp --- Wasmer Alien Routines

;;

;;; Code:
(in-package :wasmer)


(define-alien-routine wasmer-version c-string)

(define-alien-routine wasm-config-new (* wasm-config))
(define-alien-routine wasm-engine-new (* wasm-engine))
(define-alien-routine wasm-store-new (* wasm-store) (engine (* wasm-engine)))
(define-alien-routine wasm-store-delete void (store (* wasm-store)))
(define-alien-routine wasm-engine-delete void (engine (* wasm-engine)))

(define-alien-routine wasm-instance-new (* wasm-instance)
  (store (* wasm-store))
  (module (* wasm-module))
  (imports (* wasm-extern-vec))
  (trap (* (* wasm-trap))))

(define-alien-routine wasm-config-canonicalize-nans void
  (config (* wasm-config))
  (enable boolean))

(define-alien-routine wasm-config-push-middleware void
  (config (* wasm-config))
  (middleware (* wasmer-middleware)))

(define-alien-routine wasm-config-set-engine void
  (config (* wasm-config))
  (engine int))

(define-alien-routine wasm-config-set-features void
  (config (* wasm-config))
  (features (* wasmer-features)))

(define-alien-routine wasm-config-set-target void
  (config (* wasm-config))
  (target (* wasmer-target)))

(define-alien-routine wasmer-cpu-features-add boolean
  (cpu-features (* wasmer-cpu-features))
  (feature (* wasm-name)))

(define-alien-routine wasmer-cpu-features-delete void
  (cpu-features (* wasmer-cpu-features)))

(define-alien-routine wasmer-cpu-features-new (* wasmer-cpu-features))

(define-alien-routine wasmer-features-bulk-memory boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-features-delete boolean
  (features (* wasmer-features)))

(define-alien-routine wasmer-features-memory64 boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-features-module-linking boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-features-multi-memory boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-features-multi-value boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-features-new (* wasmer-features))

(define-alien-routine wasmer-features-reference-types boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-features-simd boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-features-tail-call boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-features-threads boolean
  (features (* wasmer-features))
  (enable boolean))

(define-alien-routine wasmer-funcenv-delete boolean
  (funcenv (* wasmer-funcenv)))

(define-alien-routine wasmer-funcenv-new (* wasmer-funcenv)
  (store (* wasm-store))
  (data (* t)))

(define-alien-routine wasmer_is_engine_available boolean
  (engine wasmer-engine))

(define-alien-routine wasmer-is-headless boolean)

(define-alien-routine wasmer-last-error-length int)

(define-alien-routine wasmer-last-error-message int
  (buffer (* char))
  (length int))

(define-alien-routine wasmer-metering-as-middleware (* wasmer-middleware)
  (metering (* wasmer-metering)))

(define-alien-routine wasmer-metering-delete void
  (metering (* wasmer-metering)))

(define-alien-routine wasmer-metering-get-remaining-points unsigned-long
  (instance (* wasm-instance)))

;; TODO
(define-alien-type wasmer-metering-cost-function (function void))

(define-alien-routine wasmer-metering-new (* wasmer-metering)
  (initial-limit unsigned-long)
  (cost-function wasmer-metering-cost-function))

(define-alien-routine wasmer-metering-points-are-exhausted boolean
  (instance (* wasm-instance)))

(define-alien-routine wasmer-metering-set-remaining-points void
  (instance (* wasm-instance))
  (new-limit unsigned-long))

(define-alien-routine wasmer-module-name void
  (module (* wasm-module))
  (out (* wasm-name)))

(define-alien-routine wasmer-module-new (* wasm-module)
  (engine (* wasm-engine))
  (bytes (* wasm-byte-vec)))

(define-alien-routine wasmer-module-set-name boolean
  (module (* wasm-module))
  (name (* wasm-name)))

(define-alien-routine wasmer-setup-tracing void
  (verbosity-level int)
  (use-color int))

(define-alien-routine wasmer-target-delete void
  (target (* wasmer-target)))

(define-alien-routine wasmer_target_new (* wasmer-target)
  (triple (* wasmer-triple))
  (cpu-features (* wasmer-cpu-features)))

(define-alien-routine wasmer-triple-delete void
  (triple (* wasmer-triple)))

(define-alien-routine wasmer-triple-new (* wasmer-triple)
  (triple (* wasm-name)))

(define-alien-routine wasmer-triple-new-from-host (* wasmer-triple))

(define-alien-routine wasmer-version-major unsigned-char)

(define-alien-routine wasmer-version-minor unsigned-char)

(define-alien-routine wasmer-version-patch unsigned-char)

(define-alien-routine wasmer-version-pre c-string)

(define-alien-routine wat2wasm void
  (wat (* wasm-byte-vec))
  (out (* wasm-byte-vec)))
