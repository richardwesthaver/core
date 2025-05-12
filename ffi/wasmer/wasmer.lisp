;;; wasmer/wasmer.lisp --- Wasmer Alien Routines

;;

;;; Code:
(in-package :wasmer)


(defar wasmer-version c-string)

(defar wasm-config-new (* wasm-config))
(defar wasm-engine-new (* wasm-engine))
(defar wasm-store-new (* wasm-store) (engine (* wasm-engine)))
(defar wasm-store-delete void (store (* wasm-store)))
(defar wasm-engine-delete void (engine (* wasm-engine)))

(defar wasm-instance-new (* wasm-instance)
  (store (* wasm-store))
  (module (* wasm-module))
  (imports (* wasm-extern-vec))
  (trap (* (* wasm-trap))))

(defar wasm-config-canonicalize-nans void
  (config (* wasm-config))
  (enable boolean))

(defar wasm-config-push-middleware void
  (config (* wasm-config))
  (middleware (* wasmer-middleware)))

(defar wasm-config-set-engine void
  (config (* wasm-config))
  (engine int))

(defar wasm-config-set-features void
  (config (* wasm-config))
  (features (* wasmer-features)))

(defar wasm-config-set-target void
  (config (* wasm-config))
  (target (* wasmer-target)))

(defar wasmer-cpu-features-add boolean
  (cpu-features (* wasmer-cpu-features))
  (feature (* wasm-name)))

(defar wasmer-cpu-features-delete void
  (cpu-features (* wasmer-cpu-features)))

(defar wasmer-cpu-features-new (* wasmer-cpu-features))

(defar wasmer-features-bulk-memory boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-features-delete boolean
  (features (* wasmer-features)))

(defar wasmer-features-memory64 boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-features-module-linking boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-features-multi-memory boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-features-multi-value boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-features-new (* wasmer-features))

(defar wasmer-features-reference-types boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-features-simd boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-features-tail-call boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-features-threads boolean
  (features (* wasmer-features))
  (enable boolean))

(defar wasmer-funcenv-delete boolean
  (funcenv (* wasmer-funcenv)))

(defar wasmer-funcenv-new (* wasmer-funcenv)
  (store (* wasm-store))
  (data (* t)))

(defar wasmer_is_engine_available boolean
  (engine wasmer-engine))

(defar wasmer-is-headless boolean)

(defar wasmer-last-error-length int)

(defar wasmer-last-error-message int
  (buffer (* char))
  (length int))

(defar wasmer-metering-as-middleware (* wasmer-middleware)
  (metering (* wasmer-metering)))

(defar wasmer-metering-delete void
  (metering (* wasmer-metering)))

(defar wasmer-metering-get-remaining-points unsigned-long
  (instance (* wasm-instance)))

;; TODO
(define-alien-type wasmer-metering-cost-function (function void))

(defar wasmer-metering-new (* wasmer-metering)
  (initial-limit unsigned-long)
  (cost-function wasmer-metering-cost-function))

(defar wasmer-metering-points-are-exhausted boolean
  (instance (* wasm-instance)))

(defar wasmer-metering-set-remaining-points void
  (instance (* wasm-instance))
  (new-limit unsigned-long))

(defar wasmer-module-name void
  (module (* wasm-module))
  (out (* wasm-name)))

(defar wasmer-module-new (* wasm-module)
  (engine (* wasm-engine))
  (bytes (* wasm-byte-vec)))

(defar wasmer-module-set-name boolean
  (module (* wasm-module))
  (name (* wasm-name)))

(defar wasmer-setup-tracing void
  (verbosity-level int)
  (use-color int))

(defar wasmer-target-delete void
  (target (* wasmer-target)))

(defar wasmer_target_new (* wasmer-target)
  (triple (* wasmer-triple))
  (cpu-features (* wasmer-cpu-features)))

(defar wasmer-triple-delete void
  (triple (* wasmer-triple)))

(defar wasmer-triple-new (* wasmer-triple)
  (triple (* wasm-name)))

(defar wasmer-triple-new-from-host (* wasmer-triple))

(defar wasmer-version-major unsigned-char)

(defar wasmer-version-minor unsigned-char)

(defar wasmer-version-patch unsigned-char)

(defar wasmer-version-pre c-string)

(defar wat2wasm void
  (wat (* wasm-byte-vec))
  (out (* wasm-byte-vec)))
