;;; tests/ciphers.lisp

;;; Code:
(in-package :ironclad/tests)

(deftest verify-key.bad-cipher ()
  (handler-case (crypto::verify-key :error
                                    (make-array 0
                                                :element-type '(unsigned-byte 8)))
    (crypto:unsupported-cipher () :ok)
    (:no-error () :error))
  :ok)

(deftest verify-key.bad-key0 ()
  (handler-case (crypto::verify-key :aes "")
    (type-error () :ok)
    (:no-error () :error))
  :ok)

(deftest verify-key.bad-key1 ()
  (handler-case (crypto::verify-key :aes nil)
    (crypto:key-not-supplied () :ok)
    (:no-error () :error))
  :ok)

(deftest unprovided-key ()
  (handler-case
      (crypto:make-cipher :blowfish :mode :ecb
                          :initialization-vector (make-array 8 :element-type '(unsigned-byte 8)))
    (crypto:key-not-supplied () :ok)
    (:no-error () :error))
  :ok)

(deftest unsupported-mode.1 ()
  (handler-case
      (crypto:make-cipher :blowfish :mode :stream
                          :key (make-array 8 :element-type '(unsigned-byte 8))
                          :initialization-vector (make-array 8 :element-type '(unsigned-byte 8)))
    (crypto:unsupported-mode () :ok)
    (:no-error () :error))
  :ok)

(deftest unsupported-mode.2 ()
  (handler-case
      (crypto:make-cipher :salsa20 :mode :cbc
                          :key (make-array 16 :element-type '(unsigned-byte 8)))
    (crypto:unsupported-mode () :ok)
    (:no-error () :error))
  :ok)

(deftest block-length.known-ciphers ()
  (dolist (name (crypto:list-all-ciphers) :ok)
    (unless (crypto:block-length name)
      (return :error)))
  :ok)

(deftest block-length.bad-cipher ()
  (crypto:block-length :error)
  nil)

(deftest key-lengths.known-ciphers ()
  (dolist (name (crypto:list-all-ciphers) :ok)
    (unless (crypto:key-lengths name)
      (return :error)))
  :ok)

(deftest key-lengths.bad-cipher ()
  (crypto:key-lengths :error)
  nil)

#.(loop for cipher in (crypto:list-all-ciphers)
        collect `(deftest ,cipher ()
                   (run-test-vector-file ',cipher *cipher-tests*) t) into forms
        finally (return `(progn ,@forms)))

#.(if (boundp '*cipher-stream-tests*)
      (loop for cipher in (crypto:list-all-ciphers)
            collect `(deftest ,(symbolicate cipher '#:/stream) ()
                       (run-test-vector-file ',cipher *cipher-stream-tests*) t)
              into forms
         finally (return `(progn ,@forms)))
      nil)

(deftest ciphers.crypto-package ()
  (every #'(lambda (s)
             (eq (nth-value 1 (find-symbol (symbol-name s)
                                           (find-package :ironclad)))
                 :external))
         (crypto:list-all-ciphers))
  t)

(deftest clean-symbols.ciphers ()
    (loop with n-ciphers = (length (crypto:list-all-ciphers))
     for s being each symbol of :crypto
     when (crypto::%find-cipher s)
     count s into computed-n-ciphers
     finally (return (= n-ciphers computed-n-ciphers)))
  t)
