;;; rustls/rustls.lisp --- Rustls Alien Routines

;;

;;; Code:
(in-package :rustls)

(defar rustls-version c-string)

;;; Provider
(defar rustls-crypto-provider-builder-new-from-default rustls-result 
  (builder-out (* (* rustls-crypto-provider-builder))))

(defar rustls-crypto-provider-builder-new-with-base (* rustls-crypto-provider-builder)
  (base (* rustls-crypto-provider)))

(defar rustls-crypto-provider-builder-set-cipher-suites rustls-result
  (builder (* rustls-crypto-provider-builder))
  (cipher-suites (* (* rustls-supported-ciphersuite)))
  (cipher-suites-len size-t))

(defar rustls-crypto-provider-builder-build rustls-result
  (builder (* rustls-crypto-provider-builder))
  (provider-out (* rustls-crypto-provider)))

(defar rustls-crypto-provider-builder-build-as-default rustls-result
  (builder (* rustls-crypto-provider-builder)))

(defar rustls-crypto-provider-builder-free void
  (builder (* rustls-crypto-provider-builder)))

;; NOTE: These are dependent on Rustls compile-time features
(defar rustls-ring-crypto-provider (* rustls-crypto-provider))
(defar rustls-default-fips-provider (* rustls-crypto-provider))

;; available with default config
(defar rustls-aws-lc-rs-crypto-provider (* rustls-crypto-provider))
(defar rustls-crypto-provider-default (* rustls-crypto-provider))
(defar rustls-crypto-provider-ciphersuites-len size-t
  (provider (* rustls-crypto-provider)))
(defar rustls-crypto-provider-ciphersuites-get (* rustls-supported-ciphersuite)
  (provider (* rustls-crypto-provider))
  (index size-t))
(defar rustls-crypto-provider-load-key rustls-result
  (provider (* rustls-crypto-provider))
  (private-key (* unsigned-char))
  (private-key-len size-t)
  (signing-key-out (* (* rustls-signing-key))))

;;; Acceptor
(defar rustls-acceptor-new (* rustls-acceptor))

(defar rustls-acceptor-free void (acceptor (* rustls-acceptor)))

(defar rustls-acceptor-read-tls rustls-io-result
  (acceptor (* rustls-acceptor))
  (callback (* rustls-read-callback))
  (userdata (* t))
  (out-n (* size-t)))

(defar rustls-acceptor-accept rustls-result
  (acceptor (* rustls-acceptor))
  (out-accepted (* (* rustls-accepted)))
  (out-alert (* (* rustls-accepted-alert))))

(defar rustls-accepted-server-name c-string
  (accepted (* rustls-accepted)))

(defar rustls-accepted-signature-scheme (unsigned 16)
  (accepted (* rustls-accepted))
  (i size-t))

(defar rustls-accepted-cipher-scheme (unsigned 16)
  (accepted (* rustls-accepted))
  (i size-t))

;; (defar rustls-accepted-alpn rustls-slice-bytes
;;   (accepted (* rustls-accepted))
;;   (i size-t))

(defar rustls-accepted-into-connection rustls-result
  (accepted (* rustls-accepted))
  (config (* rustls-server-config))
  (out-conn (* (* rustls-connection)))
  (out-alert (* (* rustls-accepted-alert))))

(defar rustls-accepted-free void (accepted (* rustls-accepted)))

(defar rustls-accepted-alert-write-tls rustls-io-result
  (accepted-alert (* rustls-accepted-alert))
  (callback rustls-write-callback)
  (userdata (* t))
  (out-n (* size-t)))

(defar rustls-accepted-alert-free void
  (accepted-alert (* rustls-accepted-alert)))

;;; Ciphersuite
(defar rustls-supported-ciphersuite-get-suite (unsigned 16)
  (supported-ciphersuite (* rustls-supported-ciphersuite)))

(defar rustls-supported-ciphersuite-get-name c-string
  (supported-ciphersuite (* rustls-supported-ciphersuite)))

(defar rustls-all-ciphersuites-len size-t)

(defar rustls-all-ciphersuites-get-entry (* rustls-supported-ciphersuite)
  (i size-t))

(defar rustls-default-ciphersuites-len size-t)

(defar rustls-default-ciphersuites-get-entry (* rustls-supported-ciphersuite)
  (i size-t))

(defar rustls-certificate-get-der rustls-result
  (cert (* rustls-certificate))
  (out-der-data (* (* (unsigned 8))))
  (out-der-len (* size-t)))

;;; Certified Key
(defar rustls-certified-key-build rustls-result
  (cert-chain (* (unsigned 8)))
  (cert-chain-len size-t)
  (private-key (* (unsigned 8)))
  (private-key-len size-t)
  (certified-key-out (* (* rustls-certified-key))))

(defar rustls-certified-key-get-certificate (* rustls-certificate)
  (certified-key (* rustls-certified-key))
  (i size-t))

(defar rustls-certified-key-clone-with-ocsp rustls-result
  (certified-key (* rustls-certified-key))
  (ocsp-response (* rustls-slice-bytes))
  (clone-key-out (* (* rustls-certified-key))))

(defar rustls-certified-key-keys-match rustls-result
  (key (* rustls-certified-key)))

(defar rustls-certified-key-free void (key (* rustls-certified-key)))

;;; Root Cert Store
(defar rustls-root-cert-store-builder-new (* rustls-root-cert-store-builder))

(defar rustls-root-cert-store-builder-add-pem rustls-result
  (builder (* rustls-root-cert-store-builder))
  (pem (array (unsigned 8)))
  (pem-len size-t)
  (strict boolean))

(defar rustls-root-cert-store-builder-load-roots-from-file rustls-result
  (builder (* rustls-root-cert-store-builder))
  (filename c-string)
  (strict boolean))

(defar rustls-root-cert-store-builder-build rustls-result
  (builder (* rustls-root-cert-store-builder))
  (config-out (* (* rustls-root-cert-store))))

(defar rustls-root-cert-store-builder-free void
  (builder (* rustls-root-cert-store-builder)))

(defar rustls-root-cert-store-free void
  (storer (* rustls-root-cert-store)))

;;; Client Cert Verifier
(defar rustls-client-cert-verifier-free void
  (verifier (* rustls-client-cert-verifier)))

(defar rustls-web-pki-client-cert-verifier-builder-new (* rustls-web-pki-client-cert-verifier-builder)
  (store (* rustls-root-cert-store)))

(defar rustls-web-pki-client-cert-verifier-builder-add-crl rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder))
  (crl-pem (array (unsigned 8)))
  (crl-pem-len size-t))

(defar rustls-web-pki-client-cert-verifier-only-check-end-entity-revocation rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(defar rustls-web-pki-client-cert-verifier-allow-unknown-revocation-status rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(defar rustls-web-pki-client-cert-verifier-allow-unauthenticated rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(defar rustls-web-pki-client-cert-verifier-clear-root-hint-subjects rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(defar rustls-web-pki-client-cert-verifier-add-root-hint-subjects rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder))
  (store (* rustls-root-cert-store)))

(defar rustls-web-pki-client-cert-verifier-builder-build rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder))
  (verifier-out (* (* rustls-web-pki-client-cert-verifier))))

(defar rustls-web-pki-client-cert-verifier-builder-free void
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(defar rustls-web-pki-server-cert-verifier-builder-new (* rustls-web-pki-server-cert-verifier-builder)
  (store (* rustls-root-cert-store)))
(defar rustls-web-pki-server-cert-verifier-builder-new-with-provider (* rustls-web-pki-server-cert-verifier-builder)
  (provider (* rustls-crypto-provider))
  (store (* rustls-root-cert-store)))
(defar rustls-web-pki-server-cert-verifier-builder-add-crl rustls-result
  (builder (* rustls-web-pki-server-cert-verifier-builder))
  (crl-pem (* unsigned-char))
  (crl-pem-len size-t))
(defar rustls-web-pki-server-cert-verifier-only-check-end-entity-revocation rustls-result
  (builder (* rustls-web-pki-server-cert-verifier-builder)))
(defar rustls-web-pki-server-cert-verifier-enforce-revocation-expir rustls-result
  (builder (* rustls-web-pki-server-cert-verifier-builder)))
(defar rustls-web-pki-server-cert-verifier-builder-build rustls-result
  (builder (* rustls-web-pki-server-cert-verifier-builder))
  (verifier-out (* rustls-server-cert-verifier)))
(defar rustls-web-pki-server-cert-verifier-builder-free void
  (builder (* rustls-web-pki-server-cert-verifier-builder)))
(defar rustls-platform-server-cert-verifier rustls-result
  (verifier-out (* (* rustls-server-cert-verifier))))
(defar rustls-platform-server-cert-verifier-with-provider (* rustls-server-cert-verifier)
  (provider (* rustls-crypto-provider)))
(defar rustls-server-cert-verifier-free void
  (verifier (* rustls-server-cert-verifier)))

;;; Client Config
(defar rustls-client-config-builder-new (* rustls-client-config-builder))

(defar rustls-client-config-builder-new-custom rustls-result
  (provider (* rustls-crypto-provider))
   (tls-versions (* unsigned-short))
   (tls-versions-len size-t)
   (builder-out (* (* rustls-client-config-builder))))

(defar rustls-client-config-builder-build rustls-result
  (builder (* rustls-client-config-builder))
  (config-out (* (* rustls-client-config))))

(defar rustls-client-config-builder-set-certified-key rustls-result
  (builder (* rustls-client-config-builder))
  (certified-keys (* (* rustls-certified-key)))
  (certified-keys-len size-t))

(defar rustls-client-config-builder-set-enable-sni void
  (builder (* rustls-client-config-builder))
  (enable boolean))

(defar rustls-client-config-builder-set-alpn-protocols rustls-result
  (builder (* rustls-client-config-builder))
  (protocols (* rustls-slice-bytes))
  (len size-t))

(defar rustls-client-config-builder-set-server-verifier void
  (builder (* rustls-client-config-builder))
  (verifier (* rustls-server-cert-verifier)))

(defar rustls-client-config-builder-free void
  (c (* rustls-client-config-builder)))

(defar rustls-client-config-builder-set-key-log-file rustls-result (builder (* rustls-client-config-builder)))

(defar rustls-client-config-builder-set-key-log rustls-result 
  (builder (* rustls-client-config-builder))
  (log-cb (* rustls-keylog-log-callback))
  (will-log-cb (* rustls-keylog-will-log-callback)))

(defar rustls-client-config-builder-enable-ech rustls-result
  (builder (* rustls-client-config-builder))
  (ech-config-list-bytes (* unsigned-char))
  (ech-config-list-bytes-size size-t)
  (hpke (* rustls-hpke)))

(defar rustls-client-config-builder-enable-ech-grease rustls-result
  (builder (* rustls-client-config-builder))
  (hpke (* rustls-hpke)))

(defar rustls-client-config-free void
  (c (* rustls-client-config)))

(defar rustls-client-config-fips boolean
  (config (* rustls-client-config)))

;; (defar rustls-client-config-builder-dangerous-set-server-verifier rustls-result
;;   (builder (* rustls-client-config-builder))
;;   (callback rustls-verifiy-cert-callback))

;;; Client Connection
(defar rustls-client-connection-new rustls-result
  (config (* rustls-client-config))
  (server-name c-string)
  (conn-out (* (* rustls-connection))))

(defar rustls-connection-set-userdata void
  (conn (* rustls-connection))
  (userdata (* t)))

(defar rustls-connection-set-log-callback void
  (conn (* rustls-connection))
  (cb rustls-log-callback))

(defar rustls-connection-read-tls rustls-io-result
  (conn (* rustls-connection))
  (callback rustls-read-callback)
  (userdata (* t))
  (out-n (* size-t)))

(defar rustls-connection-write-tls rustls-io-result
  (conn (* rustls-connection))
  (callback rustls-write-callback)
  (userdata (* t))
  (out-n (* size-t)))

(defar rustls-connection-write-tls-vectored rustls-io-result
  (conn (* rustls-connection))
  (callback rustls-write-vectored-callback)
  (userdata (* t))
  (out-n (* size-t)))

(defar rustls-connection-process-new-packets rustls-result
  (conn (* rustls-connection)))

(defar rustls-connection-wants-read boolean
  (conn (* rustls-connection)))

(defar rustls-connection-wants-write boolean
  (conn (* rustls-connection)))

(defar rustls-connection-is-handshaking boolean
  (conn (* rustls-connection)))

(defar rustls-connection-set-buffer-limit void
  (conn (* rustls-connection))
  (n size-t))

(defar rustls-connection-send-close-notify void
  (conn (* rustls-connection)))

(defar rustls-connection-get-peer-certificate (* rustls-certificate)
  (conn (* rustls-connection))
  (i size-t))

(defar rustls-connection-get-alpn-protocol void
  (conn (* rustls-connection))
  (protocol-out (* (array (unsigned 8))))
  (protocol-out-len (* size-t)))

(defar rustls-connection-get-protocol-version (unsigned 16)
  (conn (* rustls-connection)))

(defar rustls-connection-get-negotiated-ciphersuite (* rustls-supported-ciphersuite)
  (conn (* rustls-connection)))

(defar rustls-connection-write rustls-result
  (conn (* rustls-connection))
  (buf (array (unsigned 8)))
  (count size-t)
  (out-n (* size-t)))

(defar rustls-connection-read rustls-result
  (conn (* rustls-connection))
  (buf (array (unsigned 8)))
  (count size-t)
  (out-n (* size-t)))

(defar rustl-connection-free void (conn (* rustls-connection)))

(defar rustls-error void (result rustls-result) (buf (* unsigned-char)) (len size-t) (out-n (* size-t)))

(defar rustls-result-is-cert-error boolean (result rustls-result))

(defar rustls-log-level-str c-string (level rustls-log-level))

(defar rustls-slice-slice-bytes-len size-t
  (input (* rustls-slice-slice-bytes)))

;; (defar rustls-slice-slice-bytes-get rustls-slice-bytes
;;   (input (* rustls-slice-slice-bytes))
;;   (n size-t))

(defar rustls-slice-str-len size-t
  (input (* rustls-slice-str)))

(defar rustls-slice-str-get c-string
  (input (* rustls-slice-str))
  (n size-t))

;;; Server Config
(defar rustls-server-config-builder-new (* rustls-server-config-builder))

(defar rustls-server-config-builder-free void (config (* rustls-server-config-builder)))

(defar rustls-server-config-builder-build rustls-result
  (builder (* rustls-server-config-builder))
  (config-out (* (* rustls-server-config))))

(defar rustls-server-config-builder-set-key-log-file rustls-result (builder (* rustls-server-config-builder)))

(defar rustls-server-config-builder-set-key-log rustls-result 
  (builder (* rustls-server-config-builder))
  (log-cb (* rustls-keylog-log-callback))
  (will-log-cb (* rustls-keylog-will-log-callback)))

(defar rustls-server-config-free void (config (* rustls-server-config)))

;;; Server Connection
(defar rustls-server-connection-new rustls-result
  (config (* rustls-server-config))
  (conn-out (* (* rustls-connection))))

(defar rustls-server-connection-get-server-name rustls-result
  (conn (* rustls-connection))
  (buf (* unsigned-char))
  (count size-t)
  (out-n (* size-t)))

(defar rustls-server-config-builder-set-hello-callback rustls-result
  (builder (* rustls-server-config-builder))
  (callback (* rustls-client-hello-callback)))

(defar rustls-client-hello-select-certified-key rustls-result
  (hello (* rustls-client-hello))
  (certified-keys (* (* rustls-certified-key)))
  (certified-keys-len size-t)
  (out-key (* (* rustls-certified-key))))

(defar rustls-server-config-builder-set-persistence rustls-result
  (builder (* rustls-server-config-builder))
  (get-cb rustls-session-store-get-callback)
  (put-cb rustls-session-store-put-callback))

(defar rustls-server-config-builder-set-ignore-client-order rustls-result
  (builder (* rustls-server-config-builder))
  (ignore boolean))

(defar rustls-server-config-builder-set-client-verifier void
  (builder (* rustls-server-config-builder))
  (verifier (* rustls-client-cert-verifier)))
