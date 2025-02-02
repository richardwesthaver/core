;;; rustls/rustls.lisp --- Rustls Alien Routines

;;

;;; Code:
(in-package :rustls)

(define-alien-routine rustls-version c-string)

(define-alien-routine rustls-acceptor-new (* rustls-acceptor))

(define-alien-routine rustls-acceptor-free void (acceptor (* rustls-acceptor)))

(define-alien-routine rustls-acceptor-read-tls rustls-io-result
  (acceptor (* rustls-acceptor))
  (callback #+nil rustls-read-callback (* t))
  (userdata (* t))
  (out-n (* size-t)))

(define-alien-routine rustls-acceptor-accept rustls-result
  (acceptor (* rustls-acceptor))
  (out-accepted (* (* rustls-accepted)))
  (out-alert (* (* rustls-accepted-alert))))

(define-alien-routine rustls-accepted-server-name c-string
  (accepted (* rustls-accepted)))

(define-alien-routine rustls-accepted-signature-scheme (unsigned 16)
  (accepted (* rustls-accepted))
  (i size-t))

(define-alien-routine rustls-accepted-cipher-scheme (unsigned 16)
  (accepted (* rustls-accepted))
  (i size-t))

;; (define-alien-routine rustls-accepted-alpn rustls-slice-bytes
;;   (accepted (* rustls-accepted))
;;   (i size-t))

(define-alien-routine rustls-accepted-into-connection rustls-result
  (accepted (* rustls-accepted))
  (config (* rustls-server-config))
  (out-conn (* (* rustls-connection)))
  (out-alert (* (* rustls-accepted-alert))))

(define-alien-routine rustls-accepted-free void (accepted (* rustls-accepted)))

(define-alien-routine rustls-accepted-alert-write-tls rustls-io-result
  (accepted-alert (* rustls-accepted-alert))
  (callback rustls-write-callback)
  (userdata (* t))
  (out-n (* size-t)))

(define-alien-routine rustls-accepted-alert-free void
  (accepted-alert (* rustls-accepted-alert)))

(define-alien-routine rustls-certificate-get-der rustls-result
  (cert (* rustls-certificate))
  (out-der-data (* (* (unsigned 8))))
  (out-der-len (* size-t)))

(define-alien-routine rustls-supported-ciphersuite-get-suite (unsigned 16)
  (supported-ciphersuite (* rustls-supported-ciphersuite)))

(define-alien-routine rustls-supported-ciphersuite-get-name c-string
  (supported-ciphersuite (* rustls-supported-ciphersuite)))

(define-alien-routine rustls-all-ciphersuites-len size-t)

(define-alien-routine rustls-all-ciphersuites-get-entry (* rustls-supported-ciphersuite)
  (i size-t))

(define-alien-routine rustls-default-ciphersuites-len size-t)

(define-alien-routine rustls-default-ciphersuites-get-entry (* rustls-supported-ciphersuite)
  (i size-t))

(define-alien-routine rustls-certified-key-build rustls-result
  (cert-chain (array (unsigned 8)))
  (cert-chain-len size-t)
  (private-key (array (unsigned 8)))
  (private-key-len size-t)
  (certified-key-out (* (* rustls-certified-key))))

(define-alien-routine rustls-certified-key-get-certificate (* rustls-certificate)
  (certified-key (* rustls-certified-key))
  (i size-t))

(define-alien-routine rustls-certified-key-clone-with-ocsp rustls-result
  (certified-key (* rustls-certified-key))
  (ocsp-response (* rustls-slice-bytes))
  (clone-key-out (* (* rustls-certified-key))))

(define-alien-routine rustls-certified-key-free void (key (* rustls-certified-key)))

(define-alien-routine rustls-root-cert-store-builder-new (* rustls-root-cert-store-builder))

(define-alien-routine rustls-root-cert-store-builder-add-pem rustls-result
  (builder (* rustls-root-cert-store-builder))
  (pem (array (unsigned 8)))
  (pem-len size-t)
  (strict boolean))

(define-alien-routine rustls-root-cert-store-builder-load-roots-from-file rustls-result
  (builder (* rustls-root-cert-store-builder))
  (filename c-string)
  (strict boolean))

(define-alien-routine rustls-root-cert-store-builder-build rustls-result
  (builder (* rustls-root-cert-store-builder))
  (config-out (* (* rustls-root-cert-store))))

(define-alien-routine rustls-root-cert-store-builder-free void
  (builder (* rustls-root-cert-store-builder)))

(define-alien-routine rustls-root-cert-store-free void
  (storer (* rustls-root-cert-store)))

(define-alien-routine rustls-client-cert-verifier-free void
  (verifier (* rustls-client-cert-verifier)))

(define-alien-routine rustls-web-pki-client-cert-verifier-builder-new (* rustls-web-pki-client-cert-verifier-builder)
  (store (* rustls-root-cert-store)))

(define-alien-routine rustls-web-pki-client-cert-verifier-builder-add-crl rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder))
  (crl-pem (array (unsigned 8)))
  (crl-pem-len size-t))

(define-alien-routine rustls-web-pki-client-cert-verifier-only-check-end-entity-revocation rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(define-alien-routine rustls-web-pki-client-cert-verifier-allow-unknown-revocation-status rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(define-alien-routine rustls-web-pki-client-cert-verifier-allow-unauthenticated rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(define-alien-routine rustls-web-pki-client-cert-verifier-clear-root-hint-subjects rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

(define-alien-routine rustls-web-pki-client-cert-verifier-add-root-hint-subjects rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder))
  (store (* rustls-root-cert-store)))

(define-alien-routine rustls-web-pki-client-cert-verifier-builder-build rustls-result
  (builder (* rustls-web-pki-client-cert-verifier-builder))
  (verifier-out (* (* rustls-client-cert-verifier))))

(define-alien-routine rustls-web-pki-client-cert-verifier-builder-free void
  (builder (* rustls-web-pki-client-cert-verifier-builder)))

;;; rustls_web_pki_server_cert_verifier

;;; rustls_client_config
(define-alien-routine rustls-client-config-builder-new (* rustls-client-config-builder))

(define-alien-routine rustls-client-config-builder-new-custom rustls-result
  (provider (* rustls-crypto-provider))
   (tls-versions (* unsigned-short))
   (tls-versions-len size-t)
   (builder-out (* (* rustls-client-config-builder))))

(define-alien-routine rustls-client-config-builder-build rustls-result
  (builder (* rustls-client-config-builder))
  (config-out (* (* rustls-client-config))))

(define-alien-routine rustls-client-config-builder-set-certified-key rustls-result
  (builder (* rustls-client-config-builder))
  (certified-keys (* (* rustls-certified-key)))
  (certified-keys-len size-t))

(define-alien-routine rustls-client-config-builder-set-enable-sni void
  (builder (* rustls-client-config-builder))
  (enable boolean))

(define-alien-routine rustls-client-config-builder-set-alpn-protocols rustls-result
  (builder (* rustls-client-config-builder))
  (protocols (* rustls-slice-bytes))
  (len size-t))

(define-alien-routine rustls-client-config-builder-set-server-verifier void
  (builder (* rustls-client-config-builder))
  (verifier (* rustls-server-cert-verifier)))

(define-alien-routine rustls-client-config-builder-free void
  (c (* rustls-client-config-builder)))

(define-alien-routine rustls-platform-server-cert-verifier rustls-result
  (verifier-out (* (* rustls-server-cert-verifier))))

(define-alien-routine rustls-client-config-free void
  (c (* rustls-client-config)))

;; (define-alien-routine rustls-client-config-builder-dangerous-set-server-verifier rustls-result
;;   (builder (* rustls-client-config-builder))
;;   (callback rustls-verifiy-cert-callback))

;;; rustls_client_connection
(define-alien-routine rustls-client-connection-new rustls-result
  (config (* rustls-client-config))
  (server-name c-string)
  (conn-out (* (* rustls-connection))))

(define-alien-routine rustls-connection-set-userdata void
  (conn (* rustls-connection))
  (userdata (* t)))

(define-alien-routine rustls-connection-set-log-callback void
  (conn (* rustls-connection))
  (cb rustls-log-callback))

(define-alien-routine rustls-connection-read-tls rustls-io-result
  (conn (* rustls-connection))
  (callback rustls-read-callback)
  (userdata (* t))
  (out-n (* size-t)))

(define-alien-routine rustls-connection-write-tls rustls-io-result
  (conn (* rustls-connection))
  (callback rustls-write-callback)
  (userdata (* t))
  (out-n (* size-t)))

(define-alien-routine rustls-connection-write-tls-vectored rustls-io-result
  (conn (* rustls-connection))
  (callback rustls-write-vectored-callback)
  (userdata (* t))
  (out-n (* size-t)))

(define-alien-routine rustls-connection-process-new-packets rustls-result
  (conn (* rustls-connection)))

(define-alien-routine rustls-connection-wants-read boolean
  (conn (* rustls-connection)))

(define-alien-routine rustls-connection-wants-write boolean
  (conn (* rustls-connection)))

(define-alien-routine rustls-connection-is-handshaking boolean
  (conn (* rustls-connection)))

(define-alien-routine rustls-connection-set-buffer-limit void
  (conn (* rustls-connection))
  (n size-t))

(define-alien-routine rustls-connection-send-close-notify void
  (conn (* rustls-connection)))

(define-alien-routine rustls-connection-get-peer-certificate (* rustls-certificate)
  (conn (* rustls-connection))
  (i size-t))

(define-alien-routine rustls-connection-get-alpn-protocol void
  (conn (* rustls-connection))
  (protocol-out (* (array (unsigned 8))))
  (protocol-out-len (* size-t)))

(define-alien-routine rustls-connection-get-protocol-version (unsigned 16)
  (conn (* rustls-connection)))

(define-alien-routine rustls-connection-get-negotiated-ciphersuite (* rustls-supported-ciphersuite)
  (conn (* rustls-connection)))

(define-alien-routine rustls-connection-write rustls-result
  (conn (* rustls-connection))
  (buf (array (unsigned 8)))
  (count size-t)
  (out-n (* size-t)))

(define-alien-routine rustls-connection-read rustls-result
  (conn (* rustls-connection))
  (buf (array (unsigned 8)))
  (count size-t)
  (out-n (* size-t)))

(define-alien-routine rustl-connection-free void (conn (* rustls-connection)))

(define-alien-routine rustls-error void (result rustls-result) (buf (* unsigned-char)) (len size-t) (out-n (* size-t)))

(define-alien-routine rustls-result-is-cert-errorerror boolean (result rustls-result))

(define-alien-routine rustls-log-level-str c-string (level rustls-log-level))

(define-alien-routine rustls-slice-slice-bytes-len size-t
  (input (* rustls-slice-slice-bytes)))

;; (define-alien-routine rustls-slice-slice-bytes-get rustls-slice-bytes
;;   (input (* rustls-slice-slice-bytes))
;;   (n size-t))

(define-alien-routine rustls-slice-str-len size-t
  (input (* rustls-slice-str)))

(define-alien-routine rustls-slice-str-get c-string
  (input (* rustls-slice-str))
  (n size-t))

;;; rustls_server_config
(define-alien-routine rustls-server-config-builder-new (* rustls-server-config-builder))

(define-alien-routine rustls-server-config-builder-free void (config (* rustls-server-config-builder)))

(define-alien-routine rustls-server-config-builder-build (* rustls-server-config) (builder (* rustls-server-config-builder)))

(define-alien-routine rustls-server-config-free void (config (* rustls-server-config)))

(define-alien-routine rustls-server-connection-new rustls-result
  (config (* rustls-server-config))
  (conn-out (* (* rustls-connection))))

(define-alien-routine rustls-server-connection-get-server-name rustls-result
  (conn (* rustls-connection))
  (buf (* unsigned-char))
  (count size-t)
  (out-n (* size-t)))

(define-alien-routine rustls-server-config-builder-set-hello-callback rustls-result
  (builder (* rustls-server-config-builder))
  (callback rustls-client-hello-callback))

(define-alien-routine rustls-client-hello-select-certified-key rustls-result
  (hello (* rustls-client-hello))
  (certified-keys (* (* rustls-certified-key)))
  (certified-keys-len size-t)
  (out-key (* (* rustls-certified-key))))

(define-alien-routine rustls-server-config-builder-set-persistence rustls-result
  (builder (* rustls-server-config-builder))
  (get-cb rustls-session-store-get-callback)
  (put-cb rustls-session-store-put-callback))
