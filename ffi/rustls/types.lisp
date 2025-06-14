;;; rustls/types.lisp --- Rustls FFI Types

;;

;;; Code:
(in-package :rustls)

(define-alien-enum (rustls-handshake-kind int)
  :unknown 0
  :full 1
  :full-with-hello-retry-request 2
  :resumed 3)

(define-alien-enum (rustls-result unsigned-int)
  :ok 7000
  :io 7001
  :null-parameter 7002
  :invalid-dns-name-error 7003
  :panic 7004
  :certificate-parse-error 7005
  :private-key-parse-error 7006
  :insufficient-size 7007
  :not-found 7008
  :invalid-parameter 7009
  :unexpected-eof 7010
  :plaintext-empty 7011
  :acceptor-not-ready 7012
  :already-used 7013
  :certificate-revocation-list-parse-error 7014
  :no-server-cert-verifier 7015
  :no-default-crypto-provider 7016
  :get-random-failed 7017
  :no-cert-resolver 7018
  :hpke-error 7019
  :builder-incompatible-tls-versions 7020
  :no-certificates-presented 7101
  :decrypt-error 7102
  :failed-to-get-current-time 7103
  :failed-to-get-random-bytes 7113
  :handshake-not-complete 7104
  :peer-sent-oversized-record 7105
  :no-application-protocol 7106
  :bad-max-fragment-size 7114
  :unsupported-name-type 7115
  :encrypt-error 7116
  :cert-encoding-bad 7121
  :cert-expired 7122
  :cert-not-yet-valid 7123
  :cert-revoked 7124
  :cert-unhandled-critical-extension 7125
  :cert-unknown-issuer 7126
  :cert-bad-signature 7127
  :cert-not-valid-for-name 7128
  :cert-invalid-purpose 7129
  :cert-application-verification-failure 7130
  :cert-other-error 7131
  :message-handshake-payload-too-large 7133
  :message-invalid-ccs 7134
  :message-invalid-content-type 7135
  :message-invalid-cert-status-type 7136
  :message-invalid-cert-request 7137
  :message-invalid-dh-params 7138
  :message-invalid-empty-payload 7139
  :message-invalid-key-update 7140
  :message-invalid-server-name 7141
  :message-too-large 7142
  :message-too-short 7143
  :message-missing-data 7144
  :message-missing-key-exchange 7145
  :message-no-signature-schemes 7146
  :message-trailing-data 7147
  :message-unexpected-message 7148
  :message-unknown-protocol-version 7149
  :message-unsupported-compression 7150
  :message-unsupported-curve-type 7151
  :message-unsupported-key-exchange-algorithm 7152
  :message-invalid-other 7153
  :peer-incompatible-error 7107
  :peer-misbehaved-error 7108
  :inappropriate-message 7109
  :inappropriate-handshake-message 7110
  :general 7112
  :alert-close-notify 7200
  :alert-unexpected-message 7201
  :alert-bad-record-mac 7202
  :alert-decryption-failed 7203
  :alert-record-overflow 7204
  :alert-decompression-failure 7205
  :alert-handshake-failure 7206
  :alert-no-certificate 7207
  :alert-bad-certificate 7208
  :alert-unsupported-certificate 7209
  :alert-certificate-revoked 7210
  :alert-certificate-expired 7211
  :alert-certificate-unknown 7212
  :alert-illegal-parameter 7213
  :alert-unknown-ca 7214
  :alert-access-denied 7215
  :alert-decode-error 7216
  :alert-decrypt-error 7217
  :alert-export-restriction 7218
  :alert-protocol-version 7219
  :alert-insufficient-security 7220
  :alert-internal-error 7221
  :alert-inappropriate-fallback 7222
  :alert-user-canceled 7223
  :alert-no-renegotiation 7224
  :alert-missing-extension 7225
  :alert-unsupported-extension 7226
  :alert-certificate-unobtainable 7227
  :alert-unrecognised-name 7228
  :alert-bad-certificate-status-response 7229
  :alert-bad-certificate-hash-value 7230
  :alert-unknown-psk-identity 7231
  :alert-certificate-required 7232
  :alert-no-application-protocol 7233
  :alert-unknown 7234
  :cert-revocation-list-bad-signature 7400
  :cert-revocation-list-invalid-crl-number 7401
  :cert-revocation-list-invalid-revoked-cert-serial-number 7402
  :cert-revocation-list-issuer-invalid-for-crl 7403
  :cert-revocation-list-other-error 7404
  :cert-revocation-list-parse-error 7405
  :cert-revocation-list-unsupported-crl-version 7406
  :cert-revocation-list-unsupported-critical-extension 7407
  :cert-revocation-list-unsupported-delta-crl 7408
  :cert-revocation-list-unsupported-indirect-crl 7409
  :cert-revocation-list-unsupported-revocation-reason 7410
  :client-cert-verifier-builder-no-root-anchors 7500)

(define-alien-enum (rustls-tls-version int)
  :unknown 0
  :sslv2 512
  :sslv3 768
  :tlsv1-0 769
  :tlsv1-1 770
  :tlsv1-2 771
  :tlsv1-3 772)

(define-alien-type rustls-crypto-provider (struct rustls-crypto-provider))
(define-alien-type rustls-crypto-provider-builder (struct rustls-crypto-provider-builder))
(define-alien-type rustls-signing-key (struct rustls-signing-key))

(define-alien-type rustls-accepted (struct rustls-accepted))

(define-alien-type rustls-accepted-alert (struct rustls-accepted-alert))

(define-alien-type rustls-acceptor (struct rustls-acceptor))

(define-alien-type rustls-certificate (struct rustls-certificate))

(define-alien-type rustls-certified-key (struct rustls-certified-key))

(define-alien-type rustls-client-cert-verifier (struct rustls-client-cert-verifier))

(define-alien-type rustls-client-config (struct rustls-client-config))

(define-alien-type rustls-client-config-builder (struct rustls-client-config-builder))

(define-alien-type rustls-connection (struct rustls-connection))

(define-alien-type rustls-iovec (struct rustls-iovec))

(define-alien-type rustls-root-cert-store (struct rustls-root-cert-store))

(define-alien-type rustls-root-cert-store-builder (struct rustls-root-cert-store-builder))

(define-alien-type rustls-server-cert-verifier (struct rustls-server-cert-verifier))

(define-alien-type rustls-server-config (struct rustls-server-config))

(define-alien-type rustls-server-config-builder (struct rustls-server-config-builder))

(define-alien-type rustls-slice-slice-bytes 
    (struct rustls-slice-slice-bytes
      (data (* unsigned-char))
      (len size-t)))

(define-alien-type rustls-slice-str (struct rustls-slice-str))

(define-alien-type rustls-supported-ciphersuite (struct rustls-supported-ciphersuite))

(define-alien-type rustls-web-pki-client-cert-verifier (struct rustls-web-pki-client-cert-verifier))

(define-alien-type rustls-web-pki-server-cert-verifier (struct rustls-web-pki-server-cert-verifier))

(define-alien-type rustls-str 
    (struct rustls-str
      (data (* char))
      (len size-t)))

(define-alien-type rustls-io-result int)

(define-alien-type rustls-slice-bytes 
    (struct rustls-slice-bytes
      (data (* unsigned-char))
      (len size-t)))

(define-alien-type rustls-verify-server-cert-user-data (* t))

(define-alien-type rustls-verify-server-cert-params 
    (struct rustls-verify-server-cert-params
      (end-entity-cert-der rustls-slice-bytes)
      (intermediate-certs-der (* rustls-slice-slice-bytes))
      (server-name rustls-str)
      (ocsp-response rustls-slice-bytes)))

(define-alien-type rustls-verify-server-cert-callback
    (function unsigned-int
        rustls-verify-server-cert-user-data
        (* rustls-verify-server-cert-params)))

(define-alien-type rustls-log-level size-t)

(define-alien-type rustls-log-params 
    (struct rustls-log-params
      (level rustls-log-level)
      (message rustls-str)))

(define-alien-type rustls-log-callback
    (function void
        (* t)
        (* rustls-log-params)))

(define-alien-type rustls-keylog-log-callback
    (function void
        rustls-str
        (* unsigned-char)
      size-t
      (* unsigned-char)
      size-t))

(define-alien-type rustls-keylog-will-log-callback (function int rustls-str))
        
(define-alien-type rustls-client-hello-userdata (* t))

(define-alien-type rustls-slice-u16 
    (struct rustls-slice-u16
      (data (* unsigned-short))
      (len size-t)))

(define-alien-type rustls-client-hello 
    (struct rustls-client-hello
      (server-name rustls-str)
      (signature-schemes rustls-slice-u16)
      (alpn (* rustls-slice-slice-bytes))))

(define-alien-type rustls-certified-key (struct rustls-certified-key))

#|
* NOTE:
* - the passed in `hello` and all its values are only available during the
*   callback invocations.
* - the passed callback function must be safe to call multiple times concurrently
*   with the same userdata, unless there is only a single config and connection
*   where it is installed.
|#
(define-alien-type rustls-client-hello-callback 
    (function (* rustls-certified-key)
        rustls-client-hello-userdata
        (* rustls-client-hello)))

(define-alien-type rustls-session-store-userdata (* t))

(define-alien-type rustls-session-store-get-callback
    (function unsigned-int
        rustls-session-store-userdata
        (* rustls-slice-bytes)
      int
      (* unsigned-char)
      size-t
      (* size-t)))

(define-alien-type rustls-session-store-put-callback
    (function unsigned-int
        rustls-session-store-userdata
        (* rustls-slice-bytes)
      (* rustls-slice-bytes)))

(define-alien-type rustls-supported-ciphersuite (struct rustls-supported-ciphersuite))

(define-alien-type rustls-web-pki-client-cert-verifier-builder (struct rustls-web-pki-client-vert-verifier-builder))

(define-alien-type rustls-web-pki-server-cert-verifier-builder (struct rustls-web-pki-server-cert-verifier-builder))

(define-alien-type rustls-read-callback
    (function rustls-io-result
        (* t)
        (* unsigned-char)
      size-t
      (* size-t)))

(define-alien-type rustls-write-callback
    (function rustls-io-result
        (* t)
        (* unsigned-char)
      size-t
      (* size-t)))

(define-alien-type rustls-write-vectored-callback
    (function rustls-io-result
        (* t)
        (* rustls-iovec)
      size-t
      (* size-t)))
