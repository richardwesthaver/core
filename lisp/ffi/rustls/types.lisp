;;; rustls/types.lisp --- Rustls FFI Types

;;

;;; Code:
(in-package :rustls)

(define-alien-type rustls-result unsigned-int)

(defconstant +rustls-result-ok+ 7000)
(defconstant +rustls-result-io+ 7001)
(defconstant +rustls-result-null-parameter+ 7002)
(defconstant +rustls-result-invalid-dns-name-error+ 7003)
(defconstant +rustls-result-panic+ 7004)
(defconstant +rustls-result-certificate-parse-error+ 7005)
(defconstant +rustls-result-private-key-parse-error+ 7006)
(defconstant +rustls-result-insufficient-size+ 7007)
(defconstant +rustls-result-not-found+ 7008)
(defconstant +rustls-result-invalid-parameter+ 7009)
(defconstant +rustls-result-unexpected-eof+ 7010)
(defconstant +rustls-result-plaintext-empty+ 7011)
(defconstant +rustls-result-acceptor-not-ready+ 7012)
(defconstant +rustls-result-already-used+ 7013)
(defconstant +rustls-result-certificate-revocation-list-parse-error+ 7014)
(defconstant +rustls-result-no-certificates-presented+ 7101)
(defconstant +rustls-result-decrypt-error+ 7102)
(defconstant +rustls-result-failed-to-get-current-time+ 7103)
(defconstant +rustls-result-failed-to-get-random-bytes+ 7113)
(defconstant +rustls-result-handshake-not-complete+ 7104)
(defconstant +rustls-result-peer-sent-oversized-record+ 7105)
(defconstant +rustls-result-no-application-protocol+ 7106)
(defconstant +rustls-result-bad-max-fragment-size+ 7114)
(defconstant +rustls-result-unsupported-name-type+ 7115)
(defconstant +rustls-result-encrypt-error+ 7116)
(defconstant +rustls-result-cert-encoding-bad+ 7121)
(defconstant +rustls-result-cert-expired+ 7122)
(defconstant +rustls-result-cert-not-yet-valid+ 7123)
(defconstant +rustls-result-cert-revoked+ 7124)
(defconstant +rustls-result-cert-unhandled-critical-extension+ 7125)
(defconstant +rustls-result-cert-unknown-issuer+ 7126)
(defconstant +rustls-result-cert-bad-signature+ 7127)
(defconstant +rustls-result-cert-not-valid-for-name+ 7128)
(defconstant +rustls-result-cert-invalid-purpose+ 7129)
(defconstant +rustls-result-cert-application-verification-failure+ 7130)
(defconstant +rustls-result-cert-other-error+ 7131)
(defconstant +rustls-result-message-handshake-payload-too-large+ 7133)
(defconstant +rustls-result-message-invalid-ccs+ 7134)
(defconstant +rustls-result-message-invalid-content-type+ 7135)
(defconstant +rustls-result-message-invalid-cert-status-type+ 7136)
(defconstant +rustls-result-message-invalid-cert-request+ 7137)
(defconstant +rustls-result-message-invalid-dh-params+ 7138)
(defconstant +rustls-result-message-invalid-empty-payload+ 7139)
(defconstant +rustls-result-message-invalid-key-update+ 7140)
(defconstant +rustls-result-message-invalid-server-name+ 7141)
(defconstant +rustls-result-message-too-large+ 7142)
(defconstant +rustls-result-message-too-short+ 7143)
(defconstant +rustls-result-message-missing-data+ 7144)
(defconstant +rustls-result-message-missing-key-exchange+ 7145)
(defconstant +rustls-result-message-no-signature-schemes+ 7146)
(defconstant +rustls-result-message-trailing-data+ 7147)
(defconstant +rustls-result-message-unexpected-message+ 7148)
(defconstant +rustls-result-message-unknown-protocol-version+ 7149)
(defconstant +rustls-result-message-unsupported-compression+ 7150)
(defconstant +rustls-result-message-unsupported-curve-type+ 7151)
(defconstant +rustls-result-message-unsupported-key-exchange-algorithm+ 7152)
(defconstant +rustls-result-message-invalid-other+ 7153)
(defconstant +rustls-result-peer-incompatible-error+ 7107)
(defconstant +rustls-result-peer-misbehaved-error+ 7108)
(defconstant +rustls-result-inappropriate-message+ 7109)
(defconstant +rustls-result-inappropriate-handshake-message+ 7110)
(defconstant +rustls-result-general+ 7112)
(defconstant +rustls-result-alert-close-notify+ 7200)
(defconstant +rustls-result-alert-unexpected-message+ 7201)
(defconstant +rustls-result-alert-bad-record-mac+ 7202)
(defconstant +rustls-result-alert-decryption-failed+ 7203)
(defconstant +rustls-result-alert-record-overflow+ 7204)
(defconstant +rustls-result-alert-decompression-failure+ 7205)
(defconstant +rustls-result-alert-handshake-failure+ 7206)
(defconstant +rustls-result-alert-no-certificate+ 7207)
(defconstant +rustls-result-alert-bad-certificate+ 7208)
(defconstant +rustls-result-alert-unsupported-certificate+ 7209)
(defconstant +rustls-result-alert-certificate-revoked+ 7210)
(defconstant +rustls-result-alert-certificate-expired+ 7211)
(defconstant +rustls-result-alert-certificate-unknown+ 7212)
(defconstant +rustls-result-alert-illegal-parameter+ 7213)
(defconstant +rustls-result-alert-unknown-ca+ 7214)
(defconstant +rustls-result-alert-access-denied+ 7215)
(defconstant +rustls-result-alert-decode-error+ 7216)
(defconstant +rustls-result-alert-decrypt-error+ 7217)
(defconstant +rustls-result-alert-export-restriction+ 7218)
(defconstant +rustls-result-alert-protocol-version+ 7219)
(defconstant +rustls-result-alert-insufficient-security+ 7220)
(defconstant +rustls-result-alert-internal-error+ 7221)
(defconstant +rustls-result-alert-inappropriate-fallback+ 7222)
(defconstant +rustls-result-alert-user-canceled+ 7223)
(defconstant +rustls-result-alert-no-renegotiation+ 7224)
(defconstant +rustls-result-alert-missing-extension+ 7225)
(defconstant +rustls-result-alert-unsupported-extension+ 7226)
(defconstant +rustls-result-alert-certificate-unobtainable+ 7227)
(defconstant +rustls-result-alert-unrecognised-name+ 7228)
(defconstant +rustls-result-alert-bad-certificate-status-response+ 7229)
(defconstant +rustls-result-alert-bad-certificate-hash-value+ 7230)
(defconstant +rustls-result-alert-unknown-psk-identity+ 7231)
(defconstant +rustls-result-alert-certificate-required+ 7232)
(defconstant +rustls-result-alert-no-application-protocol+ 7233)
(defconstant +rustls-result-alert-unknown+ 7234)
(defconstant +rustls-result-cert-revocation-list-bad-signature+ 7400)
(defconstant +rustls-result-cert-revocation-list-invalid-crl-number+ 7401)
(defconstant +rustls-result-cert-revocation-list-invalid-revoked-cert-serial-number+ 7402)
(defconstant +rustls-result-cert-revocation-list-issuer-invalid-for-crl+ 7403)
(defconstant +rustls-result-cert-revocation-list-other-error+ 7404)
(defconstant +rustls-result-cert-revocation-list-parse-error+ 7405)
(defconstant +rustls-result-cert-revocation-list-unsupported-crl-version+ 7406)
(defconstant +rustls-result-cert-revocation-list-unsupported-critical-extension+ 7407)
(defconstant +rustls-result-cert-revocation-list-unsupported-delta-crl+ 7408)
(defconstant +rustls-result-cert-revocation-list-unsupported-indirect-crl+ 7409)
(defconstant +rustls-result-cert-revocation-list-unsupported-revocation-reason+ 7410)
(defconstant +rustls-result-client-cert-verifier-builder-no-root-anchors+ 7500)

(define-alien-type rustls-tls-version int)

(defconstant +rustls-tls-version-sslv2+ 512)
(defconstant +rustls-tls-version-sslv3+ 768)
(defconstant +rustls-tls-version-tlsv1-0+ 769)
(defconstant +rustls-tls-version-tlsv1-1+ 770)
(defconstant +rustls-tls-version-tlsv1-2+ 771)
(defconstant +rustls-tls-version-tlsv1-3+ 772)

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

(define-alien-type rustls-slice-slice-bytes (struct rustls-slice-slice-bytes))

(define-alien-type rustls-slice-str (struct rustls-slice-str))

(define-alien-type rustls-supported-ciphersuite (struct rustls-supported-ciphersuite))

(define-alien-type rustls-web-pki-client-cert-verifier-builder (struct rustls-web-pki-client-cert-verifier-builder))

(define-alien-type rustls-web-pki-server-cert-verifier-builder (struct rustls-web-pki-server-cert-verifier-builder))

(define-alien-type rustls-str (struct rustls-str))

(define-alien-type rustls-io-result int)

(define-alien-type rustls-slice-bytes (struct rustls-slice-bytes))

(define-alien-type rustls-verify-server-cert-user-data (* t))

(define-alien-type rustls-verify-server-cert-params (struct rustls-verify-server-cert-params))

(define-alien-type rustls-log-level size-t)

(define-alien-type rustls-log-params (struct rustls-log-params))

(define-alien-type rustls-client-hello-userdata (* t))

(define-alien-type rustls-slice-u16 (struct rustls-slice-u16))

(define-alien-type rustls-client-hello (struct rustls-client-hello))

(define-alien-type rustls-certified-key (struct rustls-certified-key))

(define-alien-type rustls-session-store-userdata (* t))

(define-alien-type rustls-supported-ciphersuite (struct rustls-supported-ciphersuite))
