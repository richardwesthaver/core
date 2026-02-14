;;; types.lisp --- OpenSSL Foreign Types

;;

;;; Code:
(in-package :openssl)

(define-alien-type asn1-string
    (struct asn1-string-st
      (length int)
      (type int)
      (data (* unsigned-char))
      (flags long)))

(define-alien-enum (v-asn1 :type int)
  :universal #x00
  :application #x40
  :context-specific #x80
  :private #xc0
  :constructed #x20
  :primitive-tag #x1f
  :max-universal #xff
  :undef -1
  :other -3
  :any -4
  :eoc 0
  :boolean 1
  :integer 2
  :bit-string 3
  :octet-string 4
  :null 5
  :object 6
  :object-descriptor 7
  :external 8
  :real 9
  :enumerated 10
  :utf8string 12
  :sequence 16
  :set 17
  :numericstring 18
  :printablestring 19
  :t61string 20
  :teletexstring 20
  :videotexstring 21
  :ia5string 22
  :utftime 23
  :generalizedtime 24
  :graphicstring 25
  :iso64string 26
  :visiblestring 26
  :universalstring 28
  :bmpstring 30
  :neg #x100
  :neg-integer (logior 2 #x100)
  :neg-enumerated (logior 10 #x100))

;; base
(define-opaque asn1-item asn1-item-st)
(define-opaque asn1-object asn1-object-st)
(define-opaque asn1-pctx asn1-pctx-st)
(define-opaque asn1-bit-string asn1-string-st)
(define-opaque asn1-bmpstring asn1-string-st)
(define-opaque asn1-enumerated asn1-string-st)
(define-opaque asn1-generalizedtime asn1-string-st)
(define-opaque asn1-generalstring asn1-string-st)
(define-opaque asn1-ia5string asn1-string-st)
(define-opaque asn1-integer asn1-string-st)
(define-opaque asn1-octet-string asn1-string-st)
(define-opaque asn1-printablestring asn1-string-st)
(define-opaque asn1-string asn1-string-st)
(define-opaque asn1-t61string asn1-string-st)
(define-opaque asn1-time asn1-string-st)
(define-opaque asn1-universalstring asn1-string-st)
(define-opaque asn1-utctime asn1-string-st)
(define-opaque asn1-utf8string asn1-string-st)
(define-opaque asn1-visiblestring asn1-string-st)
(define-opaque asn1-type asn1-type-st)
(define-opaque authority-keyid authority-keyid-st)
(define-opaque basic-constraints basic-constraints-st)
(define-opaque dist-point dist-point-st)
(define-opaque dsa-sig dsa-sig-st)
(define-alien-type general-name 
  (struct general-name-st
    (type int)
    (data (* t))))
(define-opaque issuing-dist-point issuing-dist-point-st)
(define-opaque name-constraints name-constraints-st)
(define-opaque netscape-spkac netscape-spkac-st)
(define-opaque netscape-spki netscape-spki-st)
(define-opaque ripemd160-ctx ripemd160state-st)
(define-opaque x509-verify-param x509-verify-param-st)
(define-opaque x509-algor x509-algor-st)
(define-opaque x509-crl x509-crl-st)
(define-opaque x509-extension x509-extension-st)
(define-opaque x509-info x509-info-st)
(define-opaque x509-name-entry x509-name-entry-st)
(define-opaque x509-name x509-name-st)
(define-opaque x509-pubkey x509-pubkey-st)
(define-opaque x509-req x509-req-st)
(define-opaque x509-sig-info x509-sig-info-st)
(define-opaque x509-sig x509-sig-st)
(define-opaque bn-ctx bignum-ctx)
(define-opaque bignum bignum-st)
(define-alien-type bio-method 
    (struct bio-method-st
      (type int)
      (name (* t))
      (bwrite (* t))
      (bread (* t))
      (bputs (* t))
      (bgets (* t))
      (ctrl (* t))
      (create (* t))
      (destroy (* t))
      (callback-ctrl (* t))))
(define-alien-type bio 
  (struct bio-st
    (method (* t))
    (callback (* t))
    (cb-arg (* t))
    (init int)
    (shutdown int)
    (flags int)
    (retry-reason int)
    (num int)
    (ptr (* t))
    (next-bio (* t))
    (prev-bio (* t))
    (references int)
    (num-read unsigned-long)
    (num-write unsigned-long)
    (crypto-ex-data-stack (* t))
    (crypto-ex-data-dummy int)))

(define-opaque blake2b-ctx blake2b-state-st)
(define-opaque bn-gencb bn-gencb-st)
(define-opaque bn-mont-ctx bn-mont-ctx-st)
(define-opaque buf-mem buf-mem-st)
(define-opaque cast-key cast-key-st)
(define-opaque cbb cbb-st)
(define-opaque cbs cbs-st)
(define-opaque cmac-ctx cmac-ctx-st)
(define-opaque conf conf-st)
(define-opaque conf-value conf-value-st)
(define-opaque crypto-buffer-pool crypto-buffer-pool-st)
(define-opaque crypto-buffer crypto-buffer-st)
(define-opaque ctr-drbg-state ctr-drbg-state-st)
(define-opaque dh dh-st)
(define-opaque dsa dsa-st)
(define-opaque ec-group ec-group-st)
(define-opaque ec-key ec-key-st)
(define-opaque ec-point ec-point-st)
(define-opaque ec-key-method ec-key-method-st)
(define-opaque ecdsa-sig ecdsa-sig-st)
(define-opaque engine engine-st)
(define-opaque evp-md-ctx env-md-ctx-st)
(define-opaque evp-md env-md-st)
(define-opaque evp-aead evp-aead-st)
(define-opaque evp-aead-ctx evp-aead-ctx-st)
(define-opaque evp-cipher-ctx evp-cipher-ctx-st)
(define-opaque evp-cipher evp-cipher-st)
(define-opaque evp-encode-ctx evp-encode-ctx-st)
(define-opaque evp-hpke-aead evp-hpke-aead-st)
(define-opaque evp-hpke-ctx evp-hpke-ctx-st)
(define-opaque evp-hpke-kdf evp-hpke-kdf-st)
(define-opaque evp-hpke-kem evp-hpke-kem-st)
(define-opaque evp-hpke-key evp-hpke-key-st)
(define-opaque evp-kem evp-kem-st)
(define-opaque kem-key kem-key-st)
(define-opaque evp-pkey-ctx evp-pkey-ctx-st)
(define-opaque evp-pkey-asn1-method evp-pkey-asn1-method-st)
(define-opaque evp-pkey evp-pkey-st)
(define-opaque evp-pkey-ctx-signature-context-params evp-pkey-ctx-signature-context-params-st)
(define-opaque hmac-ctx hmac-ctx-st)
(define-opaque md4-ctx md4-state-st)
(define-opaque md5-ctx md5-state-st)
(define-opaque pqdsa-key pqdsa-key-st)
(define-opaque ocsp-req-ctx ocsp-req-ctx-st)
(define-opaque openssl-init-settings ossl-init-settings-st)
(define-opaque pkcs7-digest pkcs7-digest-st)
(define-opaque pkcs7-enc-content pkcs7-enc-content-st)
(define-opaque pkcs7-encrypt pkcs7-encrypt-st)
(define-opaque pkcs7-envelope pkcs7-envelope-st)
(define-opaque pkcs7-issuer-and-serial pkcs7-issuer-and-serial-st)
(define-opaque pkcs7-recip-info pkcs7-recip-info-st)
(define-opaque pkcs7-sign-envelope pkcs7-sign-envelope-st)
(define-opaque pkcs7-signed pkcs7-signed-st)
(define-opaque pkcs7-signer-info pkcs7-signer-info-st)
(define-opaque pkcs7 pkcs7-st)
(define-opaque pkcs12 pkcs12-st)
(define-opaque pkcs8-priv-key-info pkcs8-priv-key-info-st)
(define-opaque x509-pkey private-key-st)
(define-opaque rand-method rand-meth-st)
(define-opaque rc4-key rc4-key-st)
(define-opaque rsa-method rsa-meth-st)
(define-opaque rsassa-pss-params rsassa-pss-params-st)
(define-opaque rsa-pss-params rsa-pss-params-st)
(define-opaque rsa rsa-st)
(define-opaque sha256-ctx sha256-state-st)
(define-opaque sha512-ctx sha512-state-st)
(define-opaque sha-ctx sha-state-st)
(define-opaque spake2-ctx spake2-ctx-st)
(define-opaque srtp-protection-profile srtp-protection-profile-st)
(define-opaque ssl-cipher ssl-cipher-st)
(define-opaque ssl-ctx ssl-ctx-st)
(define-opaque ssl-client-hello ssl-early-callback-ctx)
(define-opaque ssl-ech-keys ssl-ech-keys-st)
(define-opaque ssl-method ssl-method-st)
(define-opaque ssl-private-key-method ssl-private-key-method-st)
(define-opaque ssl-quic-method ssl-quic-method-st)
(define-opaque ssl-session ssl-session-st)
(define-opaque ssl ssl-st)
(define-opaque ssl-ticket-aead-method ssl-ticket-aead-method-st)
(define-opaque err-fns st-err-fns)
(define-opaque trust-token trust-token-st)
(define-opaque trust-token-client trust-token-client-st)
(define-opaque trust-token-issuer trust-token-issuer-st)
(define-opaque trust-token-method trust-token-method-st)
(define-opaque x509v3-ctx v3-ext-ctx)
(define-opaque x509v3-ext-method v3-ext-method)
(define-opaque x509-attributes x509-attributes-st)
(define-opaque x509-lookup x509-lookup-st)
(define-opaque x509-lookup-method x509-lookup-methodst)
(define-opaque x509-object x509-object)
(define-opaque x509-revoked x509-revoked-st)
(define-opaque x509 x509-st)
(define-opaque x509-store-ctx x509-store-ctx-st)
(define-opaque x509-store x509-store-st)
(define-opaque x509-trust x509-trust-st)

(std:eval-always
  (define-alien-enum (err-lib)
    :none 1
    :sys 2
    :bn 3
    :rsa 4
    :dh 5
    :evp 6
    :buf 7
    :obj 8
    :pem 9
    :dsa 10
    :x509 11
    :asn1 12
    :conf 13
    :crypto 14
    :ec 15
    :ssl 16
    :bio 17
    :pkcs7 18
    :pkcs8 19
    :x509v3 20
    :rand 21
    :engine 22
    :ocsp 23
    :ui 24
    :comp 25
    :ecdsa 26
    :ecdh 27
    :hmac 28
    :digest 29
    :cipher 30
    :hkdf 31
    :trust-token 32
    :user 33
    :libs 34
    :pkcs12 35
    :dso 36
    :ossl-store 37
    :fips 38
    :cms 39
    :ts 40
    :ct 41
    :async 42
    :kdf 43
    :sm2 44))

(define-alien-enum (err-r)
  :sys-lib (err-lib :sys)
  :bn-lib (err-lib :bn)
  :rsa-lib (err-lib :rsa)
  :dh-lib (err-lib :dh)
  :evp-lib (err-lib :evp)
  :buf-lib (err-lib :buf)
  :obj-lib (err-lib :obj)
  :pem-lib (err-lib :pem)
  :dsa-lib (err-lib :dsa)
  :x509-lib (err-lib :x509)
  :asn1-lib (err-lib :asn1)
  :conf-lib (err-lib :conf)
  :crypto-lib (err-lib :crypto)
  :ec-lib (err-lib :ec)
  :ssl-lib (err-lib :ssl)
  :bio-lib (err-lib :bio)
  :pkcs7-lib (err-lib :pkcs7)
  :pkcs8-lib (err-lib :pkcs8)
  :x509v3-lib (err-lib :x509v3)
  :rand-lib (err-lib :rand)
  :dso-lib (err-lib :dso)
  :engine-lib (err-lib :engine)
  :ocsp-lib (err-lib :ocsp)
  :ui-lib (err-lib :ui)
  :comp-lib (err-lib :comp)
  :ecdsa-lib (err-lib :ecdsa)
  :ecdh-lib (err-lib :ecdh)
  ;; todo: where is this defined?
  ;;  :store-lib (err-lib :store)
  :fips-lib (err-lib :fips)
  :cms-lib (err-lib :cms)
  :ts-lib (err-lib :ts)
  :hmac-lib (err-lib :hmac)
  ;; TODO: where is this defined?
  ;; :jpake-lib (err-lib :jpake)
  :user-lib (err-lib :user)
  :digest-lib (err-lib :digest)
  :cipher-lib (err-lib :cipher)
  :hkdf-lib (err-lib :hkdf)
  :trust-token-lib (err-lib :trust-token)
  :fatal 64
  :malloc-failure (logior 1 64)
  :should-not-have-been-called (logior 2 64)
  :passed-null-parameter (logior 3 64)
  :internal-error (logior 4 64)
  :overflow (logior 5 64))
