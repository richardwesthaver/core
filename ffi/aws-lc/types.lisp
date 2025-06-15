;;; rustls/types.lisp --- Rustls FFI Types

;;

;;; Code:
(in-package :aws-lc)

(define-alien-type asn1-string
    (struct asn1-string-st
      (length int)
      (type int)
      (data (* unsigned-char))
      (flags long)))

(define-alien-enum (v-asn1 int)
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

(define-opaque x509)

(define-alien-enum (err-lib int)
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
  :sm2 44)
