;;; openssl.lisp --- OpenSSL Alien Routines

;;

;;; Code:
(in-package :openssl)

;;;_. ASN1/X509
;; TODO
(defar ("ASN1_STRING_data" asn1-string-data) (* char)
  (str (* t)))

(defar ("ASN1_STRING_length" asn1-string-length) int
  (str (* t)))

(defar ("ASN1_STRING_type" asn1-string-type) int
  (str (* t)))

(defar ("ASN1_STRING_free" asn1-string-free) void
  (str (* t)))

(defar ("ASN1_TIME_check" asn1-time-check) int
  (time (* t)))

(defar ("ASN1_UTCTIME_check" asn1-utctime-check) int
  (time (* t)))

(defar ("d2i_X509" d2i-x509) (* x509)
  (out (* (* x509)))
  (inp (* (* unsigned-char)))
  (len long))

(defar ("X509_digest" x509-digest) int
  (cert (* t))
  (type (* t))
  (buf (* t))
  (*len (* int)))

(defar ("PEM_write_bio_X509" pem-write-bio-x509) int
  (bio (* t))
  (x509 (* t)))

(defar ("PEM_read_bio_X509" pem-read-bio-x509) (* t)
  ;; all args are pointers in fact, but they are NULL anyway
  (bio (* t))
  (x509 int)
  (callback int)
  (passphrase int))

(defar ("X509_free" x509-free) void
  (x509 (* x509)))

(defar ("X509_NAME_oneline" x509-name-oneline) (* t)
  (x509-name (* t))
  (buf (* t))
  (size int))

(defar ("X509_NAME_get_index_by_NID" x509-name-get-index-by-nid) int
  (name (* t))
  (nid int)
  (lastpos int))

(defar ("X509_NAME_get_entry" x509-name-get-entry) (* t)
  (name (* t))
  (log int))

(defar ("X509_NAME_ENTRY_get_data" x509-name-entry-get-data) (* t)
  (name-entry (* t)))

(defar ("X509_get_issuer_name" x509-get-issuer-name) (* x509-name)
  (x509 (* x509)))

(defar ("X509_get_subject_name" x509-get-subject-name) (* x509-name)
  (x509 (* x509)))

(defar ("X509_get0_notBefore" x509-get0-not-before) (* asn1-time)
  (x509 (* x509)))

(defar ("X509_get0_notAfter" x509-get0-not-after) (* asn1-time)
  (x509 (* x509)))

(defar ("X509_get_ext_d2i" x509-get-ext-d2i) (* t)
  (cert (* t))
  (nid int)
  (crit (* t))
  (idx (* t)))

(defar ("X509_STORE_CTX_get_error" x509-store-ctx-get-error) int
  (ctx (* ssl-ctx)))

;;; Errors
(defar ("ERR_get_error" err-get-error) unsigned-int)

(defar ("ERR_error_string" err-error-string) c-string
  (e unsigned-int)
  (buf (* char)))

(defar ("ERR_new" err-new) void)
(defar ("ERR_set_debug" err-set-debug) void
  (file c-string)
  (line int)
  (func c-string))

;; ERR_set_error (variadic)

(defar ("ERR_get_next_error_library" err-get-next-error-library) int)

;; (defar ("ERR_add_error_data" err-add-error-data) void
;;   (num int) &rest)

;; (define-alien-routine ("ERR_add_error_data" err-add-error-data) void (num int) &rest)

(defar ("ERR_add_error_txt" err-add-error-txt) void
  (sep c-string)
  (txt c-string))

(defar ("ERR_print_errors" err-print-errors) void
  (bio (* t)))

;;;_. SSL
(defar ("SSL_set_cipher_list" ssl-set-cipher-list) int
  (ssl (* ssl))
  (str c-string))

(defar ("SSL_set_ciphersuites" ssl-set-ciphersuites) int
  (ssl (* ssl))
  (str c-string))

(defar ("SSL_use_RSAPrivateKey_file" ssl-use-rsa-privatekey-file) int
  (ssl (* ssl))
  (str c-string)
  (type int)) ;; pem or asn1

(defar ("SSL_CTX_use_RSAPrivateKey_file" ssl-ctx-use-rsa-privatekey-file) int
  (ctx (* ssl-ctx))
  (type int))

(defar ("SSL_use_PrivateKey_file" ssl-use-privatekey-file) int
  (ssl (* ssl))
  (str c-string)
  (type int))

(defar ("SSL_CTX_use_PrivateKey_file" ssl-ctx-use-privatekey-file) int
  (ctx (* ssl-ctx))
  (file c-string)
  (type int))

(defar ("SSL_use_certificate_file" ssl-use-certificate-file) int
  (ssl (* ssl))
  (str c-string)
  (type int))

(defar ("SSL_CTX_ctrl" ssl-ctx-ctrl) long
  (ctx (* ssl-ctx))
  (cmd int)
  (larg unsigned-long)
  (parg (* t)))

(defar ("SSL_ctrl" ssl-ctrl) long
  (ssl (* t))
  (cmd int)
  (larg long)
  (parg (* t)))

(defar ("SSL_CTX_set_options" ssl-ctx-set-options) long
  (ctx (* t))
  (options long))

(defar ("SSL_CTX_set_cipher_list" ssl-ctx-set-cipher-list) int
  (ctx (* t))
  (ciphers c-string))

(defar ("SSL_CTX_set_ciphersuites" ssl-ctx-set-ciphersuites) int
  (ctx (* t))
  (ciphers c-string))

(defar ("SSL_CTX_use_certificate_chain_file" ssl-ctx-use-certificate-chain-file) int
  (ctx (* ssl-ctx))
  (str c-string))

(defar ("SSL_CTX_load_verify_locations" ssl-ctx-load-verify-locations) int
  (ctx (* ssl-ctx))
  (cafile c-string)
  (capath c-string))

(defar ("SSL_CTX_set_client_CA_list" ssl-ctx-set-client-ca-list) void
  (ctx (* ssl-ctx))
  (lst (* t)))

(defar ("SSL_load_client_CA_file" ssl-load-client-ca-list) (* t)
  (file c-string))

(defar ("SSL_CTX_set_default_passwd_cb" ssl-ctx-set-default-passwd-cb) void
  (ctx (* ssl-ctx))
  (pem-password-cb (* (function (signed 32) (* (signed 8)) (signed 32) (signed 32) (* t)))))

(defar ("RAND_seed" rand-seed) void
  (buf (* t))
  (num int))

(defar ("RAND_bytes" rand-bytes) int
  (buf (* t))
  (num int))

(defar ("SSL_CTX_set_verify_depth" ssl-ctx-set-verify-depth) void
  (ctx (* t))
  (depth int))

(defar ("SSL_CTX_set_verify" ssl-ctx-set-verify) void
  (ctx (* t))
  (mode int)
  (verify-callback (* t)))

(defar ("SSL_get_verify_result" ssl-get-verify-result) long
  (ssl (* t)))

(defar ("SSL_get_peer_certificate" ssl-get-peer-certificate) (* t)
  (ssl (* t)))

(defar ("SSL_get1_peer_certificate" ssl-get1-peer-certificate) (* x509)
  (ssl (* t)))

(defconstant +err-error-string-buf-len+ 120)

(defar ("PEM_read" pem-read) int
  (fp (* int)) 
  (name (* c-string)) 
  (header (* c-string)) 
  (data (* (* unsigned-char))) 
  (len (* long)))

(defar ("PEM_write" pem-write) int
  (fp (* int))
  (name c-string)
  (header c-string)
  (data (* unsigned-char))
  (len long))

(defar ("PEM_def_callback" pem-def-callback) int
  (buf (* char)) 
  (size int) 
  (rwflag int) 
  (userdata (* t)))

(defar ("SSL_get_version" ssl-get-version) c-string
  (ssl (* t)))

(defar ("TLSv1_client_method" tlsv1-client-method) (* ssl-method))
(defar ("TLSv1_server_method" tlsv1-server-method) (* ssl-method))
(defar ("TLSv1_method" tlsv1-method) (* ssl-method))

(defar ("OpenSSL_version_num" openssl-version-num) long)

(defar ("TLSv1_1_client_method" tlsv1-1-client-method) (* ssl-method))
(defar ("TLSv1_1_server_method" tlsv1-1-server-method) (* ssl-method))
(defar ("TLSv1_1_method" tlsv1-1-method) (* ssl-method))
(defar ("TLSv1_2_client_method" tlsv1-2-client-method) (* ssl-method))
(defar ("TLSv1_2_server_method" tlsv1-2-server-method) (* ssl-method))
(defar ("TLSv1_2_method" tlsv1-2-method) (* ssl-method))
(defar ("TLS_method" tls-method) (* ssl-method))

(defar ("SSL_CTX_new" ssl-ctx-new) (* ssl-ctx)
  (method (* ssl-method)))
(defar ("SSL_new" ssl-new) (* ssl)
  (method (* ssl-ctx)))
(defar ("SSL_get_fd" ssl-get-fd) int
  (ssl (* ssl)))
(defar ("SSL_set_fd" ssl-set-fd) int
  (ssl (* ssl))
  (fd int))
(defar ("SSL_set_bio" ssl-set-bio) void
  (ssl (* ssl))
  (rbio (* t))
  (wbio (* t)))
(defar ("SSL_get_error" ssl-get-error) int
  (ssl (* ssl))
  (ret int))
(defar ("SSL_set_connect_state" ssl-set-connect-state) void
  (ssl (* ssl)))
(defar ("SSL_set_accept_state" ssl-set-accept-state) void
  (ssl (* ssl)))
(defar ("SSL_connect" ssl-connect) int
  (ssl (* ssl)))
(defar ("SSL_accept" ssl-accept) int
  (ssl (* ssl)))
(defar ("SSL_write" ssl-write) int
  (ssl (* ssl))
  (buf (* t))
  (num int))
(defar ("SSL_read" ssl-read) int
  (ssl (* ssl))
  (buf (* t))
  (num int))
(defar ("SSL_shutdown" ssl-shutdown) int
  (ssl (* ssl)))
(defar ("SSL_free" ssl-free) void
  (ssl (* ssl)))
(defar ("SSL_CTX_free" ssl-ctx-free) void
  (ctx (* ssl-ctx)))
(defar ("SSL_set_alpn_protos" ssl-set-alpn-protos) int
  (ssl (* t))
  (text c-string)
  (len int))
(defar ("SSL_get0_alpn_selected" ssl-get0-alpn-selected) void
  (ssl (* t))
  (text (* c-string))
  (len (* int)))

(defar ("SSL_CTX_set_default_verify_paths" ssl-ctx-set-default-verify-paths) int
  (ctx (* t)))
(defar ("SSL_CTX_set_default_verify_dir" ssl-ctx-set-default-verify-dir) int    
  (ctx (* t)))
(defar ("SSL_CTX_set_default_verify_file" ssl-ctx-set-default-verify-file) int    
  (ctx (* t)))

(defar ("RSA_generate_key" rsa-generate-key) (* t)
  (num int)
  (e unsigned-long)
  (callback (* t))
  (opt (* t)))
(defar ("RSA_free" rsa-free) void
  (rsa (* t)))

;;;_. BIO
(defar ("BIO_ctrl" bio-ctrl) long
  (bio (* t))
  (cmd int)
  (larg long)
  (part (* t)))
(defar ("BIO_new_socket" bio-new-socket) (* t)
  (fd int)
  (close-flag int))
(defar ("BIO_new" bio-new) (* t)
  (method (* t)))
(defar ("BIO_free" bio-free) void
  (method (* t)))
(defar ("BIO_get_new_index" bio-get-new-index) int)
(defar ("BIO_meth_new" bio-meth-new) (* t)
  (type int)
  (name c-string))
(defar ("BIO_meth_set_puts" bio-meth-set-puts) int
  (meth (* t))
  (puts (* t)))
(defar ("BIO_meth_set_write" bio-meth-set-write) int
  (meth (* t))
  (puts (* t)))
(defar ("BIO_meth_set_read" bio-meth-set-read) int
  (meth (* t))
  (read (* t)))
(defar ("BIO_meth_set_gets" bio-meth-set-gets) int
  (meth (* t))
  (read (* t)))
(defar ("BIO_meth_set_create" bio-meth-set-create) int
  (meth (* t))
  (read (* t)))
(defar ("BIO_meth_set_destroy" bio-meth-set-destroy) int
  (meth (* t))
  (read (* t)))
(defar ("BIO_meth_set_ctrl" bio-meth-set-ctrl) int
  (meth (* t))
  (read (* t)))
(defar ("BIO_set_init" bio-set-init) int
  (meth (* t))
  (value int))
(defar ("BIO_set_flags" bio-set-flags) int
  (meth (* t))
  (value int))
(defar ("BIO_clear_flags" bio-clear-flags) int
  (meth (* t))
  (value int))
(defar ("BIO_test_flags" bio-test-flags) int
  (meth (* t))
  (value int))

(defar ("OPENSSL_sk_value" openssl-sk-value) (* t)
  (stack (* t))
  (index int))

(defar ("OPENSSL_sk_num" openssl-sk-num) int
  (stack (* t)))

(defar ("GENERAL_NAMES_free" general-names-free) void
  (general-names (* t)))

(defar ("EVP_get_digestbyname" evp-get-digest-by-name) (* t)
  (name c-string))

(defar ("EVP_MD_get_size" evp-md-get-size) int
  (evp (* t)))
