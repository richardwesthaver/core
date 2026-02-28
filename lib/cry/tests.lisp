(cl:defpackage :cry/tests
  (:use :rt :std :cl 
   :cry-int :cry/hotp :cry/totp :cry/crc64 
   :cry/jwt :cry/b3 :cry/keyring :cry/authinfo 
   :cry/password :cry/drm :cry/tls :config :net/tcp)
  (:shadowing-import-from :rt :random-bytes))

(in-package :cry/tests)

(defsuite :cry)
(in-suite :cry)

(keyutils:load-keyutils)

(deftest hotp ()
  (is (integerp (hotp "1234" 100))))

(deftest totp ()
  (is (integerp (totp "1234"))))

(deftest crc64 ()
  (init-crc64 42)
  (is (integerp (crc64-sequence "aaaaaaaaaaaaaaaaaaaaaaa"))))

(deftest b3 ()
  (blake3:load-blake3)
  (isequal
   (b3hash-string "1234")
   (b3hash-string "1234")))

(deftest jwt ()
  ;; https://jwt.io/#debugger-io
  (multiple-value-bind (claims header)
      (cry/jwt:jwt-decode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c" :secret "your-256-bit-secret")
    (istype 'dat/json:json-object claims)
    (istype 'dat/json:json-object header)))

(deftest keyring ()
  (let ((kr (make-keyring :user)))
    (istype 'keyring kr)
    (iszero (clear-keys kr))))

(defvar *test-authinfo* "machine foo login bar port abc password hackme")

(deftest authinfo ()
  (when-let ((f (probe-file "~/.authinfo")))
    (istype 'authinfo (deserialize f :authinfo)))
  (deserialize *test-authinfo* :authinfo))

(deftest password ()
  (let ((secret "hackme")
        (salt (sb-ext:string-to-octets "pepper")))
    (istype 'string (make-password-hash secret salt))
    (let ((pw (make-instance 'password :password secret)))
      (istype 'password pw)
      (is (auth pw secret)))))

(deftest ssh ()
  ;; config
  (istype 'cry/ssh:ssh-config (make-config :ssh :path (cry/ssh:system-ssh-config-file))))

(deftest gpg ()
  (istype 'cry/gpg:gpg-agent-config (make-config :gpg-agent)))

(defun checksum-bench ()
  (blake3:load-blake3)
  (labels ((.md5sum () (crypto:digest-file (crypto:make-digest :md5) *load-truename*))
           (.sha1sum () (crypto:digest-file (crypto:make-digest :sha1) *load-truename*))
           (.sha256sum () (crypto:digest-file (crypto:make-digest :sha256) *load-truename*))
           (.sha512sum () (crypto:digest-file (crypto:make-digest :sha512) *load-truename*))
           (.b3sum () (b3sum *load-truename* :hex nil))
           (.crc64sum () (crc64-file *load-truename*)))
    (init-crc64 +improved-polynomial+)
    (let ((n 1000))
      (time (dotimes (i n) (.sha1sum))) ;; 20 bytes
      (time (dotimes (i n) (.sha256sum))) ;; 32 bytes
      (time 
       (sb-sprof:with-profiling (:report :graph)
         (dotimes (i n) (.b3sum)))) ;; 32 bytes ; incredibly slow
      (time (dotimes (i n) (.sha512sum))) ;; 64 bytes
      (time (dotimes (i n) (.md5sum))) ;; 16 bytes
      (time (dotimes (i n) (.crc64sum)));; 8 bytes
      )))

(deftest tls ()
  (reset :ssl)
  (ensure-ssl)
  (is (ssl-initialized-p))
  (let ((ctx (make-ssl-context :verify-mode openssl::+ssl-verify-none+)))
    (with-global-context (ctx :auto-free-p t)
      (let ((sock (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
        (sb-bsd-sockets:socket-connect sock (sb-bsd-sockets:make-inet-address (net/dns:resolve "compiler.company")) 443)
        (let* ((stream (sb-bsd-sockets:socket-make-stream sock :output t))
               (ssl-stream (make-ssl-client-stream stream :hostname "compiler.company")))
          (is (open-stream-p ssl-stream))
          (istype 'cry/tls::ssl-stream ssl-stream)
          (finish-output ssl-stream)
          (close stream))))))

