(defpackage :cry/tests
  (:use :rt :std :cl 
    :cry/jwt :cry/keyring :cry/authinfo :cry-int :cry/otp :cry/crc64 
    :cry/password :cry/drm :cry/tls :config :net/tcp)
  (:shadowing-import-from :rt :random-bytes))

(in-package :cry/tests)

(defsuite :cry)
(in-suite :cry)

(deftest hotp ()
  (is (integerp (hotp "1234" 100))))

(deftest totp ()
  (is (integerp (totp "1234"))))

(deftest crc64 ()
  (init-crc64 42)
  (is (integerp (crc64-sequence "aaaaaaaaaaaaaaaaaaaaaaa"))))

(deftest jwt ()
  ;; https://jwt.io/#debugger-io
  (multiple-value-bind (claims header)
      (cry/jwt:jwt-decode "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c" :secret "your-256-bit-secret")
    (istype 'dat/json:json-object claims)
    (istype 'dat/json:json-object header)))

(deftest keyring ()
  (keyutils:load-keyutils)
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
  (istype 'cry/ssh:ssh-config (make-config :ssh :path (cry/ssh:system-ssh-config-file)))
  (istype 'cry/ssh:ssh-config (make-config :ssh :path (cry/ssh:user-ssh-config-file))))

(deftest gpg ()
  (istype 'cry/gpg:gpg-agent-config (make-config :gpg-agent)))

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

