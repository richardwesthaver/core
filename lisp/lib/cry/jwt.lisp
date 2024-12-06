;;; jwt.lisp --- JSON Web Tokens

;; This implementation is based on CLJWT: https://github.com/gschjetne/cljwt/blob/master/src/cljwt.lisp

;;; Commentary:

;; ref: https://en.wikipedia.org/wiki/JSON_Web_Token

;; rfc:7519

;; TODO 2024-06-30: ref: https://datatracker.ietf.org/doc/html/rfc7517

;; https://40ants.com/lisp-project-of-the-day/2020/05/0080-jose.html

;; https://medium.facilelogin.com/jwt-jws-and-jwe-for-not-so-dummies-b63310d201a3

;;; Code:
(in-package :cry/jwt)

;;; Conditions
(define-condition unsecured-token (error) ())

(define-condition invalid-hmac (error) ())

(define-condition unsupported-algorithm (error)
  ((algorithm :initarg :algorithm :reader algorithm))
  (:report (lambda (condition stream)
             (format stream "Algorithm \"~A\" not supported"
                     (algorithm condition)))))

(define-condition invalid-time (error)
  ((delta :initarg :delta :reader time-delta))
  (:report (lambda (condition stream)
             (format stream "Token ~A. ~D seconds off."
                     (typecase condition
                       (expired "has expired")
                       (not-yet-valid "is not yet valid"))
                     (time-delta condition)))))

(define-condition expired (invalid-time) ())

(define-condition not-yet-valid (invalid-time) ())

(defmacro bind-hash-tables (bindings &body body)
  `(let ,(loop for binding in bindings collect
              (list (car binding)
                    `(etypecase ,(cadr binding)
                       (hash-table ,(cadr binding))
                       (list (plist-hash-table ,(cadr binding)
                                               :test #'equal)))))
     ,@body))

(defmacro add-claims (hash &rest claims)
  `(progn ,@(loop for (key value) on claims by #'cddr collect
                 `(when ,value
                    (setf (gethash ,key ,hash) ,value)))))

(defun to-unix-time (time)
  "Convert universal time to New Jersey time"
  (when time (- time (encode-universal-time 0 0 0 1 1 1970 0))))

(defun from-unix-time (time)
  "Convert New Jersey time to universal time"
  (when time (+ time (encode-universal-time 0 0 0 1 1 1970 0))))

(defun base64-encode (input)
  "Takes a string or octets, returns an unpadded URI-encoded Base64 string."
  (etypecase input
    (string (base64-encode (sb-ext:string-to-octets input :external-format :utf-8)))
    ((simple-array (unsigned-byte 8))
     (with-output-to-string (out)
       (with-input-from-string (in (dat/base64:usb8-array-to-base64-string input :uri t))
         (loop for character = (read-char in nil)
               while character do
                 ;; CL-BASE64 always uses padding, which must be removed.
                 (unless (eq character #\.)
                   (write-char character out))))))))

(defun base64-decode (base-64-string)
  "Takes a base64-uri string and return an array of octets"
  (dat/base64:base64-string-to-usb8-array
   ;; Re-pad the string, or CL-BASE64 will get confused
   (concatenate 'string
                base-64-string
                (make-array (rem (length base-64-string) 4)
                            :element-type 'character
                            :initial-element #\.))
   :uri t))

(defun issue (claims &key algorithm secret issuer subject audience
                       expiration not-before issued-at id more-header)
  "Encodes and returns a JSON Web Token. Times are in universal-time,
number of seconds from 1900-01-01 00:00:00"
  (bind-hash-tables ((claimset claims)
                     (header more-header))
    ;; Add registered claims to the claims hash table
    (add-claims claimset
                "iss" issuer
                "sub" subject
                "aud" audience
                "exp" (to-unix-time expiration)
                "nbf" (to-unix-time not-before)
                "iat" (to-unix-time issued-at)
                "jti" id)
    ;; Add type and algorithm to the header hash table
    (add-claims header
                "typ" "JWT"
                "alg" (ecase algorithm
                        (:none "none")
                        (:hs256 "HS256")))
    ;; Prepare JSON
    (let ((header-string (base64-encode
                          (with-output-to-string (s)
                            (dat/json:json-encode header s))))
          (claims-string (base64-encode
                          (with-output-to-string (s)
                            (dat/json:json-encode claimset s)))))
      ;; Assemble and, if applicable, sign the JWT
      (format nil "~A.~A.~@[~A~]"
              header-string
              claims-string
              (when (eq algorithm :hs256)
                (HS256-digest header-string
                              claims-string
                              secret))))))

(defun hs256-digest (header-string claims-string secret)
  "Takes header and claims in Base64, secret as a string or octets,
returns the digest, in Base64"
  (base64-encode
   (ironclad:hmac-digest
    (ironclad:update-hmac
     (ironclad:make-hmac (etypecase secret
                  ((simple-array (unsigned-byte 8))
                   secret)
                  (string
                   (sb-ext:string-to-octets secret
                                     :external-format :utf-8)))
                'ironclad:SHA256)
     (concatenate '(vector (unsigned-byte 8))
                  (sb-ext:string-to-octets
                   header-string)
                  #(46) ; ASCII period (.)
                  (sb-ext:string-to-octets
                   claims-string))))))

(defun compare-hs256-digest (header-string claims-string
                             secret reported-digest)
  "Takes header and claims in Base64, secret as a string or octets, and a digest in Base64 to compare with. Signals an error if there is a mismatch."
  (let ((computed-digest
         (hs256-digest header-string
                       claims-string
                       secret)))
    (unless (equalp computed-digest
                   reported-digest)
      (cerror "Continue anyway" 'invalid-hmac
             :reported-digest reported-digest
             :computed-digest computed-digest))))

(defun jwt-decode (jwt-string &key secret fail-if-unsecured)
  "Decodes and verifies a JSON Web Token. Returns two hash tables,
token claims and token header"
  (destructuring-bind (header-string claims-string digest-string)
      (split-sequence #\. jwt-string)
    (let* ((header-hash (dat/json:json-decode
                         (sb-ext:octets-to-string
                          (base64-decode
                           header-string)
                          :external-format :utf-8)))
           (claims-hash (json-decode
                         (sb-ext:octets-to-string
                          (base64-decode
                           claims-string)
                          :external-format :utf-8)))
           (algorithm (dat/json:json-getf header-hash "alg")))
      ;; Verify HMAC
      (cond ((equal algorithm "HS256") 
             (compare-HS256-digest header-string
                                   claims-string
                                   secret
                                   digest-string))
            ((and (or (null algorithm) (equal algorithm "none")) fail-if-unsecured)
             (cerror "Continue anyway" 'unsecured-token))
            (t (cerror "Continue anyway" 'unsupported-algorithm
                       :algorithm algorithm)))
      ;; Verify timestamps
      (let ((expires (from-unix-time (json-getf claims-hash "exp")))
            (not-before (from-unix-time (json-getf claims-hash "nbf")))
            (current-time (get-universal-time)))
        (when (and expires (> current-time expires))
          (cerror "Continue anyway" 'expired :delta (- current-time expires)))
        (when (and not-before (< current-time not-before))
          (cerror "Continue anyway" 'not-yet-valid :delta (- current-time not-before))))
      ;; Return json objects
      (values claims-hash header-hash))))
