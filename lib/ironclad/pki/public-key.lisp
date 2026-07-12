;;; public-key.lisp --- implementation of common public key components

;;; Code:
(in-package :crypto)

(defun list-all-key-pair-kinds ()
  (copy-list '(:curve25519 :curve448 :dsa :ed25519 :ed448 :elgamal
               :rsa :secp256k1 :secp256r1 :secp384r1 :secp521r1)))

;;; class definitions
(defclass discrete-logarithm-group ()
  ((p :initarg :p :reader group-pval)
   (q :initarg :q :reader group-qval)
   (g :initarg :g :reader group-gval)))


;;; Special variable to force the signature nonce during tests instead of
;;; generating a random one.
(defparameter *signature-nonce-for-test* nil)
