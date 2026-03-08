;;; password.lisp --- Reasonably Safe User Passwords

;; for even more reasonably safe password hashes see ironclad/src/kdf/password-hash.lisp

;;; Code:
(in-package :cry/password)

(defvar *password-digest* 'sha256)

(defclass password ()
  ((hash :initarg :hash
         :reader password-hash)
   (salt :initarg :salt
         :initform
         ;; Use /dev/urandom seed for portability.
         (let ((ironclad:*prng* (ironclad:make-prng :fortuna :seed :urandom)))
           (ironclad:make-random-salt 20))
         :reader password-salt)))

(defun make-password-hash (password salt)
  (ironclad:pbkdf2-hash-password password :salt salt)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    *password-digest*
    (concatenate '(vector (unsigned-byte 8))
                 (sb-ext:string-to-octets password)
                 salt))))

(defgeneric (setf password) (password auth)
  (:method (password (object password))
    (let ((hash (make-password-hash 
                 password
                 (slot-value object 'salt))))
      (setf (slot-value object 'hash) hash))))

(defmethod initialize-instance :after ((object password) &rest initargs
                                                         &key password &allow-other-keys)
  (declare (ignore initargs))
  (when password
    (setf (password object) password)))
