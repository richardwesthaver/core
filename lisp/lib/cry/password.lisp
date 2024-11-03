;;; password.lisp --- Reasonably Safe User Passwords

;; 

;;; Code:
(in-package :cry/password)

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
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
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

(defmethod auth (object password)
  (string= (password-hash object)
           (make-password-hash password
                               (password-salt object))))
