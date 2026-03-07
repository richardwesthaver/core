;;; crypto/hotp.lisp --- HMAC-Based One-Time Passwords

;; see https://github.com/bhyde/cl-one-time-passwords/hotp.lisp

;;; Code:
(in-package :cry/otp)

;;; HOTP
;; RFC 4226
(defvar *digits* 6)

(defvar *hmac-sha-mode* :sha3)

(defun hmac-sha-n (key-string counter)
  (loop
    with counter-bytes = (make-array 8 :element-type '(unsigned-byte 8))
    with hmac = (ironclad:make-hmac
                 (ironclad:hex-string-to-byte-array key-string)
                 *hmac-sha-mode*)
    finally
       (ironclad:update-hmac hmac counter-bytes)
       (return (ironclad:hmac-digest hmac))
    for i from 7 downto 0
    for offset from 0 by 8
    do (setf (aref counter-bytes i) (ldb (byte 8 offset) counter))))

(defun hotp-truncate (20-bytes)
  (flet ((dt (ht)
           (let* ((byte19 (aref ht 19))
                  (byte-offset (ldb (byte 4 0) byte19))
                  (result 0))
             (setf (ldb (byte 7 24) result) (aref ht byte-offset))
             (setf (ldb (byte 8 16) result) (aref ht (+ 1 byte-offset)))
             (setf (ldb (byte 8  8) result) (aref ht (+ 2 byte-offset)))
             (setf (ldb (byte 8  0) result) (aref ht (+ 3 byte-offset)))
             result)))
    (let ((sbits (dt 20-bytes)))
      (mod sbits
           (svref #(1 10 100 1000 10000 100000 1000000 10000000 100000000)
                  *digits*)))))

(defun hotp (key-string counter)
  (hotp-truncate (hmac-sha-n key-string counter)))

;;; TOTP
;; RFC 6238
(defconstant .unix-epoch-zero. 2208988800)
  ;; 00:00:00 UTC on 1 January 1970
  ;; (encode-universal-time 0 0 0 1 1 1970 0)
  ;; --> 2208988800

(defvar *time-zero* 0) ; aka the unix epoch zero
(defvar *time-step-in-seconds* 30)

(defmacro time-step (unix-time)
  `(floor (- ,unix-time *time-zero*) *time-step-in-seconds*))

(defun totp (key-hexstring &optional (offset 0) (time (- (get-universal-time) .unix-epoch-zero. offset)))
  (hotp key-hexstring (time-step time)))
