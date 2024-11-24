;;; b3.lisp --- BLAKE3 Hasher

;; 

;;; Code:
(in-package :cry/b3)

(defun b3hash (in &optional (len +blake3-out-len+))
  "Hash the sequence IN using blake3 returning an OCTET-VECTOR of length LEN."
  (with-static-vector (out len)
    (let ((output (static-vector-pointer out)))
      (with-blake3-hasher h
        (with-alien ((input (* unsigned-char) (octets-to-alien in)))
          (blake3-hasher-init (addr h))
          (blake3-hasher-update (addr h) (addr input) (length in))
          (blake3-hasher-finalize (addr h) output len))))
    out))

(defun b3sum () (nyi!))
