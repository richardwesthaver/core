;;; b3.lisp --- BLAKE3 Hasher

;; 

;;; Code:
(in-package :cry/b3)

(defun b3hash (in &optional (len +blake3-out-len+))
  "Hash the sequence IN using blake3 returning an OCTET-VECTOR of length LEN."
    (let ((out (make-octets len)))
      (with-blake3-hasher h
        (with-alien ((input (* unsigned-char) (octets-to-alien in))
                     (output (* unsigned-char) (make-alien unsigned-char len)))
          (blake3-hasher-update (addr h) input (length in))
          (blake3-hasher-finalize (addr h) output len)
          (clone-octets-from-alien output out len)
          out))))

(defun b3hash-string (in &key (length +blake3-out-len+) (hex t))
  "Hash a string using BLAKE3. When HEX is T (the default) return a hex-encoded
string instead of octets."
  (let ((hash (b3hash (sb-ext:string-to-octets in) length)))
    (if hex
        (octet-vector-to-hex-string hash)
        hash)))
  
(defun b3sum (path &key (hex t))
  (with-open-file (f path :element-type 'octet)
    (let ((out (make-octets (file-length f))))
      (read-sequence out f)
      (let ((hash (b3hash out)))
        (if hex
            (octet-vector-to-hex-string hash)
            hash)))))
