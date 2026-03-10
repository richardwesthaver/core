;;; swap-bytes.lisp --- Fast translation between local and network order

;; Base on SWAP-BYTES by sionescu/stassats

;;; Code:
(in-package :io/swap-bytes)

(defknown swap-bytes-16 ((unsigned-byte 16)) (unsigned-byte 16)
    (movable foldable flushable)
  :overwrite-fndb-silently t)

(defknown swap-bytes-32 ((unsigned-byte 32)) (unsigned-byte 32)
    (movable foldable flushable)
  :overwrite-fndb-silently t)

#+x86-64
(defknown swap-bytes-64 ((unsigned-byte 64)) (unsigned-byte 64)
    (movable foldable flushable)
  :overwrite-fndb-silently t)

#+x86
(define-vop (16bit-swap-bytes)
  (:policy :fast-safe)
  (:translate swap-bytes-16)
  (:note "inline 16-bit swap bytes")
  (:args (integer :scs (sb-vm::unsigned-reg) :target eax))
  (:arg-types sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::unsigned-reg
               :offset sb-vm::eax-offset :target res
               :from :eval)
              eax)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 2
    (move eax integer)
    (inst xchg sb-vm::al-tn sb-vm::ah-tn)
    (move res eax)))

#+x86-64
(define-vop (16bit-swap-bytes)
  (:policy :fast-safe)
  (:translate swap-bytes-16)
  (:note "inline 16-bit swap bytes")
  (:args (integer :scs (sb-vm::unsigned-reg) :target res))
  (:arg-types sb-vm::unsigned-num)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 2
    (move res integer)
    #+#1=#.(cl:if (cl:ignore-errors (sb-ext:assert-version->= 1 5 9 17) t) '(and) '(or))
    (inst rol :word res 8)
    #-#1#
    (inst rol (sb-vm::reg-in-size res :word) 8)))

#+x86
(define-vop (32bit-swap-bytes)
  (:policy :fast-safe)
  (:translate swap-bytes-32)
  (:note "inline 32-bit swap bytes")
  (:args (integer :scs (sb-vm::unsigned-reg) :target res))
  (:arg-types sb-vm::unsigned-num)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 2
    (move res integer)
    (inst bswap res)))

#+x86-64
(define-vop (32bit-swap-bytes)
  (:policy :fast-safe)
  (:translate swap-bytes-32)
  (:note "inline 32-bit swap bytes")
  (:args (integer :scs (sb-vm::unsigned-reg) :target res))
  (:arg-types sb-vm::unsigned-num)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 2
    (move res integer)
    #+#1=#.(cl:if (cl:ignore-errors (sb-ext:assert-version->= 1 5 9 17) t) '(and) '(or))
    (inst bswap :dword res)
    #-#1#
    (inst bswap (sb-vm::reg-in-size res :dword))))

#+x86-64
(define-vop (64bit-swap-bytes)
  (:policy :fast-safe)
  (:translate swap-bytes-64)
  (:note "inline 64-bit swap bytes")
  (:args (integer :scs (sb-vm::unsigned-reg) :target res))
  (:arg-types sb-vm::unsigned-num)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 2
    (move res integer)
    (inst bswap res)))

(defun swap-bytes-16 (integer)
  (declare (type (unsigned-byte 16) integer))
  (swap-bytes-16 integer))

(defun swap-bytes-32 (integer)
  (declare (type (unsigned-byte 32) integer))
  (swap-bytes-32 integer))

#+x86
(defun swap-bytes-64 (integer)
  (declare (type (unsigned-byte 64) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  (logior
   (swap-bytes-32 (ldb (byte 32 32) integer))
   (ash (swap-bytes-32 (ldb (byte 32 0) integer)) 32)))

#+x86-64
(defun swap-bytes-64 (integer)
  (declare (type (unsigned-byte 64) integer))
  (swap-bytes-64 integer))

(declaim (inline htons ntohs htonl ntohl htonq ntohq))

(defun htons (integer)
  "Convert (unsigned-byte 16) from host order(little- or big-endian)
to network order(always big-endian)."
  (declare (type (unsigned-byte 16) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-16 integer)
  #+big-endian    integer)

(defun ntohs (integer)
  "Convert (unsigned-byte 16) from network order(always big-endian) to
host order(little- or big-endian)."
  (declare (type (unsigned-byte 16) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-16 integer)
  #+big-endian    integer)

(defun htonl (integer)
  "Convert (unsigned-byte 32) from host order(little- or big-endian)
to network order(always big-endian)."
  (declare (type (unsigned-byte 32) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-32 integer)
  #+big-endian    integer)

(defun ntohl (integer)
  "Convert (unsigned-byte 32) from network order(always big-endian) to
host order(little- or big-endian)."
  (declare (type (unsigned-byte 32) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-32 integer)
  #+big-endian    integer)

(defun htonq (integer)
  "Convert (unsigned-byte 64) from host order(little- or big-endian)
to network order(always big-endian)."
  (declare (type (unsigned-byte 64) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-64 integer)
  #+big-endian    integer)

(defun ntohq (integer)
  "Convert (unsigned-byte 64) from network order(always big-endian) to
host order(little- or big-endian)."
  (declare (type (unsigned-byte 64) integer)
           (optimize (speed 3) (safety 0) (debug 0)))
  #+little-endian (swap-bytes-64 integer)
  #+big-endian    integer)

(deftype endianness ()
  '(member :big-endian :little-endian))

(deftype endianness-designator ()
  '(member :big-endian :little-endian :network :local))

(defconstant +endianness+
  #+big-endian    :big-endian
  #+little-endian :little-endian)

(defun endianness (endianness)
  (check-type endianness endianness-designator)
  (case endianness
    (:local   +endianness+)
    (:network :big-endian)
    (t        endianness)))

(defun find-swap-byte-function (&key size from (to :local))
  (let ((from (endianness from))
        (to   (endianness to)))
    (if (eql from to)
        'identity
        (ecase size
          (1 'identity)
          (2 'swap-bytes-16)
          (4 'swap-bytes-32)
          (8 'swap-bytes-64)))))
