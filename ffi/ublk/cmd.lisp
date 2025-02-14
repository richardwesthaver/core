;;; cmd.lisp --- Ublk Commands

;; 

;;; Code:
(in-package :ublk)
(define-alien-type ublksrv-ctrl-cmd
    (struct ublksrv-ctrl-cmd
            (dev-id (unsigned 32))
            (queue-id (unsigned 16))
            (len (unsigned 16))
            (addr (unsigned 64))
            (data (array (unsigned 64) 1))
            (dev-path-len (unsigned 16))
            (pad (unsigned 16))
            (reserved (unsigned 32))))

(define-alien-type ublksrv-ctrl-dev-info
    (struct ublksrv-ctrl-dev-info
            (nr-hw-queues (unsigned 16))
            (queue-depth (unsigned 16))
            (state (unsigned 16))
            (pad0 (unsigned 16))
            (max-io-buf-bytes (unsigned 32))
            (dev-id (unsigned 32))
            (ublksrv-pid (signed 32))
            (pad1 (unsigned 32))
            (flags (unsigned 64))
            (ublksrv-flags (unsigned 64))
            (owner-uid (unsigned 32))
            (owner-gid (unsigned 32))
            (reserved1 (unsigned 64))
            (reserved2 (unsigned 64))))

(define-alien-type ublksrv-io-cmd
    (struct ublksrv-io-cmd
            (q-id (unsigned 16))
            (tag (unsigned 16))
            (result (signed 32))
            (addr (unsigned 64))))

(define-alien-type ublksrv-io-desc
    (struct ublksrv-io-desc
            (op-flags unsigned-int)
            (nr-sectors unsigned-int)
            (start-sector (unsigned 64))
            (addr (unsigned 64))))

(define-alien-type ublk-param-basic
    (struct ublk-param-basic
            (attrs (unsigned 32))
            (logical-bs-shift (unsigned 8))
            (physical-bs-shift (unsigned 8))
            (io-opt-shift (unsigned 8))
            (io-min-shift (unsigned 8))
            (max-sectors (unsigned 32))
            (chunk-sectors (unsigned 32))
            (dev-sectors (unsigned 64))
            (virt-boundary-mask (unsigned 64))))

(define-alien-type ublk-param-discard
  (struct ublk-param-discard
            (discard-alignment (unsigned 32))
            (discard-granularity (unsigned 32))
            (max-discard-sectors (unsigned 32))
            (max-write-zeroes-sectors (unsigned 32))
            (reserved0 (unsigned 16))))

(define-alien-type ublk-param-devt
    (struct ublk-param-devt
            (char-major (unsigned 32))
            (char-minor (unsigned 32))
            (disk-major (unsigned 32))
            (disk-minor (unsigned 32))))

(define-alien-type ublk-params
    (struct ublk-params
            (len (unsigned 32))
            (types (unsigned 32))
            (basic (struct ublk-param-basic))
            (discard (struct ublk-param-discard))
            (devt (struct ublk-param-devt))))
