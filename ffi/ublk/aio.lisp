;;; aio.lisp --- Ublksrv from non-ublksrv context

;; 

;;; Code:
(in-package :ublk)

(define-alien-type ublksrv-aio-ctx
  (struct ublksrv-aio-ctx))

(define-alien-type ublksrv-aio
  (struct ublksrv-aio))
