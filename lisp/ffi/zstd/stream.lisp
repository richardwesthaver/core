;;; stream1.lisp --- Zstd Streaming v1 API

;; 

;;; Code:
(in-package :zstd)

(define-alien-type zstd-cstream zstd-cctx)

(define-alien-routine "ZSTD_createCStream" (* zstd-cstream))
(define-alien-routine "ZSTD_freeCStream" void (zcs (* zstd-cstream)))

(define-alien-type zstd-enddirective int)
;; (enum nil
;;       (zstd-e-continue 0)
;;       (zstd-e-flush 1)
;;       (zstd-e-end 2))

(define-alien-routine "ZSTD_compressStream2" size-t
  (cctx (* zstd-cctx))
  (output (* zstd-outbuffer))
  (input (* zstd-inbuffer))
  (end-op zstd-enddirective))

(define-alien-routine "ZSTD_CStreamInSize" size-t)
(define-alien-routine "ZSTD_CStreamOutSize" size-t)
(define-alien-routine "ZSTD_initCStream" size-t (zcs (* zstd-cstream)) (compression-level int))

(define-alien-routine "ZSTD_compressStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)) (input (* zstd-inbuffer)))
(define-alien-routine "ZSTD_flushStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)))
(define-alien-routine "ZSTD_endStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)))

(define-alien-type zstd-dstream zstd-dctx)

(define-alien-routine "ZSTD_createDStream" (* zstd-dstream))
(define-alien-routine "ZSTD_freeDStream" void (zds (* zstd-dstream)))
(define-alien-routine "ZSTD_initDStream" size-t (zds (* zstd-dstream)))

(define-alien-routine "ZSTD_decompressStream" size-t
  (zds (* zstd-dstream))
  (output (* zstd-outbuffer))
  (input (* zstd-inbuffer)))

(define-alien-routine "ZSTD_DStreamInSize" size-t)
(define-alien-routine "ZSTD_DStreamOutSize" size-t)

(defmacro with-zstd-dstream ((dv dst &key (close t)) &body body)
  `(let ((,dv ,dst))
     (unwind-protect (progn ,@body)
       ,@(when close `((zstd-freedstream ,dv))))))

(defmacro with-zstd-cstream ((cv cst &key (close t)) &body body)
  `(let ((,cv ,cst))
     (unwind-protect (progn ,@body)
       ,@(when close `((zstd-freecstream ,cv))))))
