;;; stream1.lisp --- Zstd Streaming v1 API

;; 

;;; Code:
(in-package :zstd)

(deferror zstd-dstream-error (zstd-alien-error) ())
(deferror zstd-cstream-error (zstd-alien-error)
    ()
    (:report (lambda (c s)
               (format s "ZSTD CStream signalled error: ~A" (zstd-errorcode* (zstd-error-code c))))))

(defun zstd-dstream-error (code)
  (error 'zstd-dstream-error :code code))

(defun zstd-cstream-error (code)
  (error 'zstd-cstream-error :code code))

(define-alien-type zstd-cstream zstd-cctx)

(define-alien-routine "ZSTD_createCStream" (* zstd-cstream))
(define-alien-routine "ZSTD_freeCStream" void (zcs (* zstd-cstream)))

(define-alien-enum (zstd-enddirective int :default :error :test eq)
                   :e-continue 0
                   :e-flush 1
                   :e-end 2)

(define-alien-variable "ZSTD_frameParameters" int)
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

(defmacro with-zstd-cstream ((cv &key (init t) (close t) (level (zstd-defaultclevel)) ) &body body)
  `(with-alien ((,cv (* zstd-cstream) (zstd-createcstream)))
     (unwind-protect
          (progn
            ,@(when init `((let ((%cinit (zstd-initcstream ,cv ,level)))
                             (unless (zerop (zstd-iserror %cinit))
                               (zstd-cstream-error %cinit)))))
            ,@body)
       ,@(when close `((zstd-freecstream ,cv))))))

(defmacro with-zstd-dstream ((dv &key (init t) (close t)) &body body)
  `(with-alien ((,dv (* zstd-dstream) (zstd-createdstream)))
     (unwind-protect
          (progn
            ,@(when init `((let ((%dinit (zstd-initdstream ,dv)))
                             (unless (zerop (zstd-iserror %dinit))
                               (zstd-dstream-error %dinit)))))
            ,@body)
       ,@(when close `((zstd-freedstream ,dv))))))
