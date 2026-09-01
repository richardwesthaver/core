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

(defar "ZSTD_createCStream" (* zstd-cstream))
(defar "ZSTD_freeCStream" void (zcs (* zstd-cstream)))

(define-alien-enum (zstd-enddirective :default :error :test eq)
                   :continue 0
                   :flush 1
                   :end 2)

(define-alien-variable "ZSTD_frameParameters" int)
#|
Alternative for ZSTD_compressStream2(zcs, output, input, ZSTD_e_continue).
NOTE: The return value is different. ZSTD_compressStream() returns a hint for
the next read size (if non-zero and not an error). ZSTD_compressStream2()
returns the minimum nb of bytes left to flush (if non-zero and not an error).
|#
(defar "ZSTD_initCStream" size-t (zcs (* zstd-cstream)) (compression-level int))
(defar "ZSTD_compressStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)) (input (* zstd-inbuffer)))

(defar "ZSTD_compressStream2" unsigned-long-long
  (cctx (* zstd-cctx))
  (output (* zstd-outbuffer))
  (input (* zstd-inbuffer))
  (end-op zstd-enddirective))

(defar "ZSTD_CStreamInSize" unsigned-long-long)
(defar "ZSTD_CStreamOutSize" unsigned-long-long)

(defar "ZSTD_flushStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)))
(defar "ZSTD_endStream" size-t (zcs (* zstd-cstream)) (output (* zstd-outbuffer)))

(define-alien-type zstd-dstream zstd-dctx)

(defar "ZSTD_createDStream" (* zstd-dstream))
(defar "ZSTD_freeDStream" void (zds (* zstd-dstream)))
;; returns recommended first input size
(defar "ZSTD_initDStream" size-t (zds (* zstd-dstream)))

#|
@return : 0 when a frame is completely decoded and fully flushed, or an error
          code, which can be tested using ZSTD_isError(), or any other value >
          0, which means there is some decoding or flushing to do to complete
          current frame.

Note: when an operation returns with an error code, the @zds state may be left
      in undefined state.  It's UB to invoke `ZSTD_decompressStream()` on such
      a state.  In order to re-use such a state, it must be first reset, which
      can be done explicitly (`ZSTD_DCtx_reset()`), or is implied for
      operations starting some new decompression job (`ZSTD_initDStream`,
      `ZSTD_decompressDCtx()`, `ZSTD_decompress_usingDict()`)
|#
(defar "ZSTD_decompressStream" size-t
  (zds (* zstd-dstream))
  (output (* zstd-outbuffer))
  (input (* zstd-inbuffer)))

(defar "ZSTD_DStreamInSize" size-t)
(defar "ZSTD_DStreamOutSize" size-t)

#+nil
(defmacro with-zstd-inbuffer ((iv &key src size pos) &body body)
  `(with-alien ((,iv (* zstd-inbuffer) (allocate-zstd-inbuffer)))
     (unwind-protect
          (progn
            ,@(when src `((setf (zstd-inbuffer-src ,iv) ,src)))
            ,@(when size `((setf (zstd-inbuffer-size ,iv) ,size)))
            ,@(when pos `((setf (zstd-inbuffer-pos ,iv) ,pos)))
            ,@body)
       (free-alien ,iv))))

#+nil
(defmacro with-zstd-outbuffer ((ov &key dst size pos) &body body)
  `(with-alien ((,ov (* zstd-outbuffer) (allocate-zstd-outbuffer)))
     (unwind-protect
          (progn
            ,@(when dst `((setf (zstd-outbuffer-dst ,ov) ,dst)))
            ,@(when size `((setf (zstd-outbuffer-size ,ov) ,size)))
            ,@(when pos `((setf (zstd-outbuffer-pos ,ov) ,pos)))
            ,@body)
       (free-alien ,ov))))
  
(defmacro with-zstd-buffers ((iv ov &key src src-size src-pos dst dst-size dst-pos) &body body)
  `(with-alien ((,iv (* zstd-inbuffer) (allocate-zstd-inbuffer))
                (,ov (* zstd-outbuffer) (allocate-zstd-outbuffer)))
     (unwind-protect
          (progn
            ,@(when src `((setf (zstd-inbuffer-src ,iv) ,src)))
            ,@(when src-size `((setf (zstd-inbuffer-size ,iv) ,src-size)))
            ,@(when src-pos `((setf (zstd-inbuffer-pos ,iv) ,src-pos)))
            ,@(when dst `((setf (zstd-outbuffer-dst ,ov) ,dst)))
            ,@(when dst-size `((setf (zstd-outbuffer-size ,ov) ,dst-size)))
            ,@(when dst-pos `((setf (zstd-outbuffer-pos ,ov) ,dst-pos)))
            ,@body)
       (free-alien ,iv)
       (free-alien ,ov))))
       
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

(defmacro with-zstd-streams ((cv dv &key init close (level (zstd-defaultclevel))) &body body)
  `(with-alien ((,cv (* zstd-cstream) (zstd-createcstream))
                (,dv (* zstd-dstream) (zstd-createdstream)))
     (unwind-protect
          (progn
            ,@(when init `((let ((%cinit (zstd-initcstream ,cv ,level))
                                 (%dinit (zstd-initdstream ,dv)))
                             ;; TODO 2024-09-18: 
                             (unless (zerop (zstd-iserror %cinit))
                               (zstd-cstream-error %cinit))
                             (unless (zerop (zstd-iserror %dinit))
                               (zstd-dstream-error %dinit)))))
            ,@body)
       ,@(when close `((zstd-freecstream ,cv)
                       (zstd-freedstream ,dv))))))
