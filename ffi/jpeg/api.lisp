;;; api.lisp --- JPEG Lisp API

;; 

;;; Code:
(in-package :jpeg)

(define-condition jpeg-condition () 
  ((jpeg :initarg :jpeg :reader jpeg)
   (message :initarg :message :reader message))
  (:report (lambda (c s) (format s "~A failed to perform:~%~A" (jpeg c) (message c)))))

(deferror jpeg-error (jpeg-condition) ())

(defwarning jpeg-warning (jpeg-condition) ())

(defun make-jpeg (sym)
  (tj3init (init-type sym)))

(defun jpeg-report (jpeg)
  (case (error-type* (tj3geterrorcode (sap jpeg)))
    (:warning (warn 'jpeg-warning :jpeg jpeg :message (tj3geterrorstr (sap jpeg))))
    (:fatal (error 'jpeg-error :jpeg jpeg :message (tj3geterrorstr (sap jpeg))))))

(defun jpeg-test (jpeg ret)
  (if (< ret 0)
      (jpeg-report jpeg)
      ret))

(defmacro jpeg-unwrap (form)
  `(jpeg-test ,(second form) (,(car form) (sap ,(second form)) ,@(cddr form))))

(definline jpeg-set (jpeg k v)
  (tj3set (sap jpeg) (parameter k)
          (etypecase v
            (integer v)
            (boolean (if v 1 0)))))

(definline jpeg-get (jpeg k)
  (tj3get (sap jpeg) (parameter k)))

(defsetf jpeg-get jpeg-set)

(defclass jpeg () ((sap :initarg :sap :initform nil :accessor sap)))

(defclass jpeg-compressor (jpeg) ())

(defclass jpeg-decompressor (jpeg) ())

(defclass jpeg-transformer (jpeg) ())

(defmethod shared-initialize :after ((jpeg jpeg) slots
                                     &key stop-on-warning bottom-up progressive arithmetic)
  (unless (sap jpeg)
    (let ((sap (make-jpeg
                (etypecase jpeg
                  (jpeg-compressor :compress)
                  (jpeg-decompressor :decompress)
                  (jpeg-transformer :transform)))))
      (if (null-alien sap)
          (error "failed to allocate JPEG handle.")
          (setf (sap jpeg) sap))))
  (when stop-on-warning (jpeg-set jpeg :stop-on-warning stop-on-warning))
  (when bottom-up (jpeg-set jpeg :bottom-up bottom-up))
  (when arithmetic (jpeg-set jpeg :arithmetic arithmetic))
  (when progressive (jpeg-set jpeg :progressive progressive)))

(defmethod free ((self jpeg))
  (with-slots (sap) self
    (when sap
      (tj3destroy sap)
      (setf sap nil))))

(defmethod shared-initialize :after ((jpeg jpeg-compressor) slots
                                     &key no-realloc fast-dct optimize 
                                     lossless quality subsampling color-space
                                     restart-blocks restart-rows
                                     x-density y-density)
  
  (jpeg-set jpeg :no-realloc no-realloc)
  (jpeg-set jpeg :quality quality)
  (when subsampling
    (jpeg-set jpeg :subsampling (chrominance-sampling subsampling)))
  (when color-space
    (jpeg-set jpeg :color-space (color-space color-space)))
  (jpeg-set jpeg :fast-dct fast-dct)
  (jpeg-set jpeg :optimize optimize)
  (jpeg-set jpeg :lossless lossless)
  (jpeg-set jpeg :restart-blocks restart-blocks)
  (jpeg-set jpeg :restart-rows restart-rows)
  (jpeg-set jpeg :x-density x-density)
  (jpeg-set jpeg :y-density y-density))

(defmacro with-jpeg-compressor ((sym &rest args) &body body)
  `(let ((,sym (make-instance 'jpeg-compressor ,@args)))
     (unwind-protect (progn ,@body)
       (free ,sym))))

(defmethod shared-initialize :after ((jpeg jpeg-decompressor) slots
                                     &key fast-dct fast-upsample optimize
                                     lossless subsampling color-space scan-limit
                                     x-density y-density)
  (jpeg-set jpeg :fast-upsample fast-upsample)
  (when subsampling
    (jpeg-set jpeg :subsampling (chrominance-sampling subsampling)))
  (when color-space
    (jpeg-set jpeg :color-space (color-space color-space)))
  (jpeg-set jpeg :fast-dct fast-dct)
  (jpeg-set jpeg :optimize optimize)
  (jpeg-set jpeg :lossless lossless)
  (jpeg-set jpeg :scan-limit scan-limit)
  (jpeg-set jpeg :x-density x-density)
  (jpeg-set jpeg :y-density y-density))

(defmacro with-jpeg-decompressor ((sym &rest args) &body body)
  `(let ((,sym (make-instance 'jpeg-decompressor ,@args)))
     (unwind-protect (progn ,@body)
       (free ,sym))))

(defmethod shared-initialize :after ((jpeg jpeg-transformer) slots
                                     &key no-realloc subsampling scan-limit)
  (jpeg-set jpeg :no-realloc no-realloc)
  (when subsampling 
    (jpeg-set jpeg :subsampling (chrominance-sampling subsampling)))
  (jpeg-set jpeg :scan-limit scan-limit))

(defmacro with-jpeg-transformer ((sym &rest args) &body body)
  `(let ((,sym (make-instance 'jpeg-transformer ,@args)))
     (unwind-protect (progn ,@body)
       (free ,sym))))

;; TODO 2025-07-19: 
(defun save-jpeg-image (dst buf width height jpeg 
                        &key (pixel-format :rgb) pitch (bit-depth 8) size)
  (unless pitch
    (setf pitch (* (pixel-size pixel-format) (jpeg-get jpeg :width))))
  (with-alien ((dst* (* unsigned-char) dst)
               (size* size-t (if dst size 0)))
    (let ((res (ecase bit-depth
                 (8 (tj3compress8 (sap jpeg) buf width pitch height pixel-format (addr dst*) (addr size*)))
                 (12 (tj3compress12 (sap jpeg) buf width pitch height pixel-format (addr dst*) (addr size*)))
                 (16 (tj3compress16 (sap jpeg) buf width pitch height pixel-format (addr dst*) (addr size*))))))
      (if (< res 0) 
          (jpeg-report jpeg)
          (values dst* size*)))))

(defun load-jpeg-image (src jpeg
                        &key (pixel-format :rgb) pitch (bit-depth 8)
                        size buffer)
  (etypecase src
    ((or pathname string)
     (with-open-file (f src :element-type 'octet)
       (setf src (make-array (file-length f) :element-type 'octet))
       (read-sequence src f)))
    (vector))
  (unless size (setf size (length src)))
  (with-vector-sap (ptr src)
    (jpeg-unwrap (tj3decompressheader (sap jpeg) ptr size))
    (unless pitch
      (setf pitch (* (pixel-size pixel-format) (jpeg-get jpeg :width))))
    (let ((bsize (* pitch (jpeg-get jpeg :height))))
      (flet ((load-img (buf)
               (let ((pxf (pixel-format pixel-format)))
                 (let ((ret (ecase bit-depth
                              (8 (tj3decompress8 (sap jpeg) ptr size buf pitch pxf))
                              (12 (tj3decompress12 (sap jpeg) ptr size buf pitch pxf))
                              (16 (tj3decompress16 (sap jpeg) ptr size buf pitch pxf)))))
                   (jpeg-test jpeg ret)
                   (values buf
                           (jpeg-get jpeg :width)
                           (jpeg-get jpeg :height)
                           pixel-format
                           bsize)))))
        (etypecase buffer
          (null (load-img (cast (tj3alloc (ceiling (* bit-depth bsize) 8)) (* unsigned-char))))
          (alien (load-img buffer)))))))

(defun transform-jpeg-image (src dst op jpeg
                             &key source-size destination-size perfect
                                  trim crop gray progressive
                                  copy-none arithmetic optimize)
  (check-type src alien)
  (with-alien ((buf* (* t) dst)
               (size* size-t destination-size)
               (transform transform))
    (setf (slot transform 'operation) (operation op))
    (let ((opts))
      (when perfect (push :perfect opts))
      (when trim (push :trim opts))
      (when gray (push :gray opts))
      (when progressive (push :progressive opts))
      (when copy-none (push :copy-none opts))
      (when arithmetic (push :arithmetic opts))
      (when optimize (push :optimize opts))
      ;; TODO 2025-07-19: 
      (when crop (push :crop opts))
      ;; (when crop 
      ;;   (push :crop opts)
      ;;   (destructuring-bind (x y w h) crop
      ;;     (setf (slot transform 'region) )))
      ;; (setf (slot transform 'options) opts)
      )
    (jpeg-unwrap (tj3transform jpeg src source-size 1 (addr buf*) (addr size*) (addr transform)))
    (values buf* size*)))

;;; Serde
(defmethod deserialize (self (format (eql :jpeg)) &rest args)
  (apply 'load-jpeg-image self (make-instance 'jpeg-decompressor) args))
