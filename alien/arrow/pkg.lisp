;;; pkg.lisp --- Apache Arrow FFI

;; 

;;; Commentary:

;; this library seems to be linking in a static glib which conflicts with
;; dynamic link already loaded

;;; Code:
(defpackage :arrow
  (:use :cl :std :sb-alien)
  (:export :load-arrow))

(in-package :arrow)

(define-alien-loader arrow "/usr/lib/")

(define-alien-type arrow-release-function (function void (* (struct nil))))

(define-alien-type arrow-schema
  (struct arrow-schema
          (format c-string)
          (name c-string)
          (metadata c-string)
          (flags long)
          (n-children long)
          (children (array (* (struct arrow-schema))))
          (dictionary (* (struct arrow-schema)))
          (release (* arrow-release-function))
          (private-data (* t))))

(define-alien-type arrow-array
  (struct arrow-array
          (length long)
          (null-count long)
          (offset long)
          (n-buffers long)
          (n-children long)
          (buffers (array (* t)))
          (children (array (* (struct arrow-array))))
          (dictionary (* (struct arrow-array)))
          (release (* arrow-release-function))
          (private-data (* t))))

(define-alien-enum (arrow-device)
                   :cpu 1
                   :cuda 2
                   :cuda-host 3
                   :opencl 4
                   :vulkan 7
                   :metal 8
                   :vpi 9
                   :rocm 10
                   :rocm-host 11
                   :ext-dev 12
                   :cuda-managed 13
                   :oneapi 14
                   :webgpu 15
                   :hexagon 16)

(define-alien-type arrow-device-array
  (struct arrow-device-array
          (array arrow-array)
          (device-id long)
          (device-type arrow-device)
          (sync-event (* t))
          (reserved (array long 3))))

(define-alien-type arrow-array-stream
  (struct arrow-array-stream
          ;; fns
          (get-schema (* t))
          (get-next (* t))
          (get-last-error (* t))
          (release (* t))
          ;; void
          (private-data (* t))))

(define-alien-type arrow-device-array-stream
  (struct arrow-device-array-stream
          (device-type arrow-device)
          (get-schema (* t))
          (get-next (* t))
          (get-last-error (* t))
          (release (* t))
          (private-data (* t))))

;;; dlpack
(defconstant +dlpack-major-version+ 1)
(defconstant +dlpack-minor-version+ 0)

(define-alien-type dlpack-version
  (struct dlpack-version
          (major unsigned-int)
          (minor unsigned-int)))

(define-alien-enum (dl-device-type)
                   :cpu 1
                   :cuda 2
                   :cuda-host 3
                   :opencl 4
                   :vulkan 7
                   :metal 8
                   :vpi 9
                   :rocm 10
                   :rocm-host 11
                   :ext-dev 12
                   :cuda-managed 13
                   :oneapi 14
                   :webgpu 15
                   :hexagon 16)

(define-alien-type dl-device
  (struct dl-device
          (device-type dl-device-type)
          (device-id int)))

(define-alien-enum (dl-data-type-code)
                   :int 0
                   :uint 1
                   :float 2
                   :opaque-handle 3
                   :bfloat 4
                   :complex 5
                   :bool 6)

(define-alien-type dl-data-type
  (struct dl-data-type
          (code unsigned-char)
          (bits unsigned-char)
          (lanes unsigned-short)))

(define-alien-type dl-tensor
  (struct dl-tensor
          (data (* t))
          (device dl-device)
          (ndim int)
          (dtype dl-data-type)
          (shape (* long))
          (strides (* long))
          (byte-offset unsigned-long)))

(define-alien-type dl-managed-tensor
  (struct dl-managed-tensor
          (dl-tensor dl-tensor)
          (manager-ctx (* t))
          ;; fn
          (deleter (* t))))

;; #define DLPACK_FLAG_BITMASK_READ_ONLY (1UL << 0UL)

(define-alien-type dl-managed-tensor-versioned
  (struct dl-managed-tensor-versioned
          (version dlpack-version)
          (manager-ctx (* t))
          ;; fn
          (deleter (* t))
          (flags unsigned-long)
          (dl-tensor dl-tensor)))

  
  

