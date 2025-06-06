;;; chroma.lisp --- High-level CHROMAPRINT API

;; High-level wrapper for CHROMAPRINT IDs

;;; Code:
(in-package :aud)
;; REVIEW 2025-03-30: audio-fingerprint class?
(defclass chromaprint (id)
  ((id :initarg :id :type (vector (unsigned-byte 32)) :accessor id))
  (:documentation "An ID class wrapper for chromaprint (audio fingerprints)."))

(defun chromaprint (data samplerate &optional (channels 2) (algo :default) (batch-size 1024))
  (let ((len (length data)))
    (with-static-vector (d len :initial-contents data)
      (with-chromaprint-ctx (ctx :algo algo :samplerate samplerate :channels channels)
        (if (< len batch-size)
            (chromaprint-feed ctx (static-vector-pointer d) len)
            (multiple-value-bind (batches tail) (floor len batch-size)
              (loop for i from 0 below batches
                    do (chromaprint-feed ctx (static-vector-pointer d :offset (* i batch-size)) batch-size))
              (chromaprint-feed ctx (static-vector-pointer d :offset (- len tail)) tail)))
        (chromaprint-finish ctx)
        (with-alien ((fp-size int)
                     (fpa (* unsigned-int)))
          (chromaprint-get-raw-fingerprint ctx (addr fpa) (addr fp-size))
          (let ((fp (make-array fp-size :element-type '(unsigned-byte 32) :adjustable nil)))
            (loop for i from 0 below fp-size
                  do (setf (aref fp i) (deref fpa i)))
            (values fp fp-size)))))))

(defun chromaprint-file (file &optional (default :error))
  "Return a chromaprint associated with the audio data contained in FILE."
  ;; TODO: retrieve channel-count, samplerate, and audio data buffer - may
  ;; want to take advantage of buffering here to stream directly to
  ;; CHROMAPRINT-FEED
  )
