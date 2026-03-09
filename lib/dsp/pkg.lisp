;;; pkg.lisp --- DSP Packages

;; 

;;; Code:
(defpkg :dsp/core
  (:use :cl :std :log :config)
  (:export
   #:media-codec
   #:audio-codec
   #:video-codec
   #:media-meta
   #:av-meta
   #:media-file
   #:audio-file
   #:video-file
   #:image-file
   #:*media-directory*
   #:*default-media-probe*
   #:dsp-condition
   #:dsp-error
   #:audio-config
   #:audio-system-config
   #:video-config
   #:video-system-config))

(defpkg :dsp/av
  (:use :cl :std :dsp/core :ffmpeg :sb-alien :dat/mime :id)
  (:export
   :av-error
   #:with-av-codec-context
   #:with-av-format-context
   #:av-dictionary-to-hash-table
   #:av-dictionary-alist
   #:with-av-parser
   #:av-context-metadata
   #:media-file-metadata
   #:media-file-format
   #:media-file-codecs
   #:media-file-stream-count
   #:load-av))

(defpkg :dsp/gst
  (:use :cl :std :dsp/core :gstreamer :sb-alien)
  (:export
   :gst-condition
   :gst-error
   #:load-gst
   #:gst-pipe))
