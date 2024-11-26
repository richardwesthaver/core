;;; pkg.lisp --- AUD Packages

;; 

;;; Code:
(defpackage :dsp/aud
  (:nicknames :aud)
  (:use :cl :std :dat/midi :sndfile :alsa))

(defpackage :dsp/aud/music
  (:nicknames :aud/music :music)
  (:use :cl :std :aud))
