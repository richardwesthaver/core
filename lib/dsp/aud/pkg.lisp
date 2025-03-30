;;; pkg.lisp --- AUD Packages

;; 

;;; Code:
(defpackage :dsp/aud
  (:nicknames :aud)
  (:use :cl :std :dat/midi :sndfile :jack :alsa :chromaprint)
  (:export
   #:jack-play-sound
   #:jack-close-sound
   #:*jack-sounds*
   #:jack-snd
   #:+sample-size+
   #:jack-open-sound
   #:*dac-folding*
   #:*jack-snd*
   #:jack-close-all-sounds
   #:n-sounds-playing-now
   #:n-sounds-pausing-now
   #:jack-sounds-playing-now))
