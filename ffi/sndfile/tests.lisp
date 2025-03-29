;;; sndfile/tests.lisp --- SNDFILE tests

;;; Code:
(defpackage :sndfile/tests
  (:use :cl :std :rt :sndfile :sb-ext :sb-alien))

(in-package :sndfile/tests)

(defsuite :sndfile)
(in-suite :sndfile)

(load-sndfile)

(deftest sanity ()
  (is (stringp (sf-version-string))))

(deftest open-wav ()
  "Open a WAV file."
  ;; read-only
  (with-alien ((info sf-info))
    (let ((file (sf-open "/opt/store/packy/data/test/Weltschmerz.wav" (sf-flag :read) (addr info))))
      (is= 1 (sf-format-check (addr info)))
      (let ((fmt (decode-sf-format (slot info 'format))))
        (iseq (car fmt) :wav)
        (is= (encode-sf-format :wav :pcm-16) (slot info 'format)))
      (is= 44100 (slot info 'samplerate))
      (is= 2 (slot info 'channels))
      (iszero (sf-close file))))
  ;; write-only
  (with-sf-info (info :samplerate 44100 :channels 1 :format '(:wav :pcm-16))
    (let ((path (tmpize-pathname "/tmp/snd.wav")))
      (with-sndfile (file (addr info) path :close t)
        (istype '(alien (* sndfile)) file)
        (is= 1 (sf-format-check (addr info)))))))
          
