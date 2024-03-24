;;; sndfile.lisp --- low-level bindings to SNDFILE

;;; Commentary:

;;; Code:
(defpackage :sndfile
  (:use :cl :std :sb-alien)
  (:export ))

(in-package :sndfile)

(define-alien-loader "sndfile" t "/usr/lib/")
;; (load-sndfile)

(defconstant %seek-set 0)
(defconstant %seek-cur 1)
(defconstant %seek-end 2)

(defvar *type-constants* nil)
(defvar *subtype-constants* nil)

(defmacro define-format-type (name value)
  `(progn
     (defconstant ,name ,value)
     (pushnew ',name *type-constants*)))

(defmacro define-format-subtype (name value)
  `(progn
     (defconstant ,name ,value)
     (pushnew ',name *subtype-constants*)))

(define-format-type sf-format-wav #x010000)               ; Microsoft WAV format (little endian default). 
(define-format-type sf-format-aiff #x020000)               ; Apple/SGI AIFF format (big endian). 
(define-format-type sf-format-au #x030000)               ; Sun/NeXT AU format (big endian). 
(define-format-type sf-format-raw #x040000)               ; RAW PCM data. 
(define-format-type sf-format-paf #x050000)               ; Ensoniq PARIS file format. 
(define-format-type sf-format-svx #x060000)               ; Amiga IFF / SVX8 / SV16 format. 
(define-format-type sf-format-nist #x070000)               ; Sphere NIST format. 
(define-format-type sf-format-voc #x080000)               ; VOC files. 
(define-format-type sf-format-ircam #x0A0000)               ; Berkeley/IRCAM/CARL 
(define-format-type sf-format-w64 #x0B0000)               ; Sonic Foundry's 64 bit RIFF/WAV 
(define-format-type sf-format-mat4 #x0C0000)               ; Matlab (tm) V4.2 / GNU Octave 2.0 
(define-format-type sf-format-mat5 #x0D0000)               ; Matlab (tm) V5.0 / GNU Octave 2.1 
(define-format-type sf-format-pvf #x0E0000)               ; Portable Voice Format 
(define-format-type sf-format-xi #x0F0000)               ; Fasttracker 2 Extended Instrument 
(define-format-type sf-format-htk #x100000)               ; HMM Tool Kit format 
(define-format-type sf-format-sds #x110000)               ; Midi Sample Dump Standard 
(define-format-type sf-format-avr #x120000)               ; Audio Visual Research 
(define-format-type sf-format-wavex #x130000)               ; MS WAVE with WAVEFORMATEX 
(define-format-type sf-format-sd2 #x160000)               ; Sound Designer 2 
(define-format-type sf-format-flac #x170000)               ; FLAC lossless file format 
(define-format-type sf-format-caf #x180000)               ; Core Audio File format 

;;;; Subtypes from here on. 

(define-format-subtype sf-format-pcm-s8 #x0001)                 ; Signed 8 bit data 
(define-format-subtype sf-format-pcm-16 #x0002)                 ; Signed 16 bit data 
(define-format-subtype sf-format-pcm-24 #x0003)                 ; Signed 24 bit data 
(define-format-subtype sf-format-pcm-32 #x0004)                 ; Signed 32 bit data 
(define-format-subtype sf-format-pcm-u8 #x0005)                 ; Unsigned 8 bit data (WAV and RAW only) 

(define-format-subtype sf-format-float #x0006)                 ; 32 bit float data 
(define-format-subtype sf-format-double #x0007)                 ; 64 bit float data 

(define-format-subtype sf-format-ulaw #x0010)                 ; U-Law encoded. 
(define-format-subtype sf-format-alaw #x0011)                 ; A-Law encoded. 
(define-format-subtype sf-format-ima-adpcm #x0012)                 ; IMA ADPCM. 
(define-format-subtype sf-format-ms-adpcm #x0013)                 ; Microsoft ADPCM. 

(define-format-subtype sf-format-gsm610 #x0020)                 ; GSM 6.10 encoding. 
(define-format-subtype sf-format-vox-adpcm #x0021)                 ; OKI / Dialogix ADPCM 

(define-format-subtype sf-format-g721-32 #x0030)                 ; 32kbs G721 ADPCM encoding. 
(define-format-subtype sf-format-g723-24 #x0031)                 ; 24kbs G723 ADPCM encoding. 
(define-format-subtype sf-format-g723-40 #x0032)                 ; 40kbs G723 ADPCM encoding. 

(define-format-subtype sf-format-dwvw-12 #x0040)                 ; 12 bit Delta Width Variable Word encoding. 
(define-format-subtype sf-format-dwvw-16 #x0041)                 ; 16 bit Delta Width Variable Word encoding. 
(define-format-subtype sf-format-dwvw-24 #x0042)                 ; 24 bit Delta Width Variable Word encoding. 
(define-format-subtype sf-format-dwvw-n #x0043)                 ; N bit Delta Width Variable Word encoding. 

(define-format-subtype sf-format-dpcm-8 #x0050)                 ; 8 bit differential PCM (XI only)
(define-format-subtype sf-format-dpcm-16 #x0051)                 ; 16 bit differential PCM (XI only)

(defun decode-bitflags (value flag-names)
  (loop for symbol in flag-names
        as flag-value = (symbol-value symbol)
        when (= flag-value (logand value flag-value))
        collect symbol))

(defun match-field (value flag-names)
 (loop for symbol in flag-names
       when (= value (symbol-value symbol))
       return symbol))

;;;; Endian-ness options. 

(defconstant sf-endian-file #x00000000)     ; Default file endian-ness. 
(defconstant sf-endian-little #x10000000)     ; Force little endian-ness. 
(defconstant sf-endian-big #x20000000)     ; Force big endian-ness. 
(defconstant sf-endian-cpu #x30000000)     ; Force CPU endian-ness. 

(defconstant sf-format-submask #x0000FFFF)
(defconstant sf-format-typemask #x0FFF0000)
(defconstant sf-format-endmask #x30000000)

(defconstant sf-str-title 1)
(defconstant sf-str-copyright 2)
(defconstant sf-str-software 3)
(defconstant sf-str-artist 4)
(defconstant sf-str-comment 5)
(defconstant sf-str-date 6)

;;;; Public error numbers
(defconstant sf-err-no-error 0)
(defconstant sf-err-unrecognized-format 1)
(defconstant sf-err-system 2)
(defconstant sf-err-malformed-file 3)
(defconstant sf-err-unsupported-encoding 4)
