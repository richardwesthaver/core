;;; sndfile.lisp --- low-level bindings to SNDFILE

;;; Commentary:

;; see http://www.mega-nerd.com/libsndfile/api.html

;;; Code:
(defpackage :sndfile
  (:use :cl :std :sb-alien)
  (:export :sf-version-string :load-sndfile :sndfile 
   :sf-chunk-iterator :sf-info :samplerate :channels 
   :frames :format :sections :seekable
   :sf-format-info :extension :sf-seek-mode :sf-dither
   :sf-cue-point :sf-loop :sf-instrument :sf-loop-info
   :sf-format :sf-format-subtype :sf-flag :sf-flag*
   :sf-err :sf-err* :sf-command-op :sf-command
   :sf-open
   :sf-open-fd
   :sf-error
   :sf-strerror
   :sf-error-number
   :sf-perror
   :sf-error-str
   :sf-format-check
   :sf-seek
   :sf-set-string
   :sf-get-string
   :sf-current-byterate
   :sf-read-raw
   :sf-write-raw
   :sf-close
   :sf-write-sync
   :sf-chunk-info
   :sf-set-chunk
   :sf-get-chunk-iterator
   :sf-next-chunk-iterator
   :sf-get-chunk-size
   :sf-get-chunk-data
   :decode-sf-format
   :encode-sf-format
   :sf-str
   :sf-format-mask
   :with-sndfile
   :with-sf-info
   :sf-readf-float
   :sf-writef-float
   :sf-writef-double
   :sf-readf-double
   :sf-writef-short
   :sf-readf-short))

(in-package :sndfile)

(define-alien-loader sndfile "/usr/lib/")

(define-opaque sndfile)
(define-opaque sf-chunk-iterator)

(define-alien-type sf-count long)

(define-alien-type sf-info
    (struct sf-info
      (frames sf-count)
      (samplerate int)
      (channels int)
      (format int)
      (sections int)
      (seekable int)))

(define-alien-type sf-format-info
    (struct sf-format-info
      (format int)
      (name c-string)
      (extension c-string)))

;; SF_SEEK_*
(define-alien-enum (sf-seek-mode)
  :set 0
  :cur 1
  :end 2)

;; SFD_*
(define-alien-enum (sf-dither)
  :default-level 0
  :custom-level #x40000000
  :no-dither 500
  :white 501
  :triangular-pdf 502)

(define-alien-type sf-dither-info
    (struct sf-dither-info
      (type int)
      (level double)
      (name c-string)))

(define-alien-type sf-embed-file-info
    (struct sf-embed-file-info
      (offset sf-count)
      (length sf-count)))

(define-alien-type sf-cue-point
    (struct sf-cue-point
      (indx int)
      (position unsigned-int)
      (fcc-chunk int)
      (chunk-start int)
      (block-start int)
      (sample-offset unsigned-int)
      (name (array char 256))))

(define-alien-enum (sf-loop)
  :none 800
  :forward 801
  :backward 802
  :alternating 803)

(define-alien-type sf-instrument
    (struct sf-instrument
      (gain int)
      (detune char)
      (basenote char)
      (velocity-lo char)
      (velocity-hi char)
      (key-lo char)
      (key-hi char)
      (loop-count int)
      (loops (array
              (struct nil
                (mode int)
                (start unsigned-int)
                (end unsigned-int)
                (count unsigned-int))
              16))))

(define-alien-type sf-loop-info
    (struct sf-loop-info
      (time-sig-num short)
      (time-sig-den short)
      (loop-mode int)
      (num-beats int)
      (bpm float)
      (root-key int)
      (future (array int 6))))

(define-alien-enum (sf-format)
  :wav #x010000               ; Microsoft WAV format (little endian default). 
  :aiff #x020000               ; Apple/SGI AIFF format (big endian). 
  :au #x030000               ; Sun/NeXT AU format (big endian). 
  :raw #x040000               ; RAW PCM data. 
  :paf #x050000               ; Ensoniq PARIS file format. 
  :svx #x060000               ; Amiga IFF / SVX8 / SV16 format. 
  :nist #x070000               ; Sphere NIST format. 
  :voc #x080000               ; VOC files. 
  :ircam #x0A0000               ; Berkeley/IRCAM/CARL 
  :w64 #x0B0000               ; Sonic Foundry's 64 bit RIFF/WAV 
  :mat4 #x0C0000               ; Matlab (tm) V4.2 / GNU Octave 2.0 
  :mat5 #x0D0000               ; Matlab (tm) V5.0 / GNU Octave 2.1 
  :pvf #x0E0000               ; Portable Voice Format 
  :xi #x0F0000               ; Fasttracker 2 Extended Instrument 
  :htk #x100000               ; HMM Tool Kit format 
  :sds #x110000               ; Midi Sample Dump Standard 
  :avr #x120000               ; Audio Visual Research 
  :wavex #x130000               ; MS WAVE with WAVEFORMATEX 
  :sd2 #x160000               ; Sound Designer 2 
  :flac #x170000               ; FLAC lossless file format 
  :caf #x180000)               ; Core Audio File format 

(define-alien-enum (sf-format-subtype)
  ;; subtypes
  :pcm-s8 #x0001                 ; Signed 8 bit data 
  :pcm-16 #x0002                 ; Signed 16 bit data 
  :pcm-24 #x0003                 ; Signed 24 bit data 
  :pcm-32 #x0004                 ; Signed 32 bit data 
  :pcm-u8 #x0005                 ; Unsigned 8 bit data (WAV and RAW only) 

  :float #x0006                 ; 32 bit float data 
  :double #x0007                 ; 64 bit float data 

  :ulaw #x0010                 ; U-Law encoded. 
  :alaw #x0011                 ; A-Law encoded. 
  :ima-adpcm #x0012                 ; IMA ADPCM. 
  :ms-adpcm #x0013                 ; Microsoft ADPCM. 

  :gsm610 #x0020                 ; GSM 6.10 encoding. 
  :vox-adpcm #x0021                 ; OKI / Dialogix ADPCM 

  :g721-32 #x0030                 ; 32kbs G721 ADPCM encoding. 
  :g723-24 #x0031                 ; 24kbs G723 ADPCM encoding. 
  :g723-40 #x0032                 ; 40kbs G723 ADPCM encoding. 
  :-dwvw-12 #x0040                 ; 12 bit Delta Width Variable Word encoding. 
  :dwvw-16 #x0041                 ; 16 bit Delta Width Variable Word encoding. 
  :dwvw-24 #x0042                 ; 24 bit Delta Width Variable Word encoding. 
  :dwvw-n #x0043                 ; N bit Delta Width Variable Word encoding. 

  :dpcm-8 #x0050                 ; 8 bit differential PCM (XI only)
  :dpcm-16 #x0051)                 ; 16 bit differential PCM (XI only)

;;;; Endian-ness options. 
(define-alien-enum (sf-endian)
  :file #x00000000     ; Default file endian-ness. 
  :little #x10000000     ; Force little endian-ness. 
  :big #x20000000     ; Force big endian-ness. 
  :cpu #x30000000)     ; Force CPU endian-ness. 

(define-alien-enum (sf-format-mask)
  :sub #x0000FFFF
  :type #x0FFF0000
  :end #x30000000)

(define-alien-enum (sf-str)
  :title 1
  :copyright 2
  :software 3
  :artist 4
  :comment 5
  :date 6)

(define-alien-enum (sf-flag)
  :false 0
  :true 1
  :read #x10
  :write #x20
  :rdwr #x30
  :ambisonic-none #x40
  :ambisonic-b-format #x41)

;;;; Public error numbers
(define-alien-enum (sf-err)
  :no-error 0
  :unrecognized-format 1
  :system 2
  :malformed-file 3
  :unsupported-encoding 4)

;;;; SF commands
(define-alien-enum (sf-command-op)
  :get-lib-version #x1000
  :get-log-info #x1001
  :get-current-sf-info #x1002
  :get-norm-double #x1010
  :get-norm-float #x1011
  :set-norm-double #x1012
  :set-norm-float #x1013
  :set-scale-float-int-read #x1014
  :set-scale-int-float-write #x1015
  :get-simple-format-count #x1020
  :get-simple-format #x1021
  :get-format-info #x1028
  :get-format-major-count #x1030
  :get-format-major #x1031
  :get-format-subtype-count #x1032
  :get-format-subtype #x1033
  :calc-signal-max #x1040
  :calc-norm-signal-max #x1041
  :calc-max-all-channels #x1042
  :calc-norm-max-all-channels #x1043
  :get-signal-max #x1044
  :get-max-all-channels #x1045
  :set-add-peak-chunk #x1050
  :update-header-now #x1060
  :set-update-header-auto #x1061
  :file-truncate #x1080
  :set-raw-start-offset #x1090
  ;; /* Commands reserved for dithering, which is not implemented. */
  :set-dither-on-write #x10A0
  :set-dither-on-read #x10A1
  :get-dither-info-count #x10A2
  :get-dither-info #x10A3
  :get-embed-file-info #x10B0
  :set-clipping #x10C0
  :get-clipping #x10C1
  :get-cue-count #x10CD
  :get-cue #x10CE
  :set-cue #x10CF
  :get-instrument #x10D0
  :set-instrument #x10D1
  :get-loop-info #x10E0
  :get-broadcast-info #x10F0
  :set-broadcast-info #x10F1
  :get-channel-map-info #x1100
  :set-channel-map-info #x1101
  :raw-data-needs-endswap #x1110
  ;; /* Support for Wavex Ambisonics Format */
  :wavex-set-ambisonic #x1200
  :wavex-get-ambisonic #x1201
  ;; RF64 files can be set so that on-close, writable files
  ;; that have less than 4GB of data in them are converted to
  ;; RIFF/WAV, as per EBU recommendations.
  :rf64-auto-downgrade #x1210
  :set-vbr-encoding-quality #x1300
  :set-compression-level #x1301
  ;; /* Ogg format commands */
  :set-ogg-page-latency-ms #x1302
  :set-ogg-page-latency #x1303
  :get-ogg-stream-serialno #x1306
  :get-bitrate-mode #x1304
  :set-bitrate-mode #x1305
  ;; /* Cart Chunk support */
  :set-cart-info #x1400
  :get-cart-info #x1401
  ;; /* Opus files original samplerate metadata */
  :set-original-samplerate #x1500
  :get-original-samplerate #x1501
  ;; /* Following commands for testing only. */
  :test-ieee-float-replace #x6001
  ;; These SFC_SET_ADD_* values are deprecated and will
  ;; disappear at some time in the future. They are
  ;; guaranteed to be here up to and including version 1.0.8
  ;; to avoid breakage of existing software.  They currently
  ;; do nothing and will continue to do nothing.
  :set-add-header-pad-chunk #x1051
  :set-add-dither-on-write #x1070
  :set-add-dither-on-read #x1071)

;;; Functions
(defar sf-open (* sndfile) (path c-string) (mode int) (sfinfo (* sf-info)))
(defar sf-open-fd (* sndfile) (fd int) (mode int) (sfinfo (* sf-info)))
(defar sf-error int (sndfile (* sndfile)))
(defar sf-strerror c-string (sndfile (* sndfile)))
(defar sf-error-number c-string (errnum int))
(defar sf-perror int (sndfile (* sndfile)))
(defar sf-error-str int (sndfile (* sndfile)) (str c-string) (len size-t))
(defar sf-command int (sndfile (* sndfile)) (command int) (data (* t)) (datasize int))
(defar sf-format-check int (info (* sf-info)))
(defar sf-seek sf-count (sndfile (* sndfile)) (frames sf-count) (whence int))

(defar sf-set-string int (sndfile (* sndfile)) (str-type int) (str c-string))
(defar sf-get-string c-string (sndfile (* sndfile)) (str-type int))
(defar sf-version-string c-string)
(defar sf-current-byterate int (sndfile (* sndfile)))
(defar sf-read-raw sf-count (sndfile (* sndfile)) (ptr (* t)) (bytes sf-count))
(defar sf-write-raw sf-count (sndfile (* sndfile)) (ptr (* t)) (bytes sf-count))

(defar sf-readf-float sf-count (sndfile (* sndfile)) (ptr (* float)) (frames sf-count))
(defar sf-writef-float sf-count (sndfile (* sndfile)) (ptr (* float)) (frames sf-count))
(defar sf-readf-short sf-count (sndfile (* sndfile)) (ptr (* short)) (frames sf-count))
(defar sf-writef-short sf-count (sndfile (* sndfile)) (ptr (* short)) (frames sf-count))
(defar sf-readf-double sf-count (sndfile (* sndfile)) (ptr (* double)) (frames sf-count))
(defar sf-writef-double sf-count (sndfile (* sndfile)) (ptr (* double)) (frames sf-count))

;; ...
(defar sf-close int (sndfile (* sndfile)))

(defar sf-write-sync void
  (sndfile (* sndfile)))

(define-alien-type sf-chunk-info
    (struct sf-chunk-info
      (id (array char 64))
      (id-size unsigned)
      (datalen unsigned)
      (data (* t))))

(defar sf-set-chunk int
  (sndfile (* sndfile))
  (chunk-info (* sf-chunk-info)))

(defar sf-get-chunk-iterator (* sf-chunk-iterator)
  (sndfile (* sndfile))
  (chunk-info (* sf-chunk-info)))

(defar sf-next-chunk-iterator (* sf-chunk-iterator)
  (iterator (* sf-chunk-iterator)))

(defar sf-get-chunk-size int
  (it (* sf-chunk-iterator))
  (chunk-info (* sf-chunk-info)))

(defar sf-get-chunk-data int
  (it (* sf-chunk-iterator))
  (chunk-info (* sf-chunk-info)))

;;; Utils
(defun decode-sf-format (i)
  "Decode an SF-FORMAT integer into a list of (TYPE SUB ENDIAN)."
  (list
   (sf-format* (logand i (sf-format-mask :type)))
   (sf-format-subtype* (logand i (sf-format-mask :sub)))
   (sf-endian* (logand i (sf-format-mask :end)))))

(defun encode-sf-format (type sub &optional (end :file))
  "Encode an SF-FORMAT integer from TYPE SUB and optional ENDian."
  (logior (sf-format type) (sf-format-subtype sub) (sf-endian end)))

(defmacro with-sf-info ((sym &key samplerate channels format sections seekable) &body body)
  `(with-alien ((,sym sf-info))
     ,@(when samplerate `((setf (slot ,sym 'samplerate) ,samplerate)))
     ,@(when channels `((setf (slot ,sym 'channels) ,channels)))
     ,@(when format 
	 (etypecase format
	   (list `((setf (slot ,sym 'format) (apply 'encode-sf-format ,format))))
	   (integer `((setf (slot ,sym 'format) ,format)))))
     ,@(when sections `((setf (slot ,sym 'sections) ,sections)))
     ,@(when seekable `((setf (slot ,sym 'seekable) ,seekable)))
     ,@body))

(defmacro with-sndfile ((sym info path &key close (mode (sf-flag :read))) &body body)
  `(let ((,sym (sf-open (namestring ,path) ,mode ,info)))
     (unwind-protect (progn ,@body)
       ,@(when close `((sf-close ,sym))))))
