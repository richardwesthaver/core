;;; pkg.lisp --- FFmpeg FFI

;; Bindings for FFmpeg libraries (libavcodec, libavutil, etc)

;;; Commentary:

;; ref: https://www.ffmpeg.org/doxygen/0.6/api-example_8c-source.html

;;; Code:
(defpackage :ffmpeg
  (:use :cl :std :sb-alien)
  (:export :load-avcodec :load-avutil :load-avformat
   :load-avfilter :avcodec-open2
   :avcodec-version :avformat-version :avutil-version :avfilter-version
   :avformat-alloc-context
   :avformat-free-context
   :avcodec-alloc-context3
   :avcodec-free-context
   :avcodec-close
   :avsubtitle-free
   :avcodec-get-class
   :avcodec-get-subtitle-rect-class
   :av-codec-iterate
   :avcodec-find-decoder
   :avcodec-find-decoder-by-name
   :avcodec-find-encoder
   :avcodec-find-encoder-by-name
   :av-codec-is-encoder
   :av-codec-is-decoder
   :av-get-profile-name
   :av-codec-id
   :av-codec
   :av-codec-context
   :av-dictionary
   :av-class
   :av-subtitle))
           
(in-package :ffmpeg)

(define-alien-loader :avcodec "/usr/lib/")
(define-alien-loader :avutil "/usr/lib/")
(define-alien-loader :avformat "/usr/lib/")
(define-alien-loader :avfilter "/usr/lib/")

;;; version
(define-alien-routine avcodec-version unsigned)
(define-alien-routine avformat-version unsigned)
(define-alien-routine avutil-version unsigned)
(define-alien-routine avfilter-version unsigned)

;;; avutil
(define-alien-enum (av-media-type int)
  :unknown -1
  :video 0
  :audio 1
  :data 2
  :subtitle 3
  :attachment 4)

;; dict.h
(define-opaque av-dictionary)

(define-alien-type av-dictionary-entry
    (struct av-dictionary-entry
      (key c-string)
      (vbal c-string)))

;; rational.h
(define-alien-type av-rational
    (struct av-rational
      (num int)
      (den int)))

;; samplefmt.h
(define-alien-enum (av-sample-format int)
  :none -1
  :u8 0
  :s16 1
  :s32 2
  :flt 3
  :dbl 4
  :u8p 5
  :s16p 6
  :s32p 7
  :fltp 8
  :dblp 9
  :s64 10
  :s64p 11)

;; pixfmt.h
;; TODO 2025-04-07: 
(define-alien-enum (av-pixel-format int)
  :none -1
  :yuv420p 0
  :yuv422 1)

(define-alien-enum (av-color-primaries int)
  :reserved0 0
  :bt709 1
  :unspecified 2
  :reserved 3
  :bt470m 4
  :bt470bg 5
  :smpte170m 6
  :smpte240m 7
  :film 8
  :bt2020 9
  :smpte428 10
   :smpte428-1 10
  :smpte431 11
  :smpte432 12
  :ebu3213 22
  :jedec-p22 22)

(define-alien-enum (av-color-transfer-characteristic int)
  :reserved0 0
  :bt709 1
  :unspecified 2
  :reserved 3
  :gamma22 4
  :gamma28 5
  :smpte170m 6
  :smpte240m 7
  :linear 8
  :log 9
  :log-sort 10
  :iec61966-2-4 11
  :bt1361-ecg 12
  :iec61966-2-1 13
  :bt2020-10 14
  :bt2020-12 15
  :smpte2084 16
  :smptest2084 16
  :smpte428 17
  :smpte428-1 17
  :arib-std-b67 18)

(define-alien-enum (av-color-space int)
  :rgb 0
  :bt709 1
  :unspecified 2
  :reserved 3
  :fcc 4
  :bt470bg 5
  :smpte170m 6
  :smpte240m 7
  :ycgco 8
  :ycocg 8
  :bt2020-ncl 9
  :bt2020-cl 10
  :smpte2085 11
  :chroma-derived-ncl 12
  :chroma-derived-cl 13
  :ictcp 14
  :ipt-c2 15
  :ycgco-re 16
  :ycgco-ro 17)

(define-alien-enum (av-color-range int)
  :unspecified 0
  :mpeg 1
  :jpeg 2)

(define-alien-enum (av-chroma-location int)
  :unspecified 0
  :left 1
  :center 2
  :topleft 3
  :top 4
  :bottomleft 5
  :bottom 6)

;; opt.h
(define-opaque av-option-ranges)

(define-alien-enum (av-option-type int)
  :flags 1
  :int 2
  :int64 3
  :double 4
  :float 5
  :string 6
  :rational 7
  :binary 8
  :dict 9
  :uint64 10
  :const 11
  :image-size 12
  :pixel-fmt 13
  :sample-fmt 14
  :video-rate 15
  :duration 16
  :color 17
  :bool 18
  :chlayout 19
  :uint 20
  :flag-array (ash 1 16))

(define-alien-enum (av-opt-flag int)
  :encoding-param (ash 1 0)
  :decoding-param (ash 1 1)
  :audio-param (ash 1 3)
  :video-param (ash 1 4)
  :subtitle-param (ash 1 5)
  :export (ash 1 6)
  :readonly (ash 1 7)
  :bsf-param (ash 1 8)
  :runtime-param (ash 1 15)
  :filtering-param (ash 1 16)
  :deprecated (ash 1 17)
  :child-consts (ash 1 18))

(define-alien-type av-option-array-def
    (struct av-option-array-def
      (def c-string)
      (size-min unsigned)
      (size-max unsigned)
      (sep char)))

(define-alien-type av-option
    (struct av-option
      (name c-string)
      (help c-string)
      (offset int)
      (type av-option-type)
      (default-val (union nil 
                     (i64 (signed 64))
                     (dbl double)
                     (str c-string)
                     (q av-rational)
                     (arr (* av-option-array-def))))
      (min double)
      (max double)
      (flags int)
      (unit (* char))))

;; buffer.h
(define-opaque av-buffer)

(define-alien-type av-buffer-ref
    (struct av-buffer-ref
      (buffer (* av-buffer))
      (data (* (unsigned 8)))
      (size size-t)))

;; log.h
(define-alien-enum (av-class-category int)
  :na 0
  :input 1
  :output 2
  :muxer 3
  :demuxer 4
  :encoder 5
  :decoder 6
  :filter 7
  :bitstream-filter 8
  :swscaler 9
  :swresampler 10
  :device-video-output 40
  :device-video-input 41
  :device-audio-output 42
  :device-audio-input 43
  :device-output 44
  :device-input 45
  ;; :nb 46 ;; not part of ABI/API
)

(define-alien-type av-class
    (struct av-class
      (class-name c-string)
      (item-name (* (function c-string (* t))))
      (option (* av-option))
      (version int)
      (log-level-offset-offset int)
      (parent-log-context-offset int)
      (category av-class-category)
      (get-category (* (function av-class-category (* t))))
      (query-ranges (* (function int (* (* av-option-ranges)) (* t) c-string int)))
      (child-next (* (function void (* t) (* t))))
                                        ; (* av-class)
      (child-class-iterate (* (function (* t) (* (* t)))))))

;;; avformat
(define-opaque av-format-context)
(define-opaque av-frame)
(define-opaque av-codec-tag)

(defconstant +av-num-data-pointers+ 8)

(define-alien-enum (av-io-data-marker-type int)
  :header 0
  :sync-point 1
  :boundary-point 2
  :unknown 3
  :trailer 4
  :flush-point 5)

(define-alien-type av-io-context
    (struct av-io-context
      (av-class (* av-class))
      (buffer (* unsigned-char))
      (buffer-size int)
      (buf-ptr (* unsigned-char))
      (buf-end (* unsigned-char))
      (opaque (* t))
      (read-packet (* (function int (* t) (* (unsigned 8)) int)))
      (write-packet (* (function int (* t) (* (unsigned 8)) int)))
      (seek (* (function (signed 64) (* t) (signed 64) int)))
      (pos (signed 64))
      (eof-reached int)
      (error int)
      (write-flag int)
      (max-packet-size int)
      (min-packet-size int)
      (checksum unsigned-long)
      (checksum-ptr (* unsigned-char))
      (update-checksum (* (function unsigned-long unsigned-long (* (unsigned 8)) unsigned-int)))
      (read-pause (* (function int (* t) int)))
      (read-seek (* (function (signed 64) (* t) int (signed 64) int)))
      (seekable int)
      (direct int)
      (protocol-whitelist c-string)
      (protocol-blacklist c-string)
      (write-data-type (* (function int (* t) (* (unsigned 8)) int av-io-data-marker-type (signed 64))))
      (ignore-boundary-point int)
      (buf-ptr-max (* unsigned-char))
      (bytes-read (signed 64))
      (bytes-written (signed 64))))

(define-alien-type av-probe-data
    (struct av-probe-data
      (filename c-string)
      (buf (* unsigned-char))
      (buf-size int)
      (mime-type c-string)))

(define-alien-routine av-get-packet int
  (s (* av-io-context))
  (pkt (* av-packet))
  (size int))

(define-alien-routine av-append-packet int
  (s (* av-io-context))
  (pkt (* av-packet))
  (size int))

(defconstant +avprobe-score-max+ 100)
(define-alien-enum (avprobe-score int)
  :retry (/ +avprobe-score-max+ 4)
  :stream-retry (- (/ +avprobe-score-max+ 4) 1)
  :extension 50
  :mime 75
  :max +avprobe-score-max+)

(defconstant +avprobe-padding-size+ 32)

(define-alien-enum (avfmt int)
  :nofile #x0001
  :neednumber #x0002
  :experimental #x0004
  :show-ids #x0008
  :globalheader #x0040
  :notimestamps #x0080
  :generic-index #x0100
  :ts-discont #x0200
  :variable-fps #x0400
  :nodimensions #x0800
  :nostreams #x1000
  :nobinsearch #x2000
  :nogensearch #x4000
  :no-byte-seek #x8000
  :ts-nonstrict #x20000
  :ts-negative #x40000
  :seek-to-pts #x4000000)

(define-alien-type av-output-format
    (struct av-output-format
      (name c-string)
      (long-name c-string)
      (mime-type c-string)
      (extensions c-string)
      (audio-codec av-codec-id)
      (video-codec av-codec-id)
      (subtitle-codec av-codec-id)
      (flags int)
      (codec-tags (* (* av-codec-tag)))
      (priv-class (* av-class))))

(define-alien-type av-input-format
    (struct av-input-format
      (name c-string)
      (long-name c-string)
      (flags int)
      (extensions c-string)
      (codec-tag (* (* av-codec-tag)))
      (priv-class (* av-class))
      (mime-type c-string)))

(define-alien-routine avformat-alloc-context (* av-format-context))
(define-alien-routine avformat-free-context void (ctx (* av-format-context)))
(define-alien-routine avformat-init-output int (s (* av-format-context)) (options (* (* av-dictionary))))
(define-alien-routine av-write-frame int (s (* av-format-context)) (pkt (* av-packet)))
(define-alien-routine av-guess-format (* av-output-format)
  (short-name c-string)
  (filename c-string)
  (mime-type c-string))
(define-alien-routine av-guess-codec av-codec-id
  (fmt (* av-output-format))
  (short-name c-string)
  (filename c-string)
  (mime-type c-string)
  (type av-media-type))
(define-alien-routine av-get-output-timestamp int 
  (s (* av-format-context)) 
  (stream int)
  (dts (* (signed 64)))
  (wall (* (signed 64))))
(define-alien-routine av-codec-get-id av-codec-id
  (tags (* (* av-codec-tag)))
  (tag unsigned-int))
(define-alien-routine av-codec-get-tag unsigned-int
  (tags (* (* av-codec-tag)))
  (id av-codec-id))
(define-alien-routine av-dump-format void
  (ic (* av-format-context))
  (index int)
  (url c-string)
  (is-output int))
(define-alien-routine avformat-get-riff-video-tags (* av-codec-tag))
(define-alien-routine avformat-get-riff-audio-tags (* av-codec-tag))
(define-alien-routine avformat-get-mov-video-tags (* av-codec-tag))
(define-alien-routine avformat-get-mov-audio-tags (* av-codec-tag))
(define-alien-routine avformat-open-input int 
  (ps (* (* av-format-context)))
  (url c-string)
  (fmt (* av-input-format))
  (options (* (* av-dictionary))))
(define-alien-routine avformat-write-header int
  (s (* av-format-context))
  (options (* (* av-dictionary))))
(define-alien-routine av-find-default-stream-index int (s (* av-format-context)))
(define-alien-routine avformat-network-init int)
(define-alien-routine avformat-network-deinit int)
(define-alien-routine avformat-get-class (* av-class))
(define-alien-routine avformat-stream-group-get-class (* av-class))
;; (define-alien-routine avformat-stream-group-name c-string (type av-stream-group-params-type))
;; (define-alien-routine avformat-new-stream (* av-stream) (s (* av-format-context)) (c (* av-codec)))
;; (define-alien-routine avformat-new-program (* av-program) (s (* av-format-context)) (id int))

(define-alien-routine av-opt-set-defaults void (s (* t)))
(define-alien-routine av-opt-set int (obj (* t)) (name c-string) (val c-string) (search-flags int))
(define-alien-routine av-opt-set-int int (obj (* t)) (name c-string) (val int) (search-flags int))
(define-alien-routine av-opt-set-double int (obj (* t)) (name c-string) (val double) (search-flags int))
;; (define-alien-routine av-opt-set-q int (obj (* t)) (name c-string) (val av-rational) (search-flags int))
(define-alien-routine av-opt-set-image-size int (obj (* t)) (name c-string) (val (* unsigned-char)) (size int) (search-flags int))
(define-alien-routine av-opt-set-pixel-fmt int (obj (* t)) (name c-string) (fmt av-pixel-format) (search-flags int))
(define-alien-routine av-opt-set-sample-fmt int (obj (* t)) (name c-string) (fmt av-sample-format) (search-flags int))
;; (define-alien-routine av-opt-set-video-rate int (obj (* t)) (name c-string) (val av-rational) (search-flags int))
(define-alien-routine av-opt-set-chlayout int (obj (* t)) (name c-string) (val (* av-channel-layout)) (search-flags int))
(define-alien-routine av-opt-set-dict-val int (obj (* t)) (name c-string) (val (* av-dictionary)) (search-flags int))
(define-alien-routine av-opt-set-array int (obj (* t)) (name c-string) (search-flags int)
  (start-elem unsigned-int) (nb-elems unsigned-int) (val-type av-option-type) (val (* t)))

(define-alien-routine av-opt-get int (obj (* t)) (name c-string) (search-flags int) (out-val (* (* unsigned-char))))
(define-alien-routine av-opt-get-int int (obj (* t)) (name c-string) (search-flags int) (out-val (* int)))
(define-alien-routine av-opt-get-double int (obj (* t)) (name c-string) (search-flags int) (out-val (* double)))
(define-alien-routine av-opt-get-q int (obj (* t)) (name c-string) (search-flags int) (out-val (* av-rational)))
(define-alien-routine av-opt-get-image-size int (obj (* t)) (name c-string) (search-flags int) (w-out (* int)) (h-out (* int)))
(define-alien-routine av-opt-get-pixel-fmt int (obj (* t)) (name c-string) (search-flags int) (out-val (* av-pixel-format)))
(define-alien-routine av-opt-get-sample-fmt int (obj (* t)) (name c-string) (search-flags int) (out-val (* av-sample-format)))
(define-alien-routine av-opt-get-video-rate int (obj (* t)) (name c-string) (search-flags int) (out-val (* av-rational)))
(define-alien-routine av-opt-get-chlayout int (obj (* t)) (name c-string) (search-flags int) (out-val (* av-channel-layout)))
(define-alien-routine av-opt-get-dict-val int (obj (* t)) (name c-string) (search-flags int) (out-val (* (* av-dictionary))))
(define-alien-routine av-opt-get-array-size int (obj (* t)) (name c-string) (search-flags int) (out-val (* unsigned-int)))
(define-alien-routine av-opt-get-array int (obj (* t)) (name c-string) (search-flags int) (start-elem unsigned-int) (nb-elems unsigned-int) (out-type av-option-type) (out-val (* t)))
(define-alien-routine av-opt-flag-is-set int (obj (* t)) (field-name c-string) (flag-name c-string))
(define-alien-routine av-free void (obj (* t)))

;;; avcodec
(define-alien-enum (av-codec-flag int)
  :unaligned (ash 1 0)
  :qscale (ash 1 1)
  :4mv (ash 1 2)
  :output-corrupt (ash 1 3)
  :qpel (ash 1 4)
  ;; :dropchanged (ash 1 5)
  :recon-frame (ash 1 6)
  :copy-opaque (ash 1 7)
  :frame-duration (ash 1 8)
  :pass1 (ash 1 9)
  :pass2 (ash 1 10)
  :loop-filter (ash 1 11)
  :gray (ash 1 13)
  :psnr (ash 1 15)
  :interlaced-dct (ash 1 18)
  :low-delay (ash 1 19)
  :global-header (ash 1 22)
  :bitexact (ash 1 23)
  :ac-pred (ash 1 24)
  :interlaced-me (ash 1 29)
  :closer-gop (ash 1 31))

(define-alien-enum (av-codec-flag2 int)
  :fast (ash 1 0)
  :no-output (ash 1 2)
  :local-header (ash 1 3)
  :chunks (ash 1 15)
  :ignore-crop (ash 1 16)
  :show-all (ash 1 22)
  :export-mvs (ash 1 28)
  :skip-manual (ash 1 29)
  :ro-flush-noop (ash 1 30)
  :icc-profiles (ash 1 31))

(define-alien-enum (av-codec-export-data int)
  :mvs (ash 1 0)
  :prft (ash 1 1)
  :video-enc-params (ash 1 2)
  :film-grain (ash 1 3)
  :enhancements (ash 1 4))

(defconstant +av-get-buffer-flag-ref+ (ash 1 0))
(defconstant +av-get-encode-buffer-flag-ref+ (ash 1 0))

(define-alien-enum (ff-cmp int)
  :sad 0
  :sse 1
  :satd 2
  :dct 3
  :psnr 4
  :bit 5
  :rd 6
  :zero 7
  :vsad 8
  :vsse 9
  :nsse 10
  :w53 11
  :w97 12
  :dctmax 13
  :dct264 14
  :media-sad 15
  :chroma 256)

(define-alien-enum (ff-mb-decision int)
  :simple 0
  :bits 1
  :rd 2)

(defconstant +ff-compression-default+ -1)

(define-alien-enum (ff-bug int)
  :autodetect 1
  :xvid-ilace 4
  :ump4 8
  :no-padding 16
  :amv 32
  :qpel-chroma 64
  :std-qpel 128
  :qpel-chroma2 256
  :direct-blocksize 512
  :edge 1024
  :hpel-chroma 2048
  :dc-clip 4096
  :ms 8192
  :truncated 16384
  :iedge 32768)

(define-alien-enum (ff-ec int)
  :guess-mvs 1
  :deblock 2
  :favor-inter 256)

(define-alien-enum (ff-debug int)
  :pict-info 1
  :rc 2
  :bitstream 4
  :mb-type 8
  :qp 16
  :dct-coeff #x00000040
  :skip #x00000080
  :startcode #x00000100
  :er #x00000400
  :mmco #x00000800
  :bugs #x00001000
  :buffers #x00008000
  :threads #x00010000
  :green-md #x00800000
  :nomc #x01000000)

(define-alien-enum (ff-dct int)
  :auto 0
  :fastint 1
  :int 2
  :mmx 3
  :altivec 5
  :faan 6
  :neon 7)

(define-alien-enum (ff-idct int)
  :auto 0
  :int 1
  :simple 2
  :simplemmx 3
  :arm 7
  :altivec 8
  :simplearm 10
  :xvid 14
  :simplearmv5te 16
  :simplearmv6 17
  :faan 20
  :simpleneon 22
  :simpleauto 128)

(define-alien-enum (ff-thread int)
  :frame 1
  :slice)

;; (define-alien-enum (ff-profile int) ;; see defs.h

(define-alien-enum (ff-codec-property int)
  :lossless #x00000001
  :closed-captions #x00000002
  :film-grain #x00000004)


(define-alien-enum (ff-sub-charenc-mode int)
  :do-nothing -1
  :automatic 0
  :pre-decoder 1
  :ignore 2)

;; AVHWAccel..
(define-alien-enum (av-subtitle-type int)
  :none 0
  :bitmap 1
  :text 2
  :ass 3)

(defconstant +av-subtitle-flag-forced+ #x00000001)

(define-alien-enum (av-codec-config int)
  :pix-format 0
  :frame-rate 1
  :sample-rate 2
  :sample-format 3
  :channel-layout 4
  :color-range 5
  :color-space 6)

(define-alien-enum (av-picture-structure int)
  :unknown 0
  :top-field 1
  :bottom-field 2
  :frame 3)

(define-alien-enum (parser-flag int)
  :complete-frames #x0001
  :once #x0002
  :fetched-offset #x0004
  :codec-ts #x1000)

(define-opaque av-codec-parameters)

;; TODO 2025-04-07: 
(define-alien-enum (av-codec-id int)
  :none 0
  :mpeg1video 1
  :mpeg2video 2
  :h261 3
  :h263 4
  :rv10 5
  :rv20 6)

(define-alien-type av-subtitle-rect
  (struct av-subtitle-rect
    (x int)
    (y int)
    (w int)
    (h int)
    (nb-colors int)))

(define-alien-type av-subtitle
    (struct av-subtitle
      (format (unsigned 16))
      (start-display-time (unsigned 32))
      (end-display-time (unsigned 32))
      (num-rects unsigned)
      (rects (* (* av-subtitle-rect)))
      (pts (signed 64))))

(define-alien-enum (av-field-order int)
  :unknown 0
  :progressive 1
  :tt 2
  :bb 3
  :tb 4
  :bt 5)

(define-alien-enum (av-packet-side-data-type int)
  :palette 0
  :new-extradata 1
  :param-change 2
  :h263-mb-info 3
  :replaygain 4
  :displaymatrix 5
  :stereo3d 6
  :audio-service-type 7
  :quality-stats 8
  :fallback-track 9
  :cpb-properties 10
  :skip-samples 11
  :jp-dualmono 12
  :strings-metadata 13
  :subtitle-position 14
  :matroska-blockadditional 15
  :webvtt-identifier 16
  :webvtt-settings 17
  :metadata-update 18
  :mpegts-stream-id 19
  :mastering-display-metadata 20
  :spherical 21
  :content-light-level 22
  :data-a53-cc 23
  :encryption-init-info 24
  :encryption-info 25
  :afd 26
  :prft 27
  :icc-profile 28
  :dovi-conf 29
  :s12m-timecode 30
  :dynamic-hdr10-plus 31
  :iamf-mix-gain-param 32
  :iamf-demixing-info-param 33
  :iamf-recon-gain-info-param 34
  :ambient-viewing-environment 35
  :frame-cropping 36
  :lcevc 37
  :nb 38)

(define-alien-enum (av-channel-order int)
  :unspec 0
  :native 1
  :custom 2
  :ambisonic 3)

(define-alien-enum (av-channel int)
  :none -1
  :front-left 0
  :front-right 1
  :front-center 2
  :low-frequency 3
  :back-left 4
  :back-right 5
  :front-left-of-center 6
  :front-right-of-center 7
  :back-center 8
  :side-left 9
  :side-right 10
  :top-center 11
  :top-front-left 12
  :top-front-right 13
  :top-front-center 14
  :top-back-left 15
  :top-back-center 16
  :top-back-right 17
  :stereo-left 29
  :stereo-right 30
  :wide-left 31
  :wide-right 32
  :surround-direct-left 33
  :surround-direct-right 34
  :low-frequency-2 35
  :top-side-left 36
  :top-side-right 37
  :bottom-front-center 38
  :bottom-front-left 39
  :bottom-front-right 40
  :side-surround-left 41
  :side-surround-right 42
  :top-surround-left 43
  :top-surround-right 44
  :unused #x200
  :unknown #x300
  :ambisonic-base #x400
  :ambisonic-end #x7ff)

(define-alien-enum (av-audio-service-type int)
  :main 0
  :effects 1
  :visually-impaired 2
  :hearing-impaired 3
  :dialogue 4
  :commentary 5
  :emergency 6
  :voice-over 7
  :karaoke 8)

(define-alien-enum (av-discard int)
  :none -16
  :default 0
  :nonref 8
  :bidir 16
  :nonintra 24
  :nonkey 32
  :all 48)

(define-alien-enum (av-frame-side-data-type int)
  :panscan 0
  :a53-cc 1
  :stereo3d 2
  :matrixencoding 3
  :downmix-info 4
  :replaygain 5
  :displaymatrix 6
  :afd 7
  :motion-vectors 8
  :skip-samples 9
  :audio-service-type 10
  :mastering-display-metadata 11
  :gop-timecode 12
  :spherical 13
  :content-light-level 14
  :icc-profile 15
  :s12m-timecode 16
  :dynamic-hdr-plus 17
  :regions-of-interest 18
  :video-enc-params 19
  :sei-unregistered 20
  :film-grain-params 21
  :detection-bboxes 22
  :dovi-rpu-buffer 23
  :dovi-metadata 24
  :dynamic-hdr-vivid 25
  :ambient-viewing-environment 26
  :video-hint 27
  :lcevc 28
  :view-id 29)

(define-alien-type av-profile
    (struct av-profile
      (profile int)
      (name c-string)))

(define-alien-type av-packet-side-data
    (struct av-packet-side-data
      (data (* (unsigned 8)))
      (size size-t)
      (type av-packet-side-data-type)))

(define-alien-type av-packet
    (struct av-packet
      (buf (* av-buffer-ref))
      (pts (signed 64))
      (dts (signed 64))
      (data (* (unsigned 8)))
      (size int)
      (stream-index int)
      (flags int)
      (side-data (* av-packet-side-data))
      (side-data-elems int)
      (duration (signed 64))
      (pos (signed 64))
      (opaque (* t))
      (opaque-ref (* av-buffer-ref))
      (time-base av-rational)))

(defconstant +av-parser-pts-nb+ 4)

(define-alien-type av-codec-parser-context
  (struct av-codec-parser-context
    (priv-data (* t))
    ;; (* av-codec-parser)
    (parser (* t))
    (frame-offset (signed 64))
    (cur-offset (signed 64))
    (next-frame-offset (signed 64))
    (pict-type int)
    (repeat-pict int)
    (pts (signed 64))
    (dts (signed 64))
    (last-pts (signed 64))
    (last-dts (signed 64))
    (fetch-timestamp int)
    (cur-frame-start-index int)
    (cur-frame-offset (array (signed 64) #.+av-parser-pts-nb+))
    (cur-frame-pts (array (signed 64) #.+av-parser-pts-nb+))
    (cur-frame-dts (array (signed 64) #.+av-parser-pts-nb+))
    (flags int)
    (offset (signed 64))
    (cur-frame-end (array (signed 64) #.+av-parser-pts-nb+ ))
    (key-frame int)
    (dts-sync-point int)
    (dts-ref-dts-delta int)
    (pts-dts-delta int)
    (cur-frame-pos (array (signed 64) #.+av-parser-pts-nb+))
    (pos (signed 64))
    (last-pos (signed 64))
    (duration int)
    (field-order av-field-order)
    (picture-structure av-picture-structure)
    (output-picture-number int)
    (width int)
    (height int)
    (coded-width int)
    (coded-height int)
    (format int)))

(define-alien-type av-codec-parser
    (struct av-codec-parser
      (codec-ids (array int 7))
      (priv-data-size int)
      (parser-init (* (function int (* av-codec-parser-context))))
      (parser-parse (* (function int 
                           (* av-codec-parser-context)
                           (* av-codec-context)
                         (* (* unsigned-char))
                         (* int)
                         (* unsigned-char)
                         int)))
      (parser-close (* (function void (* av-codec-parser-context))))
      (split (* (function int (* av-codec-context) (* unsigned-char) int)))))

(define-alien-type av-codec
    (struct av-codec
      (name c-string)
      (long-name c-string)
      (type av-media-type)
      (id av-codec-id)
      (capabilities int)
      (max-lowres unsigned-char)
      ;; deprecated
      (supported-framerates (* av-rational))
      (pix-fmts (* av-pixel-format))
      (supported-samplerates (* int))
      (sample-fmts (* av-sample-format))
      ;; end deprecated
      (priv-class (* av-class))
      (profiles (* av-profile))))

(define-opaque av-codec-internal) ;; ?where's the def?

(define-alien-type av-channel-custom
    (struct av-channel-custom
      (id av-channel)
      (name (array char 16))
      (opaque (* t))))

(define-alien-type av-channel-layout
    (struct av-channel-layout
      (order av-channel-order)
      (nb-channels int)
      (u (union nil
           (mask (unsigned 64))
           (map (* av-channel-custom))))
      (opaque (* t))))

(define-alien-type rc-override
    (struct rc-override
      (start-frame int)
      (end-frame int)
      (qscale int)
      (quality-factor float)))

(define-alien-type av-codec-descriptor
    (struct av-codec-descriptor
      (id av-codec-id)
      (type av-media-type)
      (name c-string)
      (long-name c-string)
      (props int)
      (mime-types (* c-string))
      (profiles (* av-profile))))

(define-alien-type av-frame-side-data
    (struct av-frame-side-data
      (type av-frame-side-data-type)
      (data (* (unsigned 8)))
      (size size-t)
      (metadata (* av-dictionary))
      (buf (* av-buffer-ref))))

;; main api structure
(define-alien-type av-codec-context
  (struct av-codec-context
    (av-class (* av-class))
    (log-level-offset int)
    (codec-type av-media-type)
    (codec (* av-codec))
    (codec-id av-codec-id)
    (codec-tag unsigned-int)
    (priv-data (* t))
    (internal (* av-codec-internal))
    (opaque (* t))
    (bit-rate (signed 64))
    (flags int)
    (flags2 int)
    (extradata (* unsigned-char))
    (extradata-size int)
    (time-base av-rational)
    (pkt-timebase av-rational)
    (framerate av-rational)
    (delay int)
    (width int)
    (height int)
    (codec-width int)
    (codec-height int)
    (sample-aspect-ratio av-rational)
    (pix-fmt av-pixel-format)
    (sw-pix-fmt av-pixel-format)
    (color-primaries av-color-primaries)
    (color-trc av-color-transfer-characteristic)
    (colorspace av-color-space)
    (color-range av-color-range)
    (chroma-sample-location av-chroma-location)
    (field-order av-field-order)
    (refs int)
    (has-b-frames int)
    (slice-flags int)
    ;; (* av-codec-context)
    (draw-horiz-band (* (function void (* t)
                            (* av-frame)
                          (array int #.+av-num-data-pointers+)
                          int int int)))
    ;; (* av-codec-context)
    (get-format (* (function av-pixel-format (* t) (* av-pixel-format))))
    (max-b-frames int)
    (b-quant-factor float)
    (b-quant-offset float)
    (i-quant-factor float)
    (i-quant-offset float)
    (lumi-masking float)
    (temporal-cplx-masking float)
    (spatial-cplx-masking float)
    (p-masking float)
    (dark-masking float)
    (nsse-weight int)
    (me-cmp int)
    (me-sub-cmp int)
    (mb-cmp int)
    (ildct-cmp int)
    (dia-size int)
    (last-predictor-count int)
    (me-pre-cmp int)
    (pre-dia-size int)
    (me-subpel-quality int)
    (me-range int)
    (mb-decision int)
    (intra-matrix (* (unsigned 16)))
    (inter-matrix (* (unsigned 16)))
    (chroma-intra-matrix (* (unsigned 16)))
    (intra-dc-precision int)
    (mb-lmin int)
    (mb-lmax int)
    (bidir-refine int)
    (keyint-min int)
    (gop-size int)
    (mv0-threshold int)
    (slices int)
    (smaple-rate int)
    (sample-fmt av-sample-format)
    (ch-layout av-channel-layout)
    (frame-size int)
    (block-align int)
    (cutoff int)
    (audio-service-type av-audio-service-type)
    (request-sample-fmt av-sample-format)
    (initial-padding int)
    (trailing-padding int)
    (seek-preroll int)
    ;; (* av-codec-context)
    (get-buffer2 (* (function int (* t) (* av-frame) int)))
    (bit-rate-tolerance int)
    (global-quality int)
    (compression-level int)
    (qcompress float)
    (qblur float)
    (qmin int)
    (qmax int)
    (max-qdiff int)
    (rc-buffer-size int)
    (rc-override-count int)
    (rc-override (* rc-override))
    (rc-max-rate (signed 64))
    (rc-min-rate (signed 64))
    (rc-max-available-vbv-use float)
    (rc-min-vbv-overflow-use float)
    (rc-initial-buffer-occupancy int)
    (trellis int)
    (stats-out (* char))
    (stats-in (* char))
    (workaround-bugs int)
    (strict-std-compliance int)
    (error-concealment int)
    (debug int)
    (err-recognition int)
    (hwaccel (* t)) ;; (* av-hw-accel)
    (hwaccel-context (* t))
    (hw-frames-ctx (* av-buffer-ref))
    (hw-device-ctx (* av-buffer-ref))
    (hwaccel-flags int)
    (extra-hw-frames int)
    (error (array (unsigned 64) #.+av-num-data-pointers+))
    (dct-algo int)
    (idct-algo int)
    (bits-per-coded-sample int)
    (bits-per-raw-sample int)
    (thread-count int)
    (thread-type int)
    (active-thread-type int)
    ;; (* (function int (* av-codec-context) (* (function int (* av-codec-context) (* t))) (* t) (* int) int int))
    (execute (* (function int (* t) (* (function int (* t) (* t))) (* t) (* int) int int)))
    ;; (* (function int (* av-codec-context) (* (function int (* av-codec-context) (* t) int int)) (* t) (* int) int))
    (execute2 (* (function int (* t) (* (function int (* t) (* t) int int)) (* t) (* int) int)))
    (profile int)
    (level int)
    (properties unsigned)
    (skip-loop-filter av-discard)
    (skip-idct av-discard)
    (skip-frame av-discard)
    (skip-alpha int)
    (skip-top int)
    (skip-bottom int)
    (lowres int)
    (codec-descriptor (* av-codec-descriptor))
    (sub-charenc (* char))
    (sub-charenc-mode int)
    (subtitle-header-size int)
    (subtitle-header (* unsigned-char))
    (dump-separator (* unsigned-char))
    (codec-whitelist (* char))
    (codec-side-data (* av-packet-side-data))
    (nb-coded-side-data int)
    (export-side-data int)
    (max-pixels (signed 64))
    (apply-cropping int)
    (discard-damaged-percentage int)
    (max-samples (signed 64))
    ;; (* av-codec-context)
    (get-encoded-buffer (* (function int (* t) (* av-packet) int)))
    (frame-num (signed 64))
    (side-data-prefer-packet (* int))
    (nb-side-data-prefer-packet unsigned)
    (decoded-side-data (* (* av-frame-side-data)))
    (nb-decoded-side-data int)))

(define-alien-routine avcodec-free-context void (ctx (* (* av-codec-context))))
(define-alien-routine avcodec-close int (ctx (* av-codec-context)))
(define-alien-routine avsubtitle-free void (sub (* av-subtitle)))
(define-alien-routine avcodec-get-class (* av-class))
(define-alien-routine avcodec-alloc-context3 (* av-codec-context) (codec (* av-codec)))
(define-alien-routine avcodec-get-subtitle-rect-class (* av-class))
(define-alien-routine avcodec-open2 int (avctx (* av-codec-context)) (codec (* av-codec)) (options (* (* av-dictionary))))
(define-alien-routine av-codec-iterate (* av-codec) (opaque (* (* t))))
(define-alien-routine avcodec-find-decoder (* av-codec) (id av-codec-id))
(define-alien-routine avcodec-find-decoder-by-name (* av-codec) (name c-string))
(define-alien-routine avcodec-find-encoder (* av-codec) (id av-codec-id))
(define-alien-routine avcodec-find-encoder-by-name (* av-codec) (name c-string))
(define-alien-routine av-codec-is-encoder int (codec (* av-codec)))
(define-alien-routine av-codec-is-decoder int (codec (* av-codec)))
(define-alien-routine av-get-profile-name c-string (codec (* av-codec)) (profile int))
