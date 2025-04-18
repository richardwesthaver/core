;;; pkg.lisp --- FFmpeg FFI

;; Bindings for FFmpeg libraries (libavcodec, libavutil, etc)

;;; Commentary:

;; ref: https://github.com/FFmpeg/FFmpeg/tree/master/doc/examples

;;; Code:
(defpackage :ffmpeg
  (:use :cl :std :sb-alien)
  (:export :load-avcodec :load-avutil :load-avformat
   :load-swresample :load-swscale
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
   :av-codec-id :av-codec-id*
   :av-codec :av-codec-context
   :av-dictionary :av-class
   :av-subtitle
   :av-get-pcm-codec
   :avcodec-profile-name
   :av-get-exact-bits-per-sample
   :av-get-bits-per-sample
   :avcodec-get-name
   :avcodec-get-type
   :avcodec-descriptor-get
   :avcodec-descriptor-next
   :avcodec-descriptor-get-by-name
   :av-format-context
   :av-frame
   :av-codec-tag
   :av-dictionary-entry
   :av-rational
   :av-sample-format
   :av-pixel-format
   :av-color-primaries
   :av-color-transfer-characteristic
   :av-color-space
   :av-color-range
   :av-chroma-location
   :av-option-ranges
   :av-option-type
   :av-opt-flag
   :av-option-array-def
   :av-option
   :av-buffer
   :av-buffer-ref
   :av-class-category
   :avformat-open-input
   :avformat-get-riff-video-tags
   :avformat-get-riff-audio-tags
   :avformat-get-mov-video-tags
   :avformat-get-mov-audio-tags
   :avformat-write-header
   :avformat-network-init
   :avformat-network-deinit
   :avformat-get-class
   :avformat-stream-group-get-class
   :av-find-default-stream-index
   :av-dump-format
   :av-codec-get-tag
   :av-codec-get-id
   :av-get-output-timestamp
   :av-guess-codec
   :av-guess-format
   :av-write-frame
   :avformat-init-output
   :av-input-format
   :av-output-format
   :avformat-find-stream-info
   :av-dict-iterate
   :av-dict-get
   :av-dict-set-int
   :av-dict-set
   :av-dict-count
   :avformat-close-input
   :avformat-flush
   :av-read-play
   :av-read-pause
   :av-packet-alloc
   :av-packet
   :av-parser-init
   :av-codec-parser-context
   :av-codec-parser
   :av-packet-free
   :av-parser-close
   :av-parser-iterate
   :av-frame-free
   :av-log-set-level
   :av-log-get-level
   :+av-input-buffer-padding-size+
   :+av-nopts-value+
   :+av-time-base+
   :av-frame-alloc
   :av-parser-parse2
   :avcodec-send-packet
   :avcodec-send-frame
   :avcodec-receive-frame
   :avcodec-receive-packet
   :av-get-bytes-per-sample
   :averror
   :averror*
   :+av-error-max-string-size+
   :av-strerror
   :av-make-error-string
   :av-channel-layout-default
   :av-channel-layout))
           
(in-package :ffmpeg)

(define-alien-loader :avcodec "/usr/lib/")
(define-alien-loader :avutil "/usr/lib/")
(define-alien-loader :avformat "/usr/lib/")
(define-alien-loader :avfilter "/usr/lib/")
(define-alien-loader :swresample "/usr/lib/")
(define-alien-loader :swscale "/usr/lib/")

;;; version
(define-alien-routine avcodec-version unsigned)
(define-alien-routine avformat-version unsigned)
(define-alien-routine avutil-version unsigned)
(define-alien-routine avfilter-version unsigned)

;;; avutil
(defconstant +av-nopts-value+ #x8000000000000000)
(defconstant +av-time-base+ 1000000)
(eval-when (:compile-toplevel)
  (defun mktag (a b c d)
    (logior a (ash b 8) (ash c 16) (ash d 24)))
  (defun fferrtag (a b c d)
    (- (mktag a b c d)))
  (defun fferrtag* (str)
    (- (mktag 
        (char-code (schar str 0)) 
        (char-code (schar str 1)) 
        (char-code (schar str 2)) 
        (char-code (schar str 3))))))

(defconstant +av-error-max-string-size+ 64)

(define-alien-enum (averror int)
  :bsf-not-found (fferrtag #xf8 (char-code #\B) (char-code #\S) (char-code #\F))
  :bug (fferrtag* "BUG!")
  :buffer-too-small (fferrtag* "BUFS")
  :decoder-not-found (fferrtag #xf8 (char-code #\D) (char-code #\E) (char-code #\C))
  :demuxer-not-found (fferrtag #xf8 (char-code #\D) (char-code #\E) (char-code #\M))
  :encoder-not-found (fferrtag #xf8 (char-code #\E) (char-code #\N) (char-code #\C))
  :eof (fferrtag* "EOF ")
  :exit (fferrtag* "EXIT")
  :external (fferrtag* "EXT ")
  :filter-not-found (fferrtag #xf8 (char-code #\F) (char-code #\I) (char-code #\L))
  :invaliddata (fferrtag* "INDA")
  :muxer-not-found (fferrtag #xf8 (char-code #\M) (char-code #\U) (char-code #\X))
  :option-not-found (fferrtag #xf8 (char-code #\O) (char-code #\P) (char-code #\T))
  :patchwelcome (fferrtag* "PAWE")
  :protocol-not-found (fferrtag #xf8 (char-code #\P) (char-code #\R) (char-code #\O))
  :stream-not-found (fferrtag #xf8 (char-code #\S) (char-code #\T) (char-code #\R))
  :bug2 (fferrtag* "BUG ")
  :unknown (fferrtag* "UNKN")
  :experimental (- #x2bb2afa8)
  :input-changed (- #x636e6701)
  :output-changed (- #x636e6702)
  :http-bad-request (fferrtag #xf8 (char-code #\4) (char-code #\0) (char-code #\0))
  :http-unauthorized (fferrtag #xf8 (char-code #\4) (char-code #\0) (char-code #\1))
  :http-forbidden (fferrtag #xf8 (char-code #\4) (char-code #\0) (char-code #\3))
  :http-not-found (fferrtag #xf8 (char-code #\4) (char-code #\0) (char-code #\4))
  :http-too-many-requests (fferrtag #xf8 (char-code #\4) (char-code #\2) (char-code #\9))
  :http-other-4xx (fferrtag #xf8 (char-code #\4) (char-code #\X) (char-code #\X))
  :http-server-error (fferrtag #xf8 (char-code #\5) (char-code #\X) (char-code #\X)))

(define-alien-routine av-strerror int (errnum int) (errbuf c-string) (errbuf-size size-t))
;; (define-alien-routine av-make-error-string c-string (errbuf c-string) (errbuf-size size-t) (errnum int))

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
      (val c-string)))

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

(define-alien-routine av-dict-iterate (* av-dictionary-entry) (m (* av-dictionary)) (prev (* av-dictionary-entry)))
(define-alien-routine av-dict-get (* av-dictionary-entry) 
  (m (* av-dictionary))  
  (key c-string)
  (prev (* av-dictionary-entry))
  (flags int))
(define-alien-routine av-dict-count int (m (* av-dictionary)))
(define-alien-routine av-dict-set int (pm (* (* av-dictionary))) (key c-string) (value c-string) (flags int))
(define-alien-routine av-dict-set-int int (pm (* (* av-dictionary))) (key c-string) (value long) (flags int))
;;; avformat
(define-opaque av-codec-tag)

(defconstant +av-num-data-pointers+ 8)

(define-alien-enum (av-io-data-marker-type int)
  :header 0
  :sync-point 1
  :boundary-point 2
  :unknown 3
  :trailer 4
  :flush-point 5)

(define-alien-type av-io-interrupt-cb
  (struct av-io-interrupt-cb
    (callback (* (function int (* t))))
    (opaque (* t))))

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

(define-alien-enum (av-duration-estimation-method int)
  :pts 0
  :stream 1
  :bitrate 2)

(define-alien-enum (av-codec-id int)
  :none 0
  :mpeg1video 1
  :mpeg2video 2
  :h261 3
  :h263 4
  :rv10 5
  :rv20 6
  :mjpeg 7
  :mjpegb 8
  :ljpeg 9
  :sp5x 10
  :jpegls 11
  :mpeg4 12
  :rawvideo 13
  :msmpeg4v1 14
  :msmpeg4v2 15
  :msmpeg4v3 16
  :wmv1 17
  :wmv2 18
  :h263p 19
  :h263i 20
  :flv1 21
  :svq1 22
  :svq3 23
  :dvvideo 24
  :huffyuv 25
  :cyuv 26
  :h264 27
  :indeo3 28
  :vp3 29
  :theora 30
  :asv1 31
  :asv2 32
  :ffv1 33
  :4xm 34
  :vcr1 35
  :cljr 36
  :mdec 37
  :roq 38
  :interplay-video 39
  :xan-wc3 40
  :xan-wc4 41
  :rpza 42
  :cinepak 43
  :ws-vqa 44
  :msrle 45
  :msvideo1 46
  :idcin 47
  :8bps 48
  :smc 49
  :flic 50
  :truemotion1 51
  :vmdvideo 52
  :mszh 53
  :zlib 54
  :qtrle 55
  :tscc 56
  :ulti 57
  :qdraw 58
  :vixl 59
  :qpeg 60
  :png 61
  :ppm 62
  :pbm 63
  :pgm 64
  :pgmyuv 65
  :pam 66
  :ffvhuff 67
  :rv30 68
  :rv40 69
  :vc1 70
  :wmv3 71
  :loco 72
  :wnv1 73
  :aasc 74
  :indeo2 75
  :fraps 76
  :truemotion2 77
  :bmp 78
  :cscd 79
  :mmvideo 80
  :zmbv 81
  :avs 82
  :smackvideo 83
  :nuv 84
  :kmvc 85
  :flashsv 86
  :cavs 87
  :jpeg2000 88
  :vmnc 89
  :vp5 90
  :vp6 91
  :vp6f 92
  :targa 93
  :dsicinvideo 94
  :tiertexseqvideo 95
  :tiff 96
  :gif 97
  :dxa 98
  :dnxhd 99
  :thp 100
  :sgi 101
  :c93 102
  :bethsoftvid 103
  :ptx 104
  :txd 105
  :vp6a 106
  :amv 107
  :vb 108
  :pcx 109
  :sunrast 110
  :indeo4 111
  :indeo5 112
  :mimic 113
  :rl2 114
  :escape124 115
  :dirac 116
  :bfi 117
  :cmv 118
  :motionpixels 119
  :tgv 120
  :tgq 121
  :tqi 122
  :aura 123
  :aura2 124
  :v210x 125
  :tmv 126
  :v210 127
  :dpx 128
  :mad 129
  :frwu 130
  :flashsv2 131
  :cdgraphics 132
  :r210 133
  :anm 134
  :binkvideo 135
  :iff-ilbm 136
  ;; #define AV_CODEC_ID_IFF_BYTERUN1 AV_CODEC_ID_IFF_ILBM
  :iff-byterun1 136
  :kgv1 137
  :yop 138
  :vp8 139
  :pictor 140
  :ansi 141
  :a64-multi 142
  :a64-multi5 143
  :r10k 144
  :mxpeg 145
  :lagarith 146
  :prores 147
  :jv 148
  :dfa 149
  :wmv3image 150
  :vc1image 151
  :utvideo 152
  :bmv-video 153
  :vble 154
  :dxtory 155
  :v410 156
  :xwd 157
  :cdxl 158
  :xbm 159
  :zerocodec 160
  :mss1 161
  :msa1 162
  :tscc2 163
  :mts2 164
  :cllc 165
  :mss2 166
  :vp9 167
  :aic 168
  :escape130 169
  :g2m 170
  :webp 171
  :hnm4-video 172
  :hevc 173
  ;; #define AV_CODEC_ID_H265 AV_CODEC_ID_HEVC
  :h265 173
  :fic 174
  :alias-pix 175
  :brender-pix 176
  :paf-video 177
  :exr 178
  :vp7 179
  :sanm 180
  :sgirle 181
  :mvc1 182
  :mvc2 183
  :hqx 184
  :tdsc 185
  :hq-hqa 186
  :hap 187
  :dds 188
  :dxv 189
  :screenpresso 190
  :rscc 191
  :avs2 192
  :pgx 193
  :avs3 194
  :msp2 195
  :vvc 196
  ;; #define AV_CODEC_ID_H266 AV_CODEC_ID_VVC
  :h266 196
  :y41p 197
  :avrp 198
  :012v 199
  :avui 200
  :targa-y216 201
  :v308 202
  :v408 203
  :yuv4 204
  :avrn 205
  :cpia 206
  :xface 207
  :snow 208
  :smvjpeg 209
  :apng 210
  :daala 211
  :cfhd 212
  :truemotion2rt 213
  :m101 214
  :magicyuv 215
  :sheervideo 216
  :ylc 217
  :psd 218
  :pixlet 219
  :speedhq 220
  :fmvc 221
  :scpr 222
  :clearvideo 223
  :xpm 224
  :av1 225
  :bitpacked 226
  :mscc 227
  :srgc 228
  :svg 229
  :gdv 230
  :fits 231
  :imm4 232
  :prosumer 233
  :mwsc 234
  :wcmv 235
  :rasc 236
  :hymt 237
  :arbc 238
  :agm 239
  :lscr 240
  :vp4 241
  :imm5 242
  :mvdv 243
  :mvha 244
  :cdtoons 245
  :mv30 246
  :notchlc 247
  :pfm 248
  :mobiclip 249
  :photocd 250
  :ipu 251
  :argo 252
  :cri 253
  :simbiosis-imx 254
  :sga-video 255
  :gem 256
  :vbn 257
  :jpegxl 258
  :qoi 259
  :phm 260
  :radiance-hdr 261
  :wbmp 262
  :media100 263
  :vqc 264
  :pdv 265
  :evc 266
  :rtv1 267
  :vmix 268
  :lead 269
  ;; various PCM codecs
  :pcm-s16le 65536
  :pcm-s16be 65537
  :pcm-u16le 65538
  :pcm-u16be 65539
  :pcm-s8 65540
  :pcm-u8 65541
  :pcm-mulaw 65542
  :pcm-alaw 65543
  :pcm-s32le 65544
  :pcm-s32be 65545
  :pcm-u32le 65546
  :pcm-u32be 65547
  :pcm-s24le 65548
  :pcm-s24be 65549
  :pcm-u24le 65550
  :pcm-u24be 65551
  :pcm-s24daud 65552
  :pcm-zork 65553
  :pcm-s16le-planar 65554
  :pcm-dvd 65555
  :pcm-f32be 65556
  :pcm-f32le 65557
  :pcm-f64be 65558
  :pcm-f64le 65559
  :pcm-bluray 65560
  :pcm-lxf 65561
  :s302m 65562
  :pcm-s8-planar 65563
  :pcm-s24le-planar 65564
  :pcm-s32le-planar 65565
  :pcm-s16be-planar 65566
  :pcm-s64le 65567
  :pcm-s64be 65568
  :pcm-f16le 65569
  :pcm-f24le 65570
  :pcm-vidc 65571
  :pcm-sga 65572
  ;; various ADPCM codecs
  :adpcm-ima-qt #x11000
  :adpcm-ima-wav 69633
  :adpcm-ima-dk3 69634
  :adpcm-ima-dk4 69635
  :adpcm-ima-ws 69636
  :adpcm-ima-smjpeg 69637
  :adpcm-ms 69638
  :adpcm-4xm 69639
  :adpcm-xa 69640
  :adpcm-adx 69641
  :adpcm-ea 69642
  :adpcm-g726 69643
  :adpcm-ct 69644
  :adpcm-swf 69645
  :adpcm-yamaha 69646
  :adpcm-sbpro-4 69647
  :adpcm-sbpro-3 69648
  :adpcm-sbpro-2 69649
  :adpcm-thp 69650
  :adpcm-ima-amv 69651
  :adpcm-ea-r1 69652
  :adpcm-ea-r3 69653
  :adpcm-ea-r2 69654
  :adpcm-ima-ea-sead 69655
  :adpcm-ima-ea-eacs 69656
  :adpcm-ea-xas 69657
  :adpcm-ea-maxis-xa 69658
  :adpcm-ima-iss 69659
  :adpcm-g722 69660
  :adpcm-ima-apc 69661
  :adpcm-vima 69662
  :adpcm-afc 69663
  :adpcm-ima-oki 69664
  :adpcm-dtk 69665
  :adpcm-ima-rad 69666
  :adpcm-g726le 69667
  :adpcm-thp-le 69668
  :adpcm-psx 69669
  :adpcm-aica 69670
  :adpcm-ima-dat4 69671
  :adpcm-mtaf 69672
  :adpcm-agm 69673
  :adpcm-argo 69674
  :adpcm-ima-ssi 69675
  :adpcm-zork 69676
  :adpcm-ima-apm 69677
  :adpcm-ima-alp 69678
  :adpcm-ima-mtf 69679
  :adpcm-ima-cunning 69680
  :adpcm-ima-moflex 69681
  :adpcm-ima-acorn 69682
  :adpcm-xmd 69683
  ;; AMR
  :amr-nb #x12000
  :amr-wb 73729
  ;; RealAudio codecs
  :ra-144 #x13000
  :ra-288 77825
  ;; various DPCM codecs
  :roq-dpcm #x14000
  :interplay-dpcm 81921
  :xan-dpcm 81922
  :sol-dpcm 81923
  :sdx2-dpcm 81924
  :gremlin-dpcm 81925
  :derf-dpcm 81926
  :wady-dpcm 81927
  :cbd2-dpcm 81928
  ;; audio codecs
  :mp2 #x15000
  :mp3 86017
  :aac 86018
  :ac3 86019
  :dts 86020
  :vorbis 86021
  :dvaudio 86022
  :wmav1 86023
  :wmav2 86024
  :mace3 86025
  :mace6 86026
  :vmdaudio 86027
  :flac 86028
  :mp3adu 86029
  :mp3on4 86030
  :shorten 86031
  :alac 86032
  :westwood-snd1 86033
  :gsm 86034
  :qdm2 86035
  :cook 86036
  :truespeech 86037
  :tta 86038
  :smackaudio 86039
  :qcelp 86040
  :wavpack 86041
  :dsicinaudio 86042
  :imc 86043
  :musepack7 86044
  :mlp 86045
  :gsm-ms 86046
  :atrac3 86047
  :ape 86048
  :nellymoser 86049
  :musepack8 86050
  :speex 86051
  :wmavoice 86052
  :wmapro 86053
  :wmalossless 86054
  :atrac3p 86055
  :eac3 86056
  :sipr 86057
  :mp1 86058
  :twinvq 86059
  :truehd 86060
  :mp4als 86061
  :atrac1 86062
  :binkaudio-rdft 86063
  :binkaudio-dct 86064
  :aac-latm 86065
  :qdmc 86066
  :celt 86067
  :g723-1 86068
  :g729 86069
  :8svx-exp 86070
  :8svx-fib 86071
  :bmv-audio 86072
  :ralf 86073
  :iac 86074
  :ilbc 86075
  :opus 86076
  :comfort-noise 86077
  :tak 86078
  :metasound 86079
  :paf-audio 86080
  :on2avc 86081
  :dss-sp 86082
  :codec2 86083
  :ffwavesynth 86084
  :sonic 86085
  :sonic-ls 86086
  :evrc 86087
  :smv 86088
  :dsd-lsbf 86089
  :dsd-msbf 86090
  :dsd-lsbf-planar 86091
  :dsd-msbf-planar 86092
  :4gv 86093
  :interplay-acm 86094
  :xma1 86095
  :xma2 86096
  :dst 86097
  :atrac3al 86098
  :atrac3pal 86099
  :dolby-e 86100
  :aptx 86101
  :aptx-hd 86102
  :sbc 86103
  :atrac9 86104
  :hcom 86105
  :acelp-kelvin 86106
  :mpegh-3d-audio 86107
  :siren 86108
  :hca 86109
  :fastaudio 86110
  :msnsiren 86111
  :dfpwm 86112
  :bonk 86113
  :misc4 86114
  :apac 86115
  :ftr 86116
  :wavarc 86117
  :rka 86118
  :ac4 86119
  :osq 86120
  :qoa 86121
  :lc3 86122
  ;; subtitle codecs
  :dvd-subtitle #x17000
  :dvb-subtitle 94209
  :text 94210
  :xsub 94211
  :ssa 94212
  :mov-text 94213
  :hdmv-pgs-subtitle 94214
  :dvb-teletext 94215
  :srt 94216
  :microdvd 94217
  :eia-608 94218
  :jacosub 94219
  :sami 94220
  :realtext 94221
  :stl 94222
  :subviewer1 94223
  :subviewer 94224
  :subrip 94225
  :webvtt 94226
  :mpl2 94227
  :vplayer 94228
  :pjs 94229
  :ass 94230
  :hdmv-text-subtitle 94231
  :ttml 94232
  :arib-caption 94233
  ;; other specific kind of codecs (generally used for attachments)
  :ttf #x18000
  :scte-35 98305
  :epg 98306
  :bintext 98307
  :xbin 98308
  :idf 98309
  :otf 98310
  :smpte-klv 98311
  :dvd-nav 98312
  :timed-id3 98313
  :bin-data 98314
  :smpte-2038 98315
  :lcevc 98316
  :probe #x19000
  :mpeg2ts #x20000
  :mpeg4systems #x20001
  :ffmetadata #x21000
  :wrapped-avframe #x21001
  :vnull #x21002
  :anull #x21003)

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

(define-alien-type av-profile
    (struct av-profile
      (profile int)
      (name c-string)))

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

(define-alien-type av-stream
    (struct av-stream))

(define-alien-type av-stream-group (struct av-stream-group))

(define-alien-type av-chapter
  (struct av-chapter
    (id long)
    (time-base av-rational)
    (start long)
    (end long)
    (metadata (* av-dictionary))))

(define-alien-enum (av-discard int)
  :none -16
  :default 0
  :nonref 8
  :bidir 16
  :nonintra 24
  :nonkey 32
  :all 48)

(define-alien-type av-program 
  (struct av-program
    (id int)
    (flags int)
    (discard av-discard)
    (stream-index (* unsigned-int))
    (nb-stream-indexes unsigned-int)
    (metadata (* av-dictionary))
    (program-num int)
    (pmt-pid int)
    (pcr-pid int)
    (pmt-version int)
    (start-time long)
    (end-time long)
    (pts-wrap-reference long)
    (pts-wrap-behavior int)))

(define-alien-type av-format-control-message
    ;; (* av-format-context)
  (* (function int (* t) int (* t) size-t)))

#|
sizeof(AVFormatContext) must not be used outside libav*, use
avformat_alloc_context() to create an AVFormatContext.

Fields can be accessed through AVOptions (av_opt*), the name string used
matches the associated command line parameter name and can be found in
libavformat/options_table.h.  The AVOption/command line parameter names differ
in some cases from the C structure field names for historic reasons or
brevity.
|#

(define-alien-type av-format-context 
    (struct av-format-context
      (av-class (* av-class))
      (iformat (* av-input-format))
      (oformat (* av-output-format))
      (priv-data (* t))
      (pb (* av-io-context))
      (ctx-flags int)
      (nb-streams unsigned-int)
      (streams (* (* av-stream)))
      (nb-stream-groups unsigned-int)
      (stream-groups (* (* av-stream-group)))
      (nb-chapters unsigned-int)
      (chapters (* (* av-chapter)))
      (url c-string)
      (start-time long)
      (duration long)
      (bit-rate long)
      (packet-size unsigned-int)
      (max-delay int)
      (flags int)
      (probesize long)
      (max-analyze-duration long)
      (key (* unsigned-char))
      (keylen int)
      (nb-programs unsigned-int)
      (programs (* (* av-program)))
      (video-codec-id av-codec-id)
      (audio-codec-id av-codec-id)
      (subtitle-codec-id av-codec-id)
      (data-codec-id av-codec-id)
      (metadata (* av-dictionary))
      (start-time-realtime long)
      (fps-probe-size int)
      (error-recognition int)
      (interrupt-callback av-io-interrupt-cb)
      (debug int)
      (max-streams int)
      (max-index-size unsigned-int)
      (max-picture-buffer unsigned-int)
      (max-interleave-delta long)
      (max-ts-probe int)
      (max-chunk-duration int)
      (max-chunk-size int)
      (max-probe-packets int)
      (strict-std-compliance int)
      (event-flags int)
      (avoid-negative-ts int)
      (audio-preload int)
      (use-wallclock-as-timestamps int)
      (skip-estimate-duration-from-pts int)
      (avio-flags int)
      (duration-estimation-method av-duration-estimation-method)
      (skip-initial-bytes long)
      (correct-ts-overflow unsigned-int)
      (seek2any int)
      (flush-packets int)
      (probe-score int)
      (format-probesize int)
      (codec-whitelist c-string)
      (format-whitelist c-string)
      (protocol-whitelist c-string)
      (protocol-blacklist c-string)
      (io-repositioned int)
      (video-codec (* av-codec))
      (audio-codec (* av-codec))
      (subtitle-codec (* av-codec))
      (data-codec (* av-codec))
      (metadata-header-padding int)
      (opaque (* t))
      (control-message-cb av-format-control-message)
      (output-ts-offset long)
      (dump-separator (* unsigned-char))
      ;; (* av-format-context)
      (io-open (* (function int (* t) (* (* av-io-context)) c-string int (* (* av-dictionary)))))
      ;; (* av-format-context)
      (io-close2 (* (function int (* t) (* av-io-context))))))

(define-alien-type av-open-callback
  (* (function int 
	 (* av-format-context) (* (* av-io-context)) c-string int (* av-io-interrupt-cb) (* (* av-dictionary)))))

;;; avcodec
(defconstant +av-input-buffer-padding-size+ 64)
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

(define-alien-enum (av-picture-type int)
  :none 0
  :i 1
  :p 2
  :b 3
  :s 4
  :si 5
  :sp 6
  :bi 7)

(define-alien-type av-frame
    (struct av-frame
      (data (array unsigned-char 8))
      (linesize (array int 8))
      (extended-data (* (* unsigned-char)))
      (width int)
      (height int)
      (nb-samples int)
      (format int)
      (pict-type av-picture-type)
      (sample-aspect-ratio av-rational)
      (pts long)
      (pkt-dts long)
      (time-base av-rational)
      (quality int)
      (opaque (* t))
      (repeat-pict int)
      (sample-rate int)
      (buf (array av-buffer-ref 8))
      (extended-buf (* (* av-buffer-ref)))
      (nb-extended-buf int)
      (side-data (* (* av-frame-side-data)))
      (nb-side-data int)
      (flags int)
      (color-range av-color-range)
      (color-primaries av-color-primaries)
      (color-trc av-color-transfer-characteristic)
      (colorspace av-color-space)
      (chroma-location av-chroma-location)
      (best-effort-timestamp long)
      (metadata (* av-dictionary))
      (decode-error-flags int)
      (hw-frames-ctx (* av-buffer-ref))
      (opaque-ref (* av-buffer-ref))
      (crop-top size-t)
      (crop-bottom size-t)
      (crop-left size-t)
      (crop-right size-t)
      (private-ref (* av-buffer-ref))
      (ch-layout av-channel-layout)
      (duration long)))
                
      
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

(define-alien-routine av-get-packet int
  (s (* av-io-context))
  (pkt (* av-packet))
  (size int))

(define-alien-routine av-append-packet int
  (s (* av-io-context))
  (pkt (* av-packet))
  (size int))

(define-alien-routine avformat-alloc-context (* av-format-context))
(define-alien-routine avformat-free-context void (ctx (* av-format-context)))
(define-alien-routine avformat-init-output int (s (* av-format-context)) (options (* (* av-dictionary))))
(define-alien-routine avformat-find-stream-info int (ic (* av-format-context)) (opts (* (* av-dictionary))))
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
(define-alien-routine avformat-flush int (s (* av-format-context)))
(define-alien-routine av-read-play int (s (* av-format-context)))
(define-alien-routine av-read-pause int (s (* av-format-context)))
(define-alien-routine avformat-close-input void (s (* (* av-format-context))))
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
(define-alien-routine avcodec-free-context void (ctx (* (* av-codec-context))))
(define-alien-routine avcodec-close int (ctx (* av-codec-context)))
(define-alien-routine avsubtitle-free void (sub (* av-subtitle)))
(define-alien-routine avcodec-get-class (* av-class))
(define-alien-routine avcodec-alloc-context3 (* av-codec-context) (codec (* av-codec)))
(define-alien-routine avcodec-get-subtitle-rect-class (* av-class))
(define-alien-routine avcodec-open2 int (avctx (* av-codec-context)) (codec (* av-codec)) (options (* (* av-dictionary))))
(define-alien-routine avcodec-get-type av-media-type (codec-id av-codec-id))
(define-alien-routine avcodec-get-name c-string (codec-id av-codec-id))
(define-alien-routine av-get-bits-per-sample int (codec-id av-codec-id))
(define-alien-routine av-get-exact-bits-per-sample int (codec-id av-codec-id))
(define-alien-routine avcodec-profile-name c-string (codec-id av-codec-id) (profile int))
(define-alien-routine av-get-pcm-codec av-codec-id (fmt av-sample-format) (be int))
(define-alien-routine av-codec-iterate (* av-codec) (opaque (* (* t))))
(define-alien-routine avcodec-find-decoder (* av-codec) (id av-codec-id))
(define-alien-routine avcodec-find-decoder-by-name (* av-codec) (name c-string))
(define-alien-routine avcodec-find-encoder (* av-codec) (id av-codec-id))
(define-alien-routine avcodec-find-encoder-by-name (* av-codec) (name c-string))
(define-alien-routine av-codec-is-encoder int (codec (* av-codec)))
(define-alien-routine av-codec-is-decoder int (codec (* av-codec)))
(define-alien-routine av-get-profile-name c-string (codec (* av-codec)) (profile int))
(define-alien-routine avcodec-descriptor-get (* av-codec-descriptor) (id av-codec-id))
(define-alien-routine avcodec-descriptor-next (* av-codec-descriptor) (prev (* av-codec-descriptor)))
(define-alien-routine avcodec-descriptor-get-by-name (* av-codec-descriptor) (name c-string))

(define-alien-routine avcodec-send-packet int (avctx (* av-codec-context)) (avpkt (* av-packet)))
(define-alien-routine avcodec-receive-frame int (avctx (* av-codec-context)) (avpkt (* av-frame)))

(define-alien-routine avcodec-send-frame int (avctx (* av-codec-context)) (avpkt (* av-frame)))
(define-alien-routine avcodec-receive-packet int (avctx (* av-codec-context)) (avpkt (* av-packet)))

(define-alien-routine av-frame-alloc (* av-frame))
(define-alien-routine av-packet-alloc (* av-packet))
(define-alien-routine av-packet-free void (pkt (* av-packet)))
(define-alien-routine av-frame-free void (frame (* av-frame)))

(define-alien-routine av-parser-iterate (* av-codec-parser) (opaque (* (* t))))
(define-alien-routine av-parser-init (* av-codec-parser-context) (codec-id int))
(define-alien-routine av-parser-close void (parser (* av-codec-parser-context)))

(define-alien-routine av-parser-parse2 int
  (s (* av-codec-parser-context))
  (avctx (* av-codec-context))
  (poutbuf (* (* unsigned-char)))
  (poutbuf-size (* int))
  (buf (* unsigned-char))
  (buf-size int)
  (pts unsigned-long)
  (dts unsigned-long)
  (pos unsigned-long))
(define-alien-routine av-log-get-level int)
(define-alien-routine av-log-set-level void (level int))
;; va-list
(define-alien-routine av-log-set-callback void
  (callback (* (function void (* t) int c-string (* t)))))
(define-alien-routine av-get-bytes-per-sample int
  (sample-fmt av-sample-format))
(define-alien-routine av-channel-layout-default void (ch-layout (* av-channel-layout)) (nb-channels int))
