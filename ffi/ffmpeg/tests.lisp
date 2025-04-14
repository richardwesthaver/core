;;; tests.lisp --- FFmpeg FFI Tests

;; 

;;; Code:
(defpackage :ffmpeg/tests
  (:use :cl :std :sb-alien :rt :ffmpeg))
(in-package :ffmpeg/tests)

(defsuite :ffmpeg)
(in-suite :ffmpeg)

(load-avutil)
(load-avcodec)
(load-avformat)
(load-avfilter)

(deftest sanity ()
  (mapcar (lambda (i) (istype 'integer i)) 
          (list
           (avcodec-version)
           (avformat-version)
           (avutil-version)
           (avfilter-version))))

(deftest metadata ()
  "See https://github.com/FFmpeg/FFmpeg/blob/master/doc/examples/show_metadata.c"
  (with-alien ((ctx (* av-format-context) (avformat-alloc-context)))
    (iszero (avformat-open-input (addr ctx) "/opt/stash/media/music/J/my_seat_and_weep/01_J_-_but_not_in_this_room.flac" nil nil))
    (iszero (avformat-find-stream-info ctx nil))
    (with-alien ((dict (* av-dictionary) (slot ctx 'ffmpeg::metadata)))
      (let ((cnt (av-dict-count dict))
            (tag))
        (is= cnt
             (length
              (print (loop do (setf tag (av-dict-iterate (slot ctx 'ffmpeg::metadata) tag))
                           while (and tag (not (null-alien tag)))
                           collect (cons (slot tag 'ffmpeg::key) (slot tag 'ffmpeg::val))))))))
    (avformat-close-input (addr ctx))
    (isnt (avformat-free-context ctx))))

(deftest decode-audio ()
  "See https://github.com/FFmpeg/FFmpeg/blob/master/doc/examples/decode_audio.c"
  (with-alien ((pkt (* av-packet) (av-packet-alloc))
               (codec (* av-codec) (avcodec-find-decoder (av-codec-id :mp2))))
    (isnt (null-alien codec))
    (with-alien ((parser (* av-codec-parser-context) (av-parser-init (slot codec 'ffmpeg::id)))
                 (c (* av-codec-context) (avcodec-alloc-context3 codec)))
      (iszero (avcodec-open2 c codec nil))
      (avcodec-free-context (addr c))
      (av-parser-close parser)
      (av-packet-free pkt))))
