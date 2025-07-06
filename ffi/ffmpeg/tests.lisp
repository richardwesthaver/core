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

(defvar *test-flac* (asdf:system-relative-pathname :core ".stash/my_seat_and_weep.flac"))
;; (setq *test-flac* "/opt/stash/media/music/NX1/SR_EP/01_SR_1.flac")
;; (setq *test-flac* "/opt/stash/media/music/Village_People/Go_West/01_In_the_Navy.mp3")
;; (setq *test-flac* "/opt/store/packy/data/test/Weltschmerz.wav")
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
    (iszero (avformat-open-input (addr ctx) *test-flac* nil nil))
    (iszero (avformat-find-stream-info ctx nil))
    (with-alien ((dict (* av-dictionary) (slot ctx 'ffmpeg::metadata)))
      (let ((cnt (av-dict-count dict))
            (tag))
        (is= cnt
             (length
              (loop do (setf tag (av-dict-iterate (slot ctx 'ffmpeg::metadata) tag))
                    while (and tag (not (null-alien tag)))
                    collect (cons (slot tag 'ffmpeg::key) (slot tag 'ffmpeg::val)))))))
    (avformat-close-input (addr ctx))
    (isnt (avformat-free-context ctx))))

(deftest decode-audio ()
  "See https://github.com/FFmpeg/FFmpeg/blob/master/doc/examples/decode_audio.c"
  (let ((inbuf-size 20480)
        (refill-thresh 4096)
        (out-path (tmpize-pathname "/tmp/av-out.flac"))
        (in-path *test-flac*))
    (with-alien ((pkt (* av-packet))
                 (codec (* av-codec) (avcodec-find-decoder (av-codec-id :flac)))
                 (ch-layout av-channel-layout))
      (isnt (null-alien codec))
      (with-alien ((parser (* av-codec-parser-context) (av-parser-init (slot codec 'ffmpeg::id)))
                   (c (* av-codec-context) (avcodec-alloc-context3 codec))
                   (frame (* av-frame)))
        (av-channel-layout-default (addr (slot c 'ffmpeg::ch-layout)) 2)
        (iszero (avcodec-open2 c codec nil))
        (io/static:with-static-vector (inbuf (+ inbuf-size +av-input-buffer-padding-size+))
          (with-open-files ((f in-path :element-type 'octet)
                            (out out-path :direction :output))
            (let ((data-size (read-sequence inbuf f))
                  (data 0)
                  (ret 0))
              (loop while (> data-size 0)
                    do (progn
                         (setf frame (av-frame-alloc)
                               pkt (av-packet-alloc))
                         (is>=  
                          (setf ret
                                (av-parser-parse2 parser c (addr (slot pkt 'ffmpeg::data)) 
                                                  (addr (slot pkt 'ffmpeg::size))
                                                  (io/static:static-vector-pointer inbuf) data-size
                                                  +av-nopts-value+ +av-nopts-value+ 0))
                          0)
                         (decf data-size ret)
                         (incf data ret)
                         (when (< 0 (slot pkt 'ffmpeg::size))
                           (let ((ret 0) (size 0))
                             (setf ret (avcodec-send-packet c pkt))
                             (loop while (>= ret 0)
                                   do (progn
                                        ;; getting EOF error
                                        (setf ret (avcodec-receive-frame c frame)
                                              size (av-get-bytes-per-sample (slot c 'ffmpeg::sample-fmt)))
                                        (loop for i below (slot frame 'ffmpeg::nb-samples)
                                              for ch below (slot (slot c 'ffmpeg::ch-layout) 'ffmpeg::nb-channels)
                                              collect inbuf)))))
                         (when (< data-size refill-thresh)
                           (when (> (print #1=(read-sequence inbuf f :end (- inbuf-size data-size))) 0)
                             (incf data-size #1#))))))))
	(setf (slot pkt 'ffmpeg::data) nil)
	(setf (slot pkt 'ffmpeg::size) 0)
        (is (ffmpeg::av-sample-format* (slot c 'ffmpeg::sample-fmt)))
        (is= 2 (slot (slot c 'ffmpeg::ch-layout) 'ffmpeg::nb-channels))
        (avcodec-free-context (addr c))
        (av-parser-close parser)
        (av-packet-free pkt)
        (av-frame-free frame)))))
