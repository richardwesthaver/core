;;; media.lisp --- CLI Media Tools

;; 

;;; Code:
(in-package :cli/tools/media)

(define-cli-tool :ffmpeg (&rest args)
  (let ((proc (sb-ext:run-program *ffmpeg* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (ffmpeg-error "FFMPEG command failed: ~A ~A" *ffmpeg* (or args "")))))

#|
 D..... = Decoding supported
 .E.... = Encoding supported
 ..V... = Video codec
 ..A... = Audio codec
 ..S... = Subtitle codec
 ..D... = Data codec
 ..T... = Attachment codec
 ...I.. = Intra frame-only codec
 ....L. = Lossy compression
 .....S = Lossless compression
|#
(define-bitfield ffmpeg-codec-props
  (decode boolean)
  (encode boolean)
  (type (member :video :audio :subtitle :data :attachment))
  (audio boolean)
  (subtitle boolean)
  (data boolean)
  (attachment boolean)
  (intra boolean)
  (lossy boolean)
  (lossless boolean))

;; TODO 2025-03-12: 
(defun parse-ffmpeg-codec-props (str)
  "DEVILS"
  (make-ffmpeg-codec-props))

(defstruct ffmpeg-codec (props 0 :type ffmpeg-codec-props) name description)

(defun read-ffmpeg-codec (stream)
  (let ((props (read stream))
        (name (read stream))
        (description (read-line stream)))
    (make-ffmpeg-codec :props (parse-ffmpeg-codec-props props) :name name :description description)))

(define-cli-tool :mpv (&rest args)
  (let ((proc (sb-ext:run-program *ffmpeg* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (mpv-error "MPV command failed: ~A ~A" *mpv* (or args "")))))
