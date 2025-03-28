;;; media.lisp --- CLI Media Tools

;; 

;;; Code:
(in-package :cli/tools/media)

(define-cli-tool :ffmpeg (args &optional (output *standard-output*))
  (let ((proc (sb-ext:run-program *ffmpeg* args :wait t :output output)))
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
  (when-let ((props (read stream nil nil))
             (name (read stream nil nil))
             (description (trim (read-line stream nil nil))))
    (make-ffmpeg-codec :props (parse-ffmpeg-codec-props props) :name name :description description)))

(defun list-ffmpeg-codecs ()
  (let ((ret (with-output-to-string (s)
               (run-ffmpeg (list "-v" "0" "-codecs") s))))
    (when-let ((i (search " -------" ret)))
      (with-input-from-string (s (subseq ret (+ i 9)))
        (loop for f = (print (read-ffmpeg-codec s))
              while f
              collect f)))))

;; TODO 2025-03-23: 
;; (defun list-ffmpeg-formats ()
;;   (with-output-to-string (s)
;;     (run-ffmpeg (list "-v" "0" "-formats") s)))

(define-cli-tool :mpv (&rest args)
  (let ((proc (sb-ext:run-program *ffmpeg* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (mpv-error "MPV command failed: ~A ~A" *mpv* (or args "")))))

(define-cli-tool :wireplumber (&rest args)
  (let ((proc (sb-ext:run-program *wireplumber* args :wait t :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (wireplumber-error "WIREPLUMBER command failed: ~A ~A" *wireplumber* (or args "")))))
