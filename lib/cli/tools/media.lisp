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
  (intra boolean)
  (lossy boolean)
  (lossless boolean))

(defun parse-ffmpeg-codec-type (char)
  (ecase char
    (#\V :video)
    (#\A :audio)
    (#\S :subtitle)
    (#\D :data)
    (#\T :attachment)))

(defun parse-ffmpeg-codec-props (str)
  "DEVILS"
  (make-ffmpeg-codec-props
   :decode (char= #\D (schar str 0))
   :encode (char= #\E (schar str 1))
   :type (parse-ffmpeg-codec-type (schar str 2))
   :intra (char= #\I (schar str 3))
   :lossy (char= #\L (schar str 4))
   :lossless (char= #\S (schar str 5))))

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

(define-cli-tool :picard (args &key (wait t) (output t))
  (let ((proc (sb-ext:run-program *picard* args :wait wait :output output)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (picard-error "PICARD command failed: ~A ~A" *picard* (or args "")))))

(defvar *picard-config-path* (merge-homedir-pathnames ".config/MusicBrainz/Picard.ini"))

(config:defconfig picard-config (cli-tool-config ini:ini-document) ())

(defmethod config:make-config ((self (eql :picard)) &rest args &key path &allow-other-keys)
  (if (remf args :path)
      (apply 'change-class (deserialize path :ini) 'picard-config args)
      (apply 'make-instance 'picard-config args)))

(defun load-picard-config (&optional (path *picard-config-path*))
  (config:make-config :picard :path path))

(defvar *picard-commands*
  '(:clear-logs
    :cluster
    :fingerprint
    :from-file
    :load
    :lookup
    :lookup-cd
    :pause
    :quit
    :remove
    :remove-all
    :remove-empty
    :remove-saved
    :save-matched
    :scan
    :show
    :submit-fingerprints
    :write-logs))

(eval-always
  (defun picard-cmd (cmd)
    (when (member cmd *picard-commands*)
      (substitute #\_ #\- (string-upcase cmd)))))

(defun exec-picard (&rest commands)
  "Execute a PICARD batch program consisting of COMMANDS."
  (apply 'run-picard (cons "-e" (apply 'picard-cmd commands))))
