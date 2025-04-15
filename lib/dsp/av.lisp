;;; av.lisp --- Audio/Video

;; High-level wrappers to libav*

;;; Code:
(in-package :dsp/av)

(defun load-av (&key (util t) (codec t) (format t))
  (when util (load-avutil))
  (when codec (load-avcodec))
  (when format (load-avformat)))

;; (load-libav)

(defmacro with-av-format-context (sym &body body)
  `(with-alien ((,sym (* av-format-context) (avformat-alloc-context)))
     (unwind-protect (progn ,@body)
       (avformat-free-context ,sym))))

(defmacro with-av-codec-context ((sym codec-id) &body body)
  `(with-alien ((,sym (* av-codec-context) (avcodec-alloc-context3 (avcodec-find-decoder ,codec-id))))
     (unwind-protect (progn ,@body)
       (avcodec-free-context ,sym))))

(defmacro with-av-parser ((sym codec-id) &body body)
  `(with-alien ((,sym (* av-codec-context) (av-parser-init ,codec-id)))
     (unwind-protect (progn ,@body)
       (av-parser-close ,sym))))

(defmacro with-av-frame (sym &body body)
  `(with-alien ((,sym (* av-frame) (av-frame-alloc)))
     (unwind-protect (progn ,@body)
       (av-frame-free ,sym))))

(defmacro with-av-packet (sym &body body)
  `(with-alien ((,sym (* av-packet) (av-packet-alloc)))
     (unwind-protect (progn ,@body)
       (av-frame-free ,sym))))

(defun av-dictionary-alist (dict)
  (let ((tag))
    (loop do (setf tag (av-dict-iterate dict tag))
          while (and tag (not (null-alien tag)))
          collect (cons (slot tag 'ffmpeg::key) (slot tag 'ffmpeg::val)))))

(defun av-dictionary-to-hash-table (dict)
  (let ((tag)
        (tbl (make-hash-table :test 'equalp)))
    (loop do (setf tag (av-dict-iterate dict tag))
	  while (and tag (not (null-alien tag)))
	  do (setf (gethash (slot tag 'ffmpeg::key) tbl) (slot tag 'ffmpeg::val))
          finally (return tbl))))

(defun av-dictionary-coerce (dict type)
  (ecase type
    (:hash-table (av-dictionary-to-hash-table dict))
    ((or :alist :list) (av-dictionary-alist dict))))

(defun media-file-metadata (path &optional (type :hash-table))
  (with-av-format-context ctx
    (assert (zerop (avformat-open-input (addr ctx) (namestring path) nil nil)))
    (with-alien ((dict (* av-dictionary) (slot ctx 'ffmpeg::metadata)))
      (prog1 (av-dictionary-coerce dict type)
	(avformat-close-input (addr ctx))))))

(defun media-file-format (path)
  (with-av-format-context ctx
    (assert (zerop (avformat-open-input (addr ctx) (namestring path) nil nil)))
    (assert (zerop (avformat-find-stream-info ctx nil)))
    (let ((iformat (slot ctx 'ffmpeg::iformat)))
      (values
       (ssplit #\, (slot iformat 'ffmpeg::name))
       (ssplit #\, (slot iformat 'ffmpeg::extensions))
       (ssplit #\, (slot iformat 'ffmpeg::mime-type))))))

(defun media-file-codecs (path)
  (with-av-format-context ctx
    (assert (zerop (avformat-open-input (addr ctx) (namestring path) nil nil)))
    (assert (zerop (avformat-find-stream-info ctx nil)))
    (let ((vc (slot ctx 'ffmpeg::video-codec))
          (ac (slot ctx 'ffmpeg::audio-codec))
          (sc (slot ctx 'ffmpeg::subtitle-codec))
          (dc (slot ctx 'ffmpeg::data-codec)))
      (values ac vc sc dc))))

(defun media-file-stream-count (path)
  (with-av-format-context ctx
    (assert (zerop (avformat-open-input (addr ctx) (namestring path) nil nil)))
    (assert (zerop (avformat-find-stream-info ctx nil)))
    (values (slot ctx 'ffmpeg::nb-streams) (slot ctx 'ffmpeg::nb-stream-groups))))
