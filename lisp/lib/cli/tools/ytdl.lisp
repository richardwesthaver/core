;;; ytdl.lisp --- YouTube Downloader

;; 

;;; Code:
(in-package :cli/tools/ytdl)

(deferror ytdl-error (simple-error error) () (:auto t))

(defvar *ytdl* (or (find-exe "yt-dlp")
                   (find-exe "youtube-dl")))

(defun run-ytdl* (args &optional (output *standard-output*) input)
  (let ((proc (if input
                  (sb-ext:run-program *ytdl* (or args nil) :output :stream :input input)
                  (sb-ext:run-program *ytdl* (or args nil) :output :stream))))
  (with-open-stream (s (sb-ext:process-output proc))
    (loop for l = (read-line s nil nil)
          while l
          do (write-string l output)))
  (if (eq 0 (sb-ext:process-exit-code proc))
      nil
      (ytdl-error "YTDL command failed: ~A ~A" *ytdl* (or args "")))))

(defun run-ytdl (&rest args)
  (run-ytdl* args))
