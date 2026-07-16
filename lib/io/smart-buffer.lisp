;;; io/smart-buffer.lisp --- Smart Octet Buffers

;; This is ported from Fukamachi's SMART-BUFFER

;;; Commentary:

;; Smart-buffers dynamically allocate based on the inferred buffer-size either
;; in-memory to a vector or on disk to a temporary file to save memory
;; consumption.

;;; Code:
(in-package :io/smart-buffer)

(defvar *default-memory-limit* (expt 2 20))
(defvar *default-disk-limit* (expt 2 30))

(defvar *smart-buffer-tmp* (ensure-directories-exist (tmp-path "smart-buffer")))

(defstruct (smart-buffer (:conc-name :buffer-)
                         (:constructor %make-smart-buffer))
  (memory-limit *default-memory-limit*)
  (disk-limit *default-disk-limit*)
  (current-len 0)
  (on-memory-p t)
  (memory-buffer (make-concatenated-xsubseqs))
  (disk-buffer nil))

(defun make-smart-buffer (&rest initargs &key memory-limit disk-limit &allow-other-keys)
  (let ((buffer (apply #'%make-smart-buffer initargs)))
    (when (and memory-limit
               disk-limit
               (< disk-limit memory-limit))
      (setf (buffer-memory-limit buffer) disk-limit))
    buffer))

(define-condition buffer-limit-exceeded (error)
  ((limit :initarg :limit
          :initform nil))
  (:report (lambda (condition stream)
             (format stream "Buffer exceeded the limit~:[~;~:*: ~A~]"
                     (slot-value condition 'limit)))))

(defun write-to-buffer (buffer seq &optional (start 0) (end (length seq)))
  (check-type seq (array (unsigned-byte 8) (*)))
  (incf (buffer-current-len buffer) (- end start))
  (check-limit buffer)
  (if (buffer-on-memory-p buffer)
      (xnconcf (buffer-memory-buffer buffer) (xsubseq seq start end))
      (with-open-file (out (buffer-disk-buffer buffer)
                           :direction :output
                           :element-type '(unsigned-byte 8)
                           :if-exists :append)
        (write-sequence seq out :start start :end end))))

(defun check-limit (buffer)
  (cond
    ((and (buffer-on-memory-p buffer)
          (< (buffer-memory-limit buffer)
             (buffer-current-len buffer)))
     (when (< (buffer-disk-limit buffer)
              (buffer-current-len buffer))
       (error 'buffer-limit-exceeded :limit (buffer-disk-limit buffer)))
     (setf (buffer-disk-buffer buffer)
           (uiop:with-temporary-file (:stream stream :pathname tmp
                                      :directory *smart-buffer-tmp*
                                      :direction :output
                                      :element-type '(unsigned-byte 8)
                                      :keep t)
             (typecase (buffer-memory-buffer buffer)
               (null-concatenated-xsubseqs)
               (t (write-sequence (coerce-to-sequence (buffer-memory-buffer buffer)) stream)))
             tmp)
           (buffer-on-memory-p buffer) nil
           (buffer-memory-buffer buffer) nil))
    ((and (not (buffer-on-memory-p buffer))
          (< (buffer-disk-limit buffer)
             (buffer-current-len buffer)))
     (error 'buffer-limit-exceeded :limit (buffer-disk-limit buffer)))))

;; REVIEW 2025-06-11: used to be flexi stream
(defun finalize-buffer (buffer)
  (if (buffer-on-memory-p buffer)
      (let ((s (make-instance 'sb-gray:fundamental-binary-input-stream)))
        (write-sequence
         (typecase (buffer-memory-buffer buffer)
           (null-concatenated-xsubseqs #())
           (t (coerce-to-sequence (buffer-memory-buffer buffer))))
         s)
        s)
      (open (buffer-disk-buffer buffer) :direction :input :element-type '(unsigned-byte 8))))

(defmacro with-smart-buffer ((buffer &key
                                       (memory-limit '*default-memory-limit*)
                                       (disk-limit '*default-disk-limit*))
                             &body body)
  `(let ((,buffer (make-smart-buffer :memory-limit ,memory-limit :disk-limit ,disk-limit)))
     ,@body
     (finalize-buffer ,buffer)))

(defun delete-stream-file (stream)
  (when (typep stream 'file-stream)
    (ignore-errors (delete-file (pathname stream))))
  (values))

(defun delete-temporary-files (&key (stale-seconds 0))
  (let ((now (get-universal-time)))
    (mapc #'probe-delete-file
          (remove-if-not (lambda (file)
                           (< stale-seconds (- now (file-write-date file))))
                         (uiop:directory-files *smart-buffer-tmp*)))))
