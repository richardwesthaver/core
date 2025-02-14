;;; containerfile.lisp --- Containerfiles

;; Containerfile read/write methods

;;; Commentary:

;; man: https://github.com/containers/common/blob/main/docs/Containerfile.5.md

;;; Code:
(in-package :pod)

;;; Vars
(defparameter *default-containerfile* "Containerfile")

(defvar *containerfile-instructions*
  '(from arg maintainer run cmd label expose env add copy entrypoint volume user workdir onbuild))

(deftype containerfile-instruction () `(member ,*containerfile-instructions*))

(defvar *containerfile-predefined-args* 
  ;; lower-case version of these are also technically supported
  (list "HTTP_PROXY"
        "HTTPS_PROXY"
        "FTP_PROXY"
        "NO_PROXY"
        "ALL_PROXY"))

;;; Utils
(defun write-containerfile-line (cons stream)
  (write (car cons) :stream stream)
  (write-char #\space stream)
  (write-line (cdr cons) stream))

(defun read-containerfile-line (str)
  (let ((ws (position-if 'sb-unicode:whitespace-p str)))
    (cons (symbolicate (string-upcase (subseq str 0 ws)))
          (subseq str
                  (1+ ws)
                  (length str)))))

(defun containerfile-comment-p (str)
  (char= #\# (aref str 0)))

(defun containerfile-from-p (str)
  (starts-with-subseq "FROM" str))

(defun read-containerfile-from (str)
  (subseq str (1+ (position-if 'sb-unicode:whitespace-p str))))

(defun containerfile-arg-p (str)
  (starts-with-subseq "ARG" str))

(defun format-containerfile-arg (arg)
  (with-output-to-string (s)
    (etypecase arg
      (atom (write arg :stream s))
      (cons (format s "~A=~A" (car arg) (cdr arg))))))
      
(defun write-containerfile-arg (arg stream)
  (format stream "ARG ~A~%" (format-containerfile-arg arg)))

(defun write-containerfile-from (base stream)
  (format stream "FROM ~A~%" base))

;; first instruction must be FROM or ARG
(defun read-containerfile-start (stream)
  (let ((args))
    (loop for line = (trim (read-line stream nil nil))
          while line
          if (not (containerfile-from-p line))
          do (push line args)
          else if (containerfile-from-p line)
          do (return (values (read-containerfile-from line) (nreverse args))))))

;;; Obj
(defclass containerfile ()
  ((path :initform (pathname *default-containerfile*) :type pathname :initarg :path :accessor path)
   (base :type string :initarg :base :accessor containerfile-base)
   (args :initform nil :type list :initarg :args :accessor containerfile-args)
   (steps :initform (make-array 0 :element-type 'cons :adjustable t) :type (vector cons) :initarg :steps :accessor containerfile-steps)))

(defmethod serde ((from containerfile) (to pathname))
  (with-open-file (file to :direction :output)
    (when-let ((base (containerfile-base from)))
      (write-containerfile-from base file))
    (loop for arg in (containerfile-args from)
          do (write-containerfile-arg arg file))
    (loop for step across (containerfile-steps from)
          do (write-containerfile-line step file))))

(defmethod serde ((from stream) (to containerfile))
  (multiple-value-bind (base args) (read-containerfile-start from)
    (setf (containerfile-base to) base)
    (setf (containerfile-args to) args))
  (setf (containerfile-steps to)
        (coerce
         (loop for line = (trim (read-line from nil nil))
               while line
               unless (containerfile-comment-p line)
               collect (read-containerfile-line line))
         'simple-vector))
    to)

(defmethod serde ((from pathname) (to containerfile))
  (with-open-file (file from)
    (setf (path to) from)
    (serde file to)))

(defmethod serde ((from string) (to containerfile))
  (with-input-from-string (stream from)
    (serde stream to)))

(defmethod deserialize ((from pathname) (format (eql :containerfile)) &key)
  (serde from (make-instance 'containerfile)))

(defmethod serialize ((obj containerfile) (format (eql :string)) &key)
  (with-output-to-string (str)
    (loop for arg in (containerfile-args obj)
          while arg
          do (write-line arg str))
    (princ "FROM " str)
    (println (containerfile-base obj) str)
    (loop for step across (containerfile-steps obj)
          do (write-containerfile-line step str))
    str))

(defmethod serialize ((obj containerfile) (format (eql :bytes)) &key)
  (sb-ext:string-to-octets (serialize obj :string)))
