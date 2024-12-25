;;; ini.lisp --- INI Format

;; https://en.wikipedia.org/wiki/INI_file

;;; Code:
(in-package :dat/ini)

(defclass ini-object (ast) ())
(defclass ini-document (ini-object) ())
(defclass ini-section (ini-object) ())
;; (defun ini-write (value &optional stream))

(defmethod print-object ((self ini-object) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (car (ast self)))))

(defun ini-peek-char (stream expected &key skip-ws)
  (when (equal (peek-char skip-ws stream nil) expected)
    (read-char stream)))

(defun ini-read-char (stream expected &key skip-ws)
  (declare (optimize (speed 3) (debug 0)))
  (if (ini-peek-char stream expected :skip-ws skip-ws)
      t
    (error "INI error: unexpected ~s~%expected ~A" (read-char stream) expected)))

(defun ini-read (stream &optional (eof-error-p t) eof-value)
  (let ((c (peek-char t stream eof-error-p :eof)))
    (case c
      (:eof eof-value)
      (#\[ (ini-read-section stream))
      (#\# (ini-read-comment stream))
      (t (ini-read-pair stream)))))

(defun ini-key-char-p (c)
  (or (digit-char-p c) (sb-unicode:alphabetic-p c) (char= #\- c) (char= #\_ c)))

(defun ini-peek-key-char (stream)
  (let ((c (peek-char t stream nil nil)))
    (when (and c (ini-key-char-p c))
      c)))

(defun ini-read-key (stream)
  (with-output-to-string (s)
    (loop for c = (peek-char t stream nil nil)
          while (and c (ini-bare-char-p c))
          do (write-char (read-char stream) s))
    s))

(defun ini-read-pair (stream)
  (let ((line (split-sequence #\= (read-line stream) :count 2)))
    (unless (sequence:emptyp (car line))
      (let ((l (mapcar 'trim line)))
        (cons (car l) 
              (with-input-from-string (s (cadr l))
                ;; read value as lisp - we may need to walk back on this and
                ;; just accept strings - for now it's fine :)
                (read s)))))))

(defun ini-read-section (stream)
  (ini-read-char stream #\[ :skip-ws t)
  (let ((ret (list (ini-read-key stream))))
    (ini-read-char stream #\] :skip-ws t)
    (loop while (ini-peek-key-char stream)
          do (push (ini-read-pair stream) ret))
    (make-instance 'ini-section
      :ast (nreverse ret))))

(defun ini-read-comment (stream)
  (ini-read-char stream #\# :skip-ws t)
  (log:debug! :toml-comment (trim (read-line stream)))
  (values))

(defun ini-read-document (stream)
  (make-instance 'ini-document
    :ast
    (loop for x = (ini-read stream nil nil)
          while x
          collect x)))

;;; Serde
(defmethod deserialize ((from stream) (format (eql :ini)) &key)
  (ini-read-document from))

(defmethod deserialize ((from string) (format (eql :ini)) &key)
  (with-input-from-string (s from)
    (ini-read-document s)))

(defmethod deserialize ((from pathname) (format (eql :ini)) &key)
  (with-open-file (f from)
    (ini-read-document f)))
