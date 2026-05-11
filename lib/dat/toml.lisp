;;; dat/toml.lisp --- TOML

;; TOML de/serialization for Lisp.

;;; Commentary:

;; This code was originally based on https://github.com/sheepduke/clop which
;; provides a TOML parser using the ESRAP package.

;; ref: https://toml.io/en/v1.0.0

;; grammar: https://raw.githubusercontent.com/toml-lang/toml/1.0.0/toml.abnf

#|
* TOML is case-sensitive.                                    
* A TOML file must be a valid UTF-8 encoded Unicode document.
* Whitespace means tab (0x09) or space (0x20).               
* Newline means LF (0x0A) or CRLF (0x0D 0x0A).               
|#

;;; Code:
(in-package :dat/toml)

;;; Vars
(defvar *+inf* :+inf
  "The value of +inf when decoding TOML.")

(defvar *-inf* :-inf
  "The value of -inf when decoding TOML.")

(defvar *+nan* :+nan
  "The value of +nan when decoding TOML.")

(defvar *-nan* :-nan
  "The value of -nan when decoding TOML.")

(defclass toml-object (ast) ())

(defmethod print-object ((self toml-object) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (car (ast self)))))

(defclass toml-table (toml-object) ())

(defclass toml-document (toml-object) ())

;;; Read
(defun toml-peek-char (stream expected &key skip-ws)
  (when (equal (peek-char skip-ws stream nil) expected)
    (read-char stream)))

(defun toml-read-char (stream expected &key skip-ws)
  (declare (optimize (speed 3) (debug 0)))
  (if (toml-peek-char stream expected :skip-ws skip-ws)
      t
      (error "TOML error: unexpected ~s~%expected ~A" (read-char stream) expected)))

(defun toml-read (stream &optional (eof-error-p t) eof-value)
  (let ((c (peek-char t stream eof-error-p :eof)))
    (case c
      (:eof eof-value)
      (#\[ (toml-read-table stream)) ;; arrays are values only
      (#\# (toml-read-comment stream) (toml-read stream eof-error-p eof-value))
      (t (toml-read-pair stream)))))

(defun toml-read-table (stream)
  (toml-read-char stream #\[ :skip-ws t) ; [
  (let ((ret (toml-read-header stream)))
    (loop while (or (toml-read-comment stream) (toml-peek-bare-char stream))
          do (push (toml-read-pair stream) ret))
    (make-instance 'toml-table :ast (nreverse ret))))

(defun toml-read-header (stream)
  (let ((c (peek-char t stream nil nil))
        (ret))
    (case c
      ;; array-table
      (#\[ (read-char stream) 
       (push (list (toml-read-key stream)) ret)
       (toml-read-char stream #\]))
      (t (push (toml-read-key stream) ret)))
    (toml-read-char stream #\])
    ret))

(defun toml-bare-char-p (c)
  (or (digit-char-p c) (sb-unicode:alphabetic-p c) (char= #\- c) (char= #\_ c)))

(defun toml-peek-bare-char (stream)
  (let ((c (peek-char t stream nil nil)))
    (and c (toml-bare-char-p c))))

(defun toml-read-bare-key (stream)
  (with-output-to-string (s)
    (loop for c = (peek-char t stream nil nil)
          while (and c (toml-bare-char-p c))
          do (write-char (read-char stream) s))
    s))

(defun toml-read-string (stream)
  "TOML supports basic, multi-line basic, literal, and multi-line literal
strings. All strings are UTF-8."
  (let ((y (peek-char t stream)))
    ;; attempt to read unquoted strings
    (if (alphabetic-p y) 
        (read-line stream)
        (let ((q (read-char stream)))
          (with-output-to-string (s)
            (if (eql q (peek-char t stream nil nil)) ;; 2 quotes
                (if (eql q (peek-char t stream nil nil)) ;; 3 quotes (multi-line)
                    (progn 
                      ;; first we consume the first 2 multi-line quote chars
                      (read-sequence (make-string 2) stream)
                      (loop for c = (read-char stream nil nil)
                            while c
                            if (eql q c)
                            do (let ((c1 (read-char stream nil nil))
                                     (c2 (read-char stream nil nil)))
                                 (if (char= q c1 c2) ;; 3 quotes
                                     (return s)
                                     (progn
                                       (write-char c s)
                                       (write-char c1 s)
                                       (write-char c2 s))))
                            else do (write-char c s)))
                    ;; empty string (2 quotes)
                    (progn
                      (read-char stream nil nil) ;; q
                      s))
                (loop for c = (read-char stream nil nil)
                      while c
                      if (eql q c)
                      return s
                      else do (write-char c s))))))))

(defun toml-read-simple-string (stream)
  "Read a single-quoted string."
  (let ((q (read-char stream)))
    (with-output-to-string (s)
      (loop for c = (read-char stream nil nil)
            until (or (eql c q) (not c))
            do (write-char c s))
      s)))

(defun toml-read-key (stream)
  "TOML supports bare, quoted, and dotted keys."
  (let ((c (peek-char t stream nil nil)))
    (case c
      ;; quoted
      ((or #\" #\')
       (let ((key (toml-read-simple-string stream)))
         (if (toml-peek-char stream #\.)
             (cons key (toml-read-key stream))
             key)))
      ;; bare/dotted
      (t (let ((key (toml-read-bare-key stream)))
           (unless (sequence:emptyp key)
             (if (toml-peek-char stream #\.)
                 (cons key (toml-read-key stream))
                 key)))))))

(defun toml-read-pair (stream)
  (when-let ((key (toml-read-key stream)))
    (toml-read-char stream #\= :skip-ws t)
    (cons key (toml-read-value stream))))

(defun toml-read-value (stream)
  (let ((c (peek-char t stream nil nil)))
    (case c
      (#\{ (toml-read-inline-table stream))
      ((or #\" #\') (toml-read-string stream))
      (#\[ (toml-read-array stream))
      (#\t (toml-read-true stream))
      (#\f (toml-read-false stream))
      (t (toml-read-number-or-datetime stream)))))

(defun toml-read-inline-table (stream)
  (toml-read-char stream #\{ :skip-ws t) ; {
  (let ((ret))
    (loop 
      (toml-peek-char stream #\, :skip-ws t)
      (if (toml-peek-char stream #\} :skip-ws t)
          (return ret)
          (if-let ((pair (toml-read-pair stream)))
            (push pair ret)
            (return (nreverse ret)))))))

(defun toml-read-array (stream)
  (toml-read-char stream #\[ :skip-ws t)
  (let ((ret))
    (loop
      (if (progn (toml-peek-char stream #\, :skip-ws t)
                 (toml-peek-char stream #\] :skip-ws t))
          (return (coerce ret 'vector))
          (push (toml-read-value stream) ret)))))

(defun toml-read-true (stream)
  (let ((s (make-string 4)))
    (read-sequence s stream)
    (if (equal s "true")
        t
        (error "TOML error: expected 'true', got ~A" s))))

(defun toml-read-false (stream &optional (default :error))
  (let ((s (make-string 5)))
    (read-sequence s stream)
    (if (equal s "false")
        t
        (if (eql default :error)
            (error "TOML error: expected 'false', got ~A" s)
            (toml-read-string stream)))))

(defun toml-read-number-or-datetime (stream)
  (let ((c (peek-char t stream nil nil)))
    (case c
      (#\+ (read-char stream) (toml-read-positive stream))
      (#\- (read-char stream) (toml-read-negative stream))
      (#\n (toml-read-nan stream))
      (#\i (toml-read-inf stream))
      (t 
       ;; REVIEW 2026-05-10: 
       (if (alphabetic-p c) ;; may not be spec compliant
           (return-from toml-read-number-or-datetime (toml-read-string stream))
       (let ((n (read stream)))
         (if (stringp n)
             ;; junk allowed for parsing time values
             (if-let ((%n (ignore-errors (parse-number n))))
               %n
               ;; if we can't parse as a number try it as a datetime
               (toml-parse-datetime stream))
             n)))))))

(defun toml-read-positive (stream)
  (let ((c (peek-char t stream nil nil)))
    (case c
      (#\i (toml-read-inf stream) *+inf*)
      (#\n (toml-read-nan stream) *+nan*)
      (t (abs (parse-number (read stream)))))))

(defun toml-read-negative (stream)
  (let ((c (peek-char t stream nil nil)))
    (case c
      (#\i (toml-read-inf stream) *-inf*)
      (#\n (toml-read-nan stream) *-nan*)
      (t (- (parse-number (read stream)))))))

(defun toml-read-nan (stream)
  (let ((s (make-string 3)))
    (read-sequence s stream)
    (if (equal s "nan")
        :nan
        (error "TOML error: expected 'nan', got ~A" s))))

(defun toml-read-inf (stream)
  (let ((s (make-string 3)))
    (read-sequence s stream)
    (if (equal s "inf")
        :inf
        (error "TOML error: expected 'inf', got ~A" s))))

(defun toml-read-comment (stream)
  (loop while (toml-peek-char stream #\# :skip-ws t)
           do (read-line stream)))

;; TODO 2024-12-23: may include spaces, can't do a simple read :C
(defun toml-parse-datetime (str)
  (or (ignore-errors (parse-timestring (ppcre:regex-replace " " "T" str)))
      (toml-parse-datetime-local str)))

(defmethod toml-parse-datetime-local (str)
  (let* ((delimeter (sequence:elt str 10))
         (splits (split-sequence delimeter str)))
    (append (with-input-from-string (s (car splits)) (toml-parse-date-local s))
            (with-input-from-string (s (cadr splits)) (toml-parse-time-local (cadr splits))))))

(defun toml-parse-date-local (value)
  "Return a plist with :year :month :date."
  (let* ((*default-timezone* +utc-zone+)
         (timestamp (parse-timestring value)))
    (list :year (timestamp-year timestamp)
          :month (timestamp-month timestamp)
          :day (timestamp-day timestamp))))

(defun toml-parse-time-local (value)
  "Return a plist with :hour :minute :second."
  (let* ((*default-timezone* +utc-zone+)
         (timestamp (parse-timestring value)))
    (list :hour (timestamp-hour timestamp)
          :minute (timestamp-minute timestamp)
          :second (timestamp-second timestamp)
          :microsecond (timestamp-microsecond timestamp))))

;; TODO 2024-12-24: traverse?
(defmethod unwrap ((self toml-document))
  (mapcar 'ast (ast self)))
  
;;; Serde

;; TODO 2023-12-23: 
;; (serialize '("test") :toml)

(defmethod serialize ((table toml-table) (format (eql :toml)) &key (style :alist))
  (loop for k in (ast table)
        collect (cons k (serialize (cdr k) format :style style))))

(defun toml-read-document (stream)
  (make-instance 'toml-document
    :ast
    (loop for x = (toml-read stream nil nil)
          while x
          collect x)))

(defmethod deserialize ((from pathname) (format (eql :toml)) &key)
  (with-open-file (f from)
    (toml-read-document f)))

(defmethod deserialize ((from string) (format (eql :toml)) &key)
  (with-input-from-string (s from)
    (toml-read-document s)))

(defmethod deserialize ((from stream) (format (eql :toml)) &key)
  (with-open-stream (s from)
    (toml-read-document s)))
