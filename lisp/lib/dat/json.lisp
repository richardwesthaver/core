;;; lib/dat/json.lisp --- JSON format

;; JSON parser generator

;; There are quite a few json libraries in the CL ecosystem. This
;; particular implementation is based on the JSON package here:
;; https://github.com/massung/json

;; It's object-based (like CL-JSON) instead of using a
;; parser-generator.

;;; Code:
(in-package :dat/json)

(defclass json-object ()
  ((members :initform nil
            :initarg :members
            :accessor json-object-members))
  (:documentation "An associative list of key/value pairs."))

;;; ----------------------------------------------------

(defmethod print-object ((obj json-object) stream)
  "Output a JSON object to a stream in readable form."
  (print-unreadable-object (obj stream :type t)
    (let ((*print-level* 1))
      (json-encode obj stream))))

;;; ----------------------------------------------------

(defun json-getf (object key &optional value)
  "Find an member's value in a JSON object."
  (let ((place (assoc key (json-object-members object) :test 'string=)))
    (if (null place)
        value
      (values (second place) t))))

;;; ----------------------------------------------------

(defun json-setf (object key value)
  "Assign a value to a key in a JSON object."
  (let ((place (assoc key (json-object-members object) :test 'string=)))
    (prog1 value
      (if (null place)
          (let ((k (if (stringp key)
                       key
                     (princ-to-string key))))
            (push (list k value) (json-object-members object)))
        (rplacd place (list value))))))

;;; ----------------------------------------------------

(defsetf json-getf json-setf)

;;; ----------------------------------------------------

(defun json-decode (string &key (start 0) end)
  "Convert a JSON string into a Lisp object."
  (with-input-from-string (stream string :start start :end end)
    (values (json-read stream)
            (file-position stream))))

(defmethod deserialize ((obj string) (format (eql :json)) &key (start 0) end)
  (declare (ignore format))
  (json-decode obj :start start :end end))

;;; ----------------------------------------------------

(defun json-encode (value &optional stream)
  "Encodes a Lisp value into a stream."
  (json-write value stream))

(defmethod serialize (obj (format (eql :json)) &key stream)
  (declare (ignore format))
  (json-encode obj stream))

;;; ----------------------------------------------------

(defun json-enable-reader-macro ()
  "Set the #{ dispatch macro character for reading JSON objects."
  (flet ((json-object-reader (stream char n)
           (declare (ignorable char n))
           (let ((xs (read-delimited-list #\} stream t)))
             (loop
                for key = (pop xs)
                for value = (pop xs)

                ;; stop when nothing is left
                unless (or xs key value)
                return (make-instance 'json-object :members pairs)

                ;; build associative list of key/value pairs
                collect (list (princ-to-string key) value)
                into pairs))))
    (set-dispatch-macro-character #\# #\{ #'json-object-reader)
    (set-macro-character #\} (get-macro-character #\) nil))))

(defun json-read (stream &optional (eof-error-p t) eof-value)
  "Read a JSON object from a stream."
  (let ((c (peek-char t stream eof-error-p :eof)))
    (case c
      (:eof eof-value)

      ;; constants, objects, lists, and strings
      (#\t (json-read-true stream))
      (#\f (json-read-false stream))
      (#\n (json-read-null stream))
      (#\{ (json-read-object stream))
      (#\[ (json-read-list stream))
      (#\" (json-read-string stream))

      ;; must be a number
      (otherwise (json-read-number stream)))))

;;; ----------------------------------------------------

(defun json-peek-char (stream expected &key skip-ws)
  "Peek at the next character or token and optionally error if unexpected."
  (declare (optimize (speed 3) (debug 0)))
  (when (equal (peek-char skip-ws stream) expected)
    (read-char stream)))

;;; ----------------------------------------------------

(defun json-read-char (stream expected &key skip-ws)
  "Read the next, expected character in the stream."
  (declare (optimize (speed 3) (debug 0)))
  (if (json-peek-char stream expected :skip-ws skip-ws)
      t
    (error "JSON error: unexpected ~s" (read-char stream))))

;;; ----------------------------------------------------

(defun json-read-true (stream)
  "Read true from a JSON stream."
  (json-read-char stream #\t :skip-ws t)
  (json-read-char stream #\r)
  (json-read-char stream #\u)
  (json-read-char stream #\e))

;;; ----------------------------------------------------

(defun json-read-false (stream)
  "Read false from a JSON stream."
  (prog1 nil
    (json-read-char stream #\f :skip-ws t)
    (json-read-char stream #\a)
    (json-read-char stream #\l)
    (json-read-char stream #\s)
    (json-read-char stream #\e)))

;;; ----------------------------------------------------

(defun json-read-null (stream)
  "Read null from a JSON stream."
  (prog1 nil
    (json-read-char stream #\n :skip-ws t)
    (json-read-char stream #\u)
    (json-read-char stream #\l)
    (json-read-char stream #\l)))

;;; ----------------------------------------------------

(defun json-read-number (stream)
  "Read a number from a JSON stream."
  (declare (optimize (speed 3) (debug 0)))
  (let ((s (with-output-to-string (s)
             (when (equal (peek-char t stream) #\-)
               (write-char (read-char stream) s))

             ;; read base-10 digits, fraction, and exponent
             (labels ((read-digits ()
                        (let ((c (read-char stream)))
                          (unless (digit-char-p c)
                            (error "JSON error: unexpected ~s" c))

                          ;; write the digits
                          (loop
                             (write-char c s)

                             ;; next digit, test for eof
                             (unless (setf c (read-char stream nil))
                               (return))

                             ;; ensure digit
                             (unless (digit-char-p c)
                               (return (unread-char c stream))))))

                      ;; fractional component
                      (read-fraction ()
                        (when (equal (peek-char nil stream nil) #\.)
                          (write-char (read-char stream) s)
                          (read-digits)))

                      ;; exponent
                      (read-exponent ()
                        (when (equalp (peek-char nil stream nil) #\e)
                          (write-char (read-char stream) s)

                          ;; optional sign
                          (case (peek-char nil stream)
                            (#\- (write-char (read-char stream) s))
                            (#\+ (write-char (read-char stream) s)))

                          ;; exponent
                          (read-digits))))

               ;; read each component; numbers beginning with 0 are a special case
               (if (equalp (peek-char nil stream) #\0)
                   (write-char (read-char stream) s)
                 (read-digits))
               (read-fraction)
               (read-exponent)))))
    (prog1
      (read-from-string s))))

;;; ----------------------------------------------------

(defun json-read-string (stream)
  "Read a string from a JSON stream."
  (declare (optimize (speed 3) (debug 0)))

  ;; read the expected quote
  (json-read-char stream #\" :skip-ws t)

  ;; read into an output buffer
  (with-output-to-string (s)
    (loop
       for c = (read-char stream)

       ;; stop at closing quote
       until (char= c #\")

       ;; write character to output
       do (if (char/= c #\\)
              (write-char c s)
            (let ((c (case (read-char stream)
                       (#\n #\newline)
                       (#\t #\tab)
                       (#\f #\formfeed)
                       (#\b #\backspace)
                       (#\r #\return)

                       ;; read unicode character
                       (#\u (let ((x1 (digit-char-p (read-char stream) 16))
                                  (x2 (digit-char-p (read-char stream) 16))
                                  (x3 (digit-char-p (read-char stream) 16))
                                  (x4 (digit-char-p (read-char stream) 16)))
                              (code-char (logior (ash x1 12)
                                                 (ash x2  8)
                                                 (ash x3  4)
                                                 (ash x4  0)))))

                       ;; verbatim character
                       (otherwise c))))
              (write-char c s))))))

;;; ----------------------------------------------------

(defun json-read-list (stream)
  "Read a list of JSON values."
  (declare (optimize (speed 3) (debug 0)))

  ;; read the expected open bracket
  (json-read-char stream #\[ :skip-ws t)

  ;; check for an empty list
  (if (json-peek-char stream #\] :skip-ws t)
      nil
    (loop
       for x = (json-read stream)
       collect x
       into xs

       ;; check for another element
       while (json-peek-char stream #\, :skip-ws t)

       ;; return the final list
       finally (return (prog1 xs
                         (json-read-char stream #\] :skip-ws t))))))

;;; ----------------------------------------------------

(defun json-read-object (stream)
  "Read an associative list of key/value pairs into a JSON object."
  (declare (optimize (speed 3) (debug 0)))

  ;; read the expected open brace
  (json-read-char stream #\{ :skip-ws t)

  ;; check for an empty object
  (if (json-peek-char stream #\} :skip-ws t)
      (make-instance 'json-object)
    (loop
       for key = (json-read-string stream)
       for value = (progn
                     (json-read-char stream #\: :skip-ws t)
                     (json-read stream))

       ;; build the associative list of members
       collect (list key value)
       into xs

       ;; check for another element
       while (json-peek-char stream #\, :skip-ws t)

       ;; return the final list
       finally (return (prog1 (make-instance 'json-object :members xs)
                         (json-read-char stream #\} :skip-ws t))))))

(defmethod json-write ((value (eql t)) &optional stream)
  "Encode the true value."
  (declare (ignore value))
  (format stream "~<true~>"))

;;; ----------------------------------------------------

(defmethod json-write ((value (eql nil)) &optional stream)
  "Encode the null constant."
  (declare (ignore value))
  (format stream "~<null~>"))

;;; ----------------------------------------------------

(defmethod json-write ((value symbol) &optional stream)
  "Encode a symbol to a stream."
  (json-write (symbol-name value) stream))

;;; ----------------------------------------------------

(defmethod json-write ((value number) &optional stream)
  "Encode a number to a stream."
  (format stream "~<~a~>" value))

;;; ----------------------------------------------------

(defmethod json-write ((value ratio) &optional stream)
  "Encode a ratio to a stream."
  (format stream "~<~a~>" (float value)))

;;; ----------------------------------------------------

(defmethod json-write ((value string) &optional stream)
  "Encode a string as a stream."
  (flet ((encode-char (c)
           (cond
            ((char= c #\\) "\\\\")
            ((char= c #\") "\\\"")
            ((char= c #\newline) "\\n")
            ((char= c #\tab) "\\t")
            ((char= c #\formfeed) "\\f")
            ((char= c #\backspace) "\\b")
            ((char= c #\return) "\\r")
            ((char> c #\~)
             (format nil "\\u~16,4,'0r" (char-code c)))
            (t
             (string c)))))
    (format stream "~<\"~{~a~}\"~>" (map 'list #'encode-char value))))

;;; ----------------------------------------------------

(defmethod json-write ((value pathname) &optional stream)
  "Encode a pathname as a stream."
  (json-write (namestring value) stream))

;;; ----------------------------------------------------

(defmethod json-write ((value vector) &optional stream)
  "Encode an array to a stream."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (stream nil :prefix "[" :suffix "]")
      (when (plusp (length value))
        (json-write (aref value 0)))
      (loop
         for i from 1 below (length value)
         do (progn
              (write-char #\, stream)
              (pprint-newline :fill)
              (pprint-indent :block 0)
              (json-write (aref value i) stream))))))

;;; ----------------------------------------------------

(defmethod json-write ((value list) &optional stream)
  "Encode a list to a stream."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (stream value :prefix "[" :suffix "]")
      (pprint-exit-if-list-exhausted)
      (loop
         (json-write (pprint-pop) stream)
         (pprint-exit-if-list-exhausted)
         (write-char #\, stream)
         (pprint-newline :fill)
         (pprint-indent :block 0)))))

;;; ----------------------------------------------------

(defmethod json-write ((value hash-table) &optional stream)
  "Encode a hash-table to a stream."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (let ((keys (loop for key being each hash-keys in value collect key)))
      (pprint-logical-block (stream keys :prefix "{" :suffix "}")
        (pprint-exit-if-list-exhausted)
        (loop
           (let ((key (pprint-pop)))
             (if (not (stringp key))
                 (progn
                   (warn "~s is not a valid JSON key; skipping...~%" key)
                   (pprint-exit-if-list-exhausted))
               (progn
                 (json-write key stream)
                 (write-char #\: stream)
                 (json-write (gethash key value) stream)
                 (pprint-exit-if-list-exhausted)
                 (write-char #\, stream)
                 (pprint-newline :mandatory)
                 (pprint-indent :current 0)))))))))

;;; ----------------------------------------------------

(defmethod json-write ((value json-object) &optional stream)
  "Encode a JSON object with an associative list of members to a stream."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (stream (json-object-members value)
                                  :prefix "{"
                                  :suffix "}")
      (pprint-exit-if-list-exhausted)
      (loop
         (let ((kv-pair (pprint-pop)))
           (destructuring-bind (k v)
               kv-pair
             (if (not (stringp k))
                 (progn
                   (warn "~s is not a valid JSON key; skipping...~%" k)
                   (pprint-exit-if-list-exhausted))
               (progn
                 (json-write k stream)
                 (write-char #\: stream)
                 (json-write v stream)
                 (pprint-exit-if-list-exhausted)
                 (write-char #\, stream)
                 (pprint-newline :mandatory)
                 (pprint-indent :current 0)))))))))
