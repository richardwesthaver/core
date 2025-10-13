;;; lib/dat/json.lisp --- JSON format

;; JSON parser generator

;; There are quite a few json libraries in the CL ecosystem. This
;; particular implementation is based on the JSON package here:
;; https://github.com/massung/json

;; It's object-based (like CL-JSON) instead of using a
;; parser-generator.

;;; Code:
(in-package :dat/json)

(defvar *allow-json-trailing-commas* nil
  "When non-nil, arrange for our json readers to allow trailing
commas. This binding does not affect writers.

Trailing commas in json lists and objects is a common source of frustration
since they're not allowed in the spec. This is easily forgotten when
generating json from a scripting language without native json support.")

(defun json-trailing-commas-p () *allow-json-trailing-commas*)

(defsetf json-trailing-commas-p () (val)
  `(setq *allow-json-trailing-commas* ,val))

(defclass json-object (ast) ()
  (:documentation "An associative list of key/value pairs.")
  (:default-initargs :ast nil))

(defmethod print-object ((obj json-object) stream)
  "Output a JSON object to a stream in readable form."
  (print-unreadable-object (obj stream :type t)
    (let ((*print-level* 1))
      (json-encode obj stream))))

(defun json-getf (object key &optional value)
  "Find an member's value in a JSON object."
  (let ((place (assoc key (ast object) :test 'string=)))
    (if (null place)
        value
      (values (second place) t))))

(defun json-setf (object key value)
  "Assign a value to a key in a JSON object."
  (let ((place (assoc key (ast object) :test 'string=)))
    (prog1 value
      (if (null place)
          (let ((k (if (stringp key)
                       key
                     (princ-to-string key))))
            (push (list k value) (ast object)))
        (rplacd place (list value))))))

(defsetf json-getf json-setf)

(defun json-remf (obj key)
  "Destructively alter OBJ to remove key/value indicated by KEY. Returns T if
such a key was present, else NIL."
  (let ((place (assoc key (ast obj) :test 'string=)))
    (unless (null place)
      (deletef (ast obj) place))))

(defun json-decode (string &key (start 0) end)
  "Convert a JSON string into a Lisp object."
  (with-input-from-string (stream string :start start :end end)
    (values (json-read stream)
            (file-position stream))))

(defmethod deserialize ((obj string) (format (eql :json)) &key (start 0) end)
  (declare (ignore format))
  (json-decode obj :start start :end end))

(defmethod deserialize ((obj pathname) (format (eql :json)) &key)
  (declare (ignore format))
  (with-open-file (f obj)
    (json-read f)))

(defmethod deserialize ((obj stream) (format (eql :json)) &key)
  (declare (ignore format))
  (json-read obj))

(defun json-encode (value &optional stream)
  "Encodes a Lisp value into a stream."
  (json-write value stream))

(defmethod serialize (obj (format (eql :json)) &key stream path)
  (declare (ignore format))
  (if stream
      (json-encode obj stream)
      (if path
          (with-open-file (stream path :direction :output)
            (json-encode obj stream))
          (with-output-to-string (stream)
            (json-encode obj stream)))))

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
      (t (json-read-number stream)))))

(defun json-peek-char (stream expected &key skip-ws)
  "Peek at the next character or token and optionally error if unexpected."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (equal (peek-char skip-ws stream) expected)
    (read-char stream)))

(defun json-read-char (stream expected &key skip-ws)
  "Read the next, expected character in the stream."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (json-peek-char stream expected :skip-ws skip-ws)
      t
    (error "JSON error: unexpected ~s~%expected ~A" (read-char stream) expected)))

(defun json-read-true (stream)
  "Read true from a JSON stream."
  (json-read-char stream #\t :skip-ws t)
  (json-read-char stream #\r)
  (json-read-char stream #\u)
  (json-read-char stream #\e))

(defun json-read-false (stream)
  "Read false from a JSON stream."
  (prog1 nil
    (json-read-char stream #\f :skip-ws t)
    (json-read-char stream #\a)
    (json-read-char stream #\l)
    (json-read-char stream #\s)
    (json-read-char stream #\e)))

(defun json-read-null (stream)
  "Read null from a JSON stream."
  (prog1 nil
    (json-read-char stream #\n :skip-ws t)
    (json-read-char stream #\u)
    (json-read-char stream #\l)
    (json-read-char stream #\l)))

(defun json-read-number (stream)
  "Read a number from a JSON stream."
  (declare (optimize (speed 3) (debug 0) (safety 0)))
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

(defun json-read-string (stream)
  "Read a string from a JSON stream."
  (declare (optimize (speed 3) (debug 0) (safety 0)))

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

(defun json-read-list (stream)
  "Read a list of JSON values."
  (declare (optimize (speed 3) (debug 0) (safety 0)))

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
      while (and (json-peek-char stream #\, :skip-ws t)
                  (unless (and (json-trailing-commas-p) (equal #\] (peek-char t stream)))
                   t))
       ;; return the final list
       finally (return (prog1 xs
                         (json-read-char stream #\] :skip-ws t))))))

(defun json-read-object (stream)
  "Read an associative list of key/value pairs into a JSON object."
  (declare (optimize (speed 3) (debug 0) (safety 0)))

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
       while (and (json-peek-char stream #\, :skip-ws t)
                  (unless (and (json-trailing-commas-p) (equal #\} (peek-char t stream)))
                    t))
      ;; return the final list
      finally (return (prog1 (make-instance 'json-object :ast xs)
                        (json-read-char stream #\} :skip-ws t))))))

(defmethod json-write ((value (eql t)) &optional stream)
  "Encode the true value."
  (declare (ignore value))
  (format stream "~<true~>"))

(defmethod json-write ((value null) &optional stream)
  "Encode the null constant."
  (declare (ignore value))
  (format stream "~<null~>"))

(defmethod json-write ((value symbol) &optional stream)
  "Encode a symbol to a stream."
  (json-write (symbol-name value) stream))

(defmethod json-write ((value number) &optional stream)
  "Encode a number to a stream."
  (format stream "~<~a~>" value))

(defmethod json-write ((value ratio) &optional stream)
  "Encode a ratio to a stream."
  (format stream "~<~a~>" (float value)))

(defmethod json-write ((value character) &optional stream)
  (json-write (string value) stream))

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

(defmethod json-write ((value pathname) &optional stream)
  "Encode a pathname as a stream."
  (json-write (namestring value) stream))

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

(defmethod json-write ((value ast) &optional stream)
  "Encode an object with an associative list of members to a stream. There must
be an AST accessor present which points to the list."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (stream (ast value)
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

(defmethod json-write ((value uuid:uuid) &optional stream)
  "Encode a pathname as a stream."
  (json-write (string-downcase (uuid:uuid-to-string value)) stream))

(defmethod json-write ((value time:timestamp) &optional stream)
  "Encode a pathname as a stream."
  (json-write (time:format-rfc3339-timestring nil value) stream))

(defmethod json-write ((value uri:uri) &optional stream)
  "Encode a pathname as a stream."
  (json-write (uri:uri-to-string value) stream))

;;; Reader Macro
;; not used, but maybe useful some day
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
                return (make-instance 'json-object :ast pairs)

                ;; build associative list of key/value pairs
                collect (list (princ-to-string key) value)
                into pairs))))
    (set-dispatch-macro-character #\# #\{ #'json-object-reader)
    (set-macro-character #\} (get-macro-character #\) nil))))

;;; Json Pointer
;; ref: https://datatracker.ietf.org/doc/html/rfc6901
#| abnf

      json-pointer    = *( "/" reference-token )
      reference-token = *( unescaped / escaped )
      unescaped       = %x00-2E / %x30-7D / %x7F-10FFFF
         ; %x2F ('/') and %x7E ('~') are excluded from 'unescaped'
      escaped         = "~" ( "0" / "1" )
        ; representing '~' and '/', respectively
|#

(defun json-pointer-token-encode (str)
  "Return a new string with '~' replaced with '~0' and '/' replaced with '~1'."
  (let ((ret))
    (loop for c across str
          if (char= c #\~)
          do (progn
               (push #\~ ret)
               (push #\0 ret))
          else if (char= c #\/)
          do (progn
               (push #\~ ret)
               (push #\1 ret))
          else do (push c ret)
          finally (return (concatenate 'string (nreverse ret))))))
          
(defun json-pointer-token-decode (str)
  "Return a new string with '~0' replace by '~' and '~1' replace by '/'."
  (let ((ret)
        (tilde))
    (loop for c across str
          if (char= c #\~) do (setf tilde t)
          else if (and tilde (char= #\0 c)) do (progn (push #\~ ret) (setf tilde nil))
          else if (and tilde (char= #\1 c)) do (progn (push #\/ ret) (setf tilde nil))
          ;; is this a syntax error?
          else if tilde do (progn (push #\~ ret) (push c ret) (setf tilde nil))
          else do (push c ret)
          finally (return (concatenate 'string (nreverse ret))))))

(defun json-pointer-p (str)
  "Return t if STR is a JSON Pointer -- a unicode string containing a sequence of
zero or more reference tokens prefixed by a '/'."
  (char= (schar str 0) #\/))

(defun json-pointer-from-string (str)
  (mapcar 'json-pointer-token-decode (ssplit #\/ str :omit-nulls t)))

;;; Json Reference
;; https://datatracker.ietf.org/doc/html/draft-pbryan-zyp-json-ref-03
(defvar *default-json-resolver* #'identity)

(defun resolve-json-reference (obj resolver)
  "Resolve a json reference of the form (\"$ref\" REF) where REF is either a uri
or a string with support for json pointers. RESOLVER is a function which is
passed the reference REF and is responsible for resolving it to a JSON value."
  (let ((ref (etypecase obj
               (json-object (json-getf obj "$ref"))
               (cons (assoc "$ref" obj :test 'string=)))))
    (funcall resolver 
             (if (json-pointer-p ref) 
                 (json-pointer-token-decode ref)
                 (uri ref)))))

(defun resolve-json-references (obj &optional (resolver *default-json-resolver*))
  "Expand all json references contained in the AST slot of json-object
OBJ. For each reference found RESOLVE-JSON-REFERENCE is called with OBJ and
RESOLVER then replaced with the result."
  (loop with ref = (json-getf obj "$ref")
        while ref
        do (json-setf obj "$ref" (resolve-json-reference ref resolver))))

;;; Json Schema
;; ref: https://json-schema.org/specification
;; examples: https://json-schema.org/learn/json-schema-examples
(defclass json-schema-object (json-object id)
  ((id :type uri)))

(defclass json-schema (json-schema-object)
  ((schema :type uri)
   (title :type string)
   (description :type string)
   (type :type string)
   (properties :type list)
   (required :type list)
   (defs :type list))
  (:documentation "Class which represents JSON Schema documents.

ref: https://json-schema.org"))

(defun json-schema-validate (schema obj)
  (declare (json-object obj) (json-schema schema))
  (when (and (ast schema) (ast obj))
    obj))

(defvar *json-schema-key-map*
  (let ((tbl (make-hash-table :test 'equal)))
    (flet ((add (key slot &optional fn push)
             (setf (gethash key tbl)
                   (lambda (x &optional consume)
                     (declare (json-object x))
                     (when-let ((val (json-getf x key)))
                       (when consume (json-remf x key))
                       (let ((ret (ifret (when fn (funcall fn val))
                                    val)))
                         (if push
                             (push ret (slot-value x slot))
                             (setf (slot-value x slot) ret))))))))
      (add "$id" 'id 'uri)
      (add "$schema" 'schema 'uri)
      (add "description" 'description)
      (add "type" 'type 'string)
      (add "properties" 'properties
           (lambda (x)
             (if (and (atom x) (not (typep x 'ast)))
                 x
                 (mapcar 
                  (lambda (y) (cons (car y) (ast (cadr y))))
                  (ast x)))))
      (add "required" 'required
           (lambda (x)
             (if (atom x)
                 x
                 (mapcar
                  (lambda (y)
                    (if (stringp y) y (ast y)))
                  x))))
      (add "$defs" 'defs 'ast)
      (add "dependentRequired" 'required 'ast t))
    tbl))

(defmethod load-ast ((self json-schema))
  (maphash-values (lambda (x) (funcall x self t)) *json-schema-key-map*)
  self)

(defmethod deserialize ((obj json-object) (fmt (eql :json-schema)) &key)
  (declare (ignore fmt))
  (load-ast (change-class obj 'json-schema)))

(defmethod deserialize ((obj t) (fmt (eql :json-schema)) &key)
  (declare (ignore fmt))
  (deserialize (the json-object (deserialize obj :json)) :json-schema))

(defmethod validate ((obj json-object) (schema json-schema) &key (default :error))
  "Check json-object OBJ against json-schema SCHEMA and return it if valid. If
validation fails then the parameter DEFAULT determines the result. A keyword
value of :error (the default) will signal an error, all other values will be
returned as is."
  (ifret (json-schema-validate schema obj)
    (if (eql default :error)
        (error "JSON-OBJECT failed validation")
        default)))
  
;; (defun json-schema (obj)
;;   "Attempt to convert a json-object OBJ to a json-schema.")
