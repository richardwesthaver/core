;;; lisp/lib/dat/bencode.lisp --- Bencode Data Format

;; based on https://github.com/nja/cl-bencode/tree/master

;;; Code:
(in-package :dat/bencode)

;;; Dictionary
(define-condition nonstring-dictionary-key (error)
  ((key :initarg :key :reader key)))

(defun make-dictionary (list)
  "Makes a dictionary from a plist or alist.  Keys must be strings."
  (let ((dictionary (make-hash-table :test 'equal)))
    (labels ((add-key-value (key value)
               (if (stringp key)
                   (setf (gethash key dictionary) value)
                   (restart-case (error 'nonstring-dictionary-key :key key)
                     (skip-key ())
                     (use-value (key)
		       :report "Specify string to use as key"
		       :interactive (lambda ()
		       		      (format t "Enter a key string: ")
		       		      (list (read)))
                       (add-key-value key value))))))
      (if (consp (car list))            ; alist
          (dolist (cons list dictionary)
            (destructuring-bind (key . value) cons
              (add-key-value key value)))
          (loop for (key value) on list by #'cddr ; plist
                do (add-key-value key value)))
      dictionary)))

(defparameter *binary-key-p* #'(lambda (x) (equal x '("pieces" "info")))
  "When decoding dictionary values, this function is passed a list,
where the first element is the key of the value. If the dictionary was
in turn a dictionary value, that key is the second element of the
list, and so on. Should a dictionary be a value in a bencoded list,
the corresponding element in the list will be the symbol :list.  When
the function return a true value, the dictionary value will be
binary. Otherwise it will be decoded as a string.

The default function in \*binary-key-p\* returns true for the
\"pieces\" value in the \"info\" dictionary. All other values are
decoded as strings.")

(defun get-dictionary (key dictionary)
  (gethash key dictionary))

(defun binary-dictionary-key-p (key)
  (when (functionp *binary-key-p*)
    (funcall *binary-key-p* key)))

(defun dictionary->alist (dictionary)
  "Returns an alist representation of the dictionary."
  (let ((alist))
    (labels ((add-key-value (key value)
               (if (stringp key)
                   (push (cons key value) alist)
                   (restart-case (error 'nonstring-dictionary-key :key key)
                     (skip-key ())
                     (use-value (key) :report "Specify string to use as key"
                       (add-key-value key value))))))
      (maphash #'add-key-value dictionary)
      (sort alist #'string< :key #'car))))

;;; Encode
(defgeneric bencode-encode (object stream &key external-format)
  (:documentation "Encode object and write it to stream or, if stream
is nil, use an in-memory stream and return the resulting sequence.
The external-format is used when encoding strings.  UTF-8 is the
default."))

(defmethod bencode-encode (object (stream stream) &key (external-format :utf-8))
  (if (typep stream 'flexi-stream)
      (error "No applicable encode method for ~S" object)
      (bencode-encode object (make-flexi-stream stream :external-format external-format))))

(defmethod bencode-encode (object (stream (eql nil)) &key (external-format :utf-8))
  (with-output-to-sequence (stream)
    (bencode-encode object (make-flexi-stream stream :external-format external-format))))

(defmethod bencode-encode ((list list) (stream flexi-stream) &key &allow-other-keys)
  (write-byte (char-code #\l) stream)
  (dolist (x list)
    (bencode-encode x stream))
  (write-byte (char-code #\e) stream))

(defmethod bencode-encode ((dictionary hash-table) (stream flexi-stream) &key &allow-other-keys)
  (write-byte (char-code #\d) stream)
  (dolist (x (dictionary->alist dictionary))
    (destructuring-bind (k . v) x
      (bencode-encode k stream)
      (bencode-encode v stream)))
  (write-byte (char-code #\e) stream))

(defmethod bencode-encode ((string string) (stream flexi-stream) &key &allow-other-keys)
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((length (octet-length string :external-format external-format)))
      (write-sequence (string-header length) stream)
      (write-sequence string stream))))

(defmethod bencode-encode ((integer integer) (stream flexi-stream) &key &allow-other-keys)
  (write-sequence (render-integer integer) stream))

(defmethod bencode-encode ((sequence array) (stream flexi-stream) &key &allow-other-keys)
  (write-sequence (string-header (length sequence)) stream)
  (write-sequence sequence stream))

(defparameter *ascii* (flex:make-external-format :ascii))

(defun string-header (length)
  (string-to-octets (format nil "~a:" length) :external-format *ascii*))

(defun render-integer (integer)
  (string-to-octets (format nil "i~ae" integer) :external-format *ascii*))

;;; Decode
defvar *dictionary-keys* nil)

(defmacro restart-case-loop (form &body clauses)
  `(loop (restart-case (return ,form)
	   ,@clauses)))

(defgeneric bencode-decode (input &key external-format)
  (:documentation "Decode a bencode object from a stream or sequence.
If input is a flexi-stream, its external-format will be used when
decoding strings.  If input is a string, all characters must have
char-codes that fit in an (unsigned-byte 8). Otherwise, the value of
the external-format parameter is used to create a flexi-stream for
decoding.  The default is UTF-8."))

(defmethod bencode-decode ((stream stream) &key (external-format :utf-8))
  (bencode-decode (make-flexi-stream stream :external-format external-format)))

(defmethod bencode-decode ((string string) &key (external-format :utf-8))
  (bencode-decode (map '(vector (unsigned-byte 8))
	       #'char-code string) :external-format external-format))

(defmethod bencode-decode ((sequence sequence) &key (external-format :utf-8))
  (restart-case-loop (with-input-from-sequence (stream sequence)
		       (bencode-decode (make-flexi-stream stream :external-format external-format)))
    (retry-sequence (new-external-format)
		    :report "Set external format and retry decoding the sequence from the beginning"
		    :interactive read-external-format
		    (setf external-format new-external-format))))

(defmethod bencode-decode ((stream flexi-stream) &key &allow-other-keys)
  (let ((c (code-char (peek-byte stream))))
    (case c
      (#\i (bencode-decode-integer stream))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	 (bencode-decode-string stream))
      (#\l (bencode-decode-list stream))
      (#\d (bencode-decode-dictionary stream))
      (t (error 'invalid-value-type :octet c)))))

(define-condition unexpected-octet (error)
  ((expected-octet :initarg :expected-octet :reader expected-octet)
   (actual-octet :initarg :actual-octet :reader actual-octet)))

(define-condition invalid-value-type (error)
  ((octet :initarg :octet :reader octet)))

(defun must-read-char (stream char)
  (restart-case
      (let ((byte (read-byte stream)))
        (if (eql byte (char-code char))
            t
            (error 'unexpected-octet
		   :expected-octet (char-code char)
		   :actual-octet byte)))
    (continue () t)))

(defun maybe-read-char (stream char)
  (if (eql (peek-byte stream nil t) (char-code char))
      (code-char (read-byte stream :eof-error-p t))
      nil))

(defun bencode-decode-integer (stream)
  (must-read-char stream #\i)
  (let* ((minus (maybe-read-char stream #\-))
         (integers (read-integers stream))
         (number (parse-integer integers)))
    (if (= number 0)
	(when (or minus (> (length integers) 1))
	  (restart-case (error "Zero must be i0e") (continue ())))
	(when (char= (elt integers 0) #\0)
	  (restart-case (error "Zero-padded integer") (continue ()))))
    (must-read-char stream #\e)
    (if minus
        (- number)
        number)))

(defun read-integers (stream)
  (with-output-to-string (string)
    (loop for octet = (peek-byte stream)
          while (digit-char-p (code-char octet))
          do (write-char (code-char (read-byte stream)) string))))

(defun read-external-format ()
  (format t "Enter a flexi-stream external format: ")
  (multiple-value-list (eval (read))))

(defun must-read-octets (stream length)
  (let* ((array (make-array length :element-type '(unsigned-byte 8)))
	 (read (read-sequence array stream)))
    (if (= read length)
	array
	(restart-case (error "EOF before string end")
	  (continue () (adjust-array array read))))))

(defun bencode-decode-string (stream)
  (with-accessors ((external-format flexi-stream-external-format))
      stream
    (let ((length (parse-integer (read-integers stream))))
      (must-read-char stream #\:)
      (let ((octets (must-read-octets stream length)))
	(restart-case-loop (octets-to-string octets :external-format external-format)
	  (use-binary ()
		      :report "Use undecoded binary vector"
		      (return octets))
	  (retry-string (new-external-format)
			:report "Set external format and continue decoding from the start of the string"
			:interactive read-external-format
			(setf external-format new-external-format)))))))

(defun bencode-decode-list (stream)
  (must-read-char stream #\l)
  (loop until (maybe-read-char stream #\e)
        collect (let ((*dictionary-keys* (cons :list *dictionary-keys*)))
		  (bencode-decode stream))))

(defun bencode-decode-binary-string (stream)
  (let ((length (parse-integer (read-integers stream))))
    (must-read-char stream #\:)
    (must-read-octets stream length)))

(defun bencode-decode-dictionary (stream)
  (must-read-char stream #\d)
  (loop with list
	with previous-key
        until (maybe-read-char stream #\e)
        do (let ((key (bencode-decode-string stream)))
	     (when (and previous-key (not (string< previous-key key)))
	       (restart-case (error "Key ~S before key ~S in dict" previous-key key)
		 (continue ())))
             (let* ((*dictionary-keys* (cons key *dictionary-keys*))
                    (value (if (binary-dictionary-key-p *dictionary-keys*)
                               (bencode-decode-binary-string stream)
                               (bencode-decode stream))))
               (push value list)
               (push key list)
               (setf previous-key key)))
        finally (return (make-dictionary list))))
