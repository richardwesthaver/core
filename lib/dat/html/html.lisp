;;; dat/html.lisp --- HTML parser

;; see https://github.com/rotatef/cl-html5-parser

;;; Commentary:

;; spec: https://html.spec.whatwg.org/

;; HTML is usually associated with XML, but not all HTML is valid
;; XML.

;; This package provides a pretty good HTML parser, with the default
;; DOM being the one used in our XML package DAT/XML.

;;; Code:

;;; inputstream
(in-package :dat/html)

(deftype array-length ()
  "Type of an array index."
  '(integer 0 #.array-dimension-limit))

(deftype chunk ()
  "Type of the input stream buffer."
  '(vector character *))

(defparameter *default-encoding* :utf-8)

(defclass html-input-stream ()
  ((source :initarg :source)
   (encoding :reader html5-stream-encoding)
   (char-stream :initform nil)
   (chunk)
   (chunk-offset)
   (pending-cr)
   (errors :initform nil :accessor html5-stream-errors)))

(defun make-html-input-stream (source &key override-encoding fallback-encoding)
  (when (stringp source)
    ;; Encoding is not relevant when input is a string,
    ;; but we set it utf-8 here to avoid auto detecting taking place.
    (setf override-encoding :utf-8))
  (let ((self (make-instance 'html-input-stream :source source)))
    (with-slots (encoding stream) self
      (setf encoding (detect-encoding self
                                      (find-encoding override-encoding)
                                      (find-encoding fallback-encoding)))
      (open-char-stream self))
    self))

;; 12.2.2.2 Character encodings

;; REVIEW 2025-06-11: used to validate the format with flexi-streams - now
;; just using the CAR of the NAMES slot of the internal EXTERNAL-FORMAT
;; structure in SB-IMPL
(defun find-encoding (encoding-name)
  ;; Normalize the string designator
  (setf encoding-name (string-upcase (substitute #\- #\_ (string-trim +space-characters+ (string encoding-name)))))
  ;; All known encoding will already be interned in the keyword package so find-symbol is fine here
  (setf encoding-name 
        (if-let ((known (sb-int:get-external-format encoding-name)))
          (car (sb-impl::ef-names known))
          *default-encoding*)))

;; 12.2.2.1 Determining the character encoding
(defun detect-encoding (stream override-encoding fallback-encoding)
  (with-slots (encoding) stream
    (block nil
      ;; 1. and 2. encoding overridden by user or transport layer
      (when override-encoding
        (return (cons override-encoding :certain)))

      ;; 3. wait for 1024 bytes, not implemented

      ;; 4. Detect BOM
      (let ((bom-encoding (detect-bom stream)))
        (when bom-encoding
          (return (cons bom-encoding :certain))))

      ;; 5. Prescan not implemented

      ;; 6. Use fallback encoding
      (when fallback-encoding
        (return (cons encoding :tentative)))

      ;; 7. Autodect not implemented

      ;; 8. Implementation-defined default
      (return (cons *default-encoding* :tentative)))))

(defmacro handle-encoding-errors (stream &body body)
  ;; REVIEW 2025-06-11: used to be flexi-streams external-format handling
  ;; here.
  `(handler-bind ((stream-error
                    (lambda (x)
                      (declare (ignore x))
                      (push :invalid-codepoint (html5-stream-errors ,stream))
                      (use-value #\uFFFD))))
     ,@body))

(defun open-char-stream (self)
  (with-slots (source encoding char-stream chunk chunk-offset pending-cr) self
    (setf chunk (make-array (* 10 1024) :element-type 'character :fill-pointer 0))
    (setf chunk-offset 0)
    (setf pending-cr nil)
    (when char-stream
      (close char-stream))

    (setf char-stream
          (if (stringp source)
              ;; REVIEW 2025-06-11: this used to be flexi-streams stuff - test
              ;; because we probably broke something..
              (make-string-input-stream source)
              ;; (make-instance 'sb-gray:fundamental-character-input-stream)
              (etypecase source
                (pathname
                 (open source :element-type '(unsigned-byte 8) :external-format (car encoding)))
                (stream
                 source)
                (vector
                 (let ((s (make-instance 'sb-gray:fundamental-binary-stream)))
                   (write-sequence source s)
                   s)))))
    ;; 12.2.2.4 says we should always skip the first byte order mark
    (handle-encoding-errors self
      (let ((first-char (peek-char nil char-stream nil)))
        (when (eql first-char #\ufeff)
          (read-char char-stream))))))

(defun detect-bom (self)
  (with-slots (source) self
    (let (byte-0 byte-1 byte-2)
      (etypecase source
        (vector
         (when (> (length source) 0) (setf byte-0 (aref source 0)))
         (when (> (length source) 1) (setf byte-1 (aref source 1)))
         (when (> (length source) 2) (setf byte-2 (aref source 2))))
        (pathname
         (with-open-file (in source :element-type '(unsigned-byte 8))
           (setf byte-0 (read-byte in nil))
           (setf byte-1 (read-byte in nil))
           (setf byte-2 (read-byte in nil))))
        (stream
         (error "Can't detect encoding when source is a stream.")))
      (cond ((and (eql byte-0 #xfe)
                  (eql byte-1 #xff))
             :utf-16be)
            ((and (eql byte-0 #xff)
                  (eql byte-1 #xfe))
             :utf-16le)
            ((and (eql byte-0 #xef)
                  (eql byte-1 #xbb)
                  (eql byte-2 #xbf))
             :utf-8)))))

;; 12.2.2.3 Changing the encoding while parsing
(defun html5-stream-change-encoding (stream new-encoding)
  (setf new-encoding (find-encoding new-encoding))
  (with-slots (encoding char-stream) stream
    ;; 1.
    (when (member (car encoding) '(:utf-16le :utf-16be))
      (setf encoding (cons (car encoding) :certain))
      (return-from html5-stream-change-encoding))

    ;; 2.
    (when (member new-encoding '(:utf-16le :utf-16be))
      (setf new-encoding :utf-8))

    ;; 3.
    (when (eql (car encoding) new-encoding)
      (setf encoding (cons (car encoding) :certain))
      (return-from html5-stream-change-encoding))

    ;; 4. Not impleneted

    ;; 5. Restart paring from scratch
    (setf encoding (cons new-encoding :certain))
    (open-char-stream stream)
    (throw 'please-reparse t)))

(defun html5-stream-char (stream)
  (with-slots (chunk chunk-offset) stream
    (when (>= chunk-offset (length chunk))
      (unless (read-chunk stream)
        (return-from html5-stream-char +eof+)))
    (prog1 (char chunk chunk-offset)
      (incf chunk-offset))))

(defun our-scan (chars opposite-p chunk &key start)
  (loop for i from start below (length chunk)
        for char = (char chunk i)
        while (if opposite-p
                  (position char chars)
                  (not (position char chars)))
        finally (return i)))

(defun html5-stream-chars-until (stream characters &optional opposite-p)
  "Returns a string of characters from the stream up to but not
   including any character in characters or end of file.
   "
  (with-slots (chunk chunk-offset) stream
    (declare (array-length chunk-offset) (chunk chunk))
    (with-output-to-string (data)
      (loop for end = (our-scan characters opposite-p chunk :start chunk-offset) do
            ;; If nothing matched then stop
               (unless end
                 (return))
               ;; If not the whole chunk matched, return everything
               ;; up to the part that didn't match
               (when (and end
                          (/= chunk-offset (length chunk)))
                 (write-string chunk data :start chunk-offset :end end)
                 (setf chunk-offset end)
                 (return))
               ;; If the whole remainder of the chunk matched,
               ;; use it all and read the next chunk
               (write-string chunk data :start chunk-offset)
               (unless (read-chunk stream)
                 (return))))))

(defun html5-stream-unget (stream char)
  (with-slots (chunk chunk-offset) stream
    (unless (eql char +eof+)
      (cond ((zerop chunk-offset)
             (cond ((< (fill-pointer chunk) (array-dimension chunk 0))
                    (incf (fill-pointer chunk))
                    (replace chunk chunk :start1 1))
                   (t
                    (let ((new-chunk (make-array (1+ (array-dimension chunk 0))
                                                 :element-type 'character
                                                 :fill-pointer (1+ (fill-pointer chunk)))))
                      (replace new-chunk chunk :start1 1)
                      (setf chunk new-chunk))))
             (setf (char chunk 0) char))
            (t
             (decf chunk-offset)
             (assert (char= char (char chunk chunk-offset))))))))

(defun read-chunk (stream)
  (declare (optimize speed))
  (with-slots (char-stream chunk chunk-offset pending-cr) stream
    (declare (array-length chunk-offset)
             (chunk chunk))
    (setf chunk-offset 0)
    (let ((start 0))
      (when pending-cr
        (setf (char chunk 0) #\Return)
        (setf start 1)
        (setf pending-cr nil))
      (setf (fill-pointer chunk) (array-dimension chunk 0))
      (handle-encoding-errors stream
        (setf (fill-pointer chunk) (read-sequence chunk char-stream :start start)))

      (unless (zerop (length chunk))

        ;; check if last char is CR and EOF was not reached
        (when (and (= (length chunk) (array-dimension chunk 0))
                   (eql (char chunk (1- (length chunk))) #\Return))
          (setf pending-cr t)
          (decf (fill-pointer chunk)))

        (report-character-errors stream chunk)

        ;; Python code replaces surrugate pairs with U+FFFD here. Why?

        ;; Normalize line endings (CR LF)
        (loop for previous = nil then current
              for current across chunk
              for index of-type array-length from 0
              with offset of-type array-length = 0
              do (unless (and (eql previous #\Return)
                              (eql current #\Newline))
                   (unless (= index offset)
                     (setf (char chunk offset) current))
                   (when (eql current #\Return)
                     (setf (char chunk offset) #\Newline))
                   (incf offset))
              finally (setf (fill-pointer chunk) offset))
        t))))

(defparameter *invalid-unicode*
  `(,@(char-range #\u0001 #\u0008)
    #\u000B
    ,@(char-range #\u000E #\u001F)
    ,@(char-range #\u007F #\u009F)
    ;; The following are noncharacter as defined by Unicode.
    ,@`(
        ,@(char-range #\uD800 #\uDFFF)
        ,@(char-range #\uFDD0 #\uFDEF)
        #\uFFFE
        #\uFFFF
        #\u0001FFFE
        #\u0001FFFF
        #\u0002FFFE
        #\u0002FFFF
        #\u0003FFFE
        #\u0003FFFF
        #\u0004FFFE
        #\u0004FFFF
        #\u0005FFFE
        #\u0005FFFF
        #\u0006FFFE
        #\u0006FFFF
        #\u0007FFFE
        #\u0007FFFF
        #\u0008FFFE
        #\u0008FFFF
        #\u0009FFFE
        #\u0009FFFF
        #\u000AFFFE
        #\u000AFFFF
        #\u000BFFFE
        #\u000BFFFF
        #\u000CFFFE
        #\u000CFFFF
        #\u000DFFFE
        #\u000DFFFF
        #\u000EFFFE
        #\u000EFFFF
        #\u000FFFFE
        #\u000FFFFF
        #\u0010FFFE
        #\u0010FFFF)))

(defparameter *invalid-unicode-hash* (make-hash-table))
(dolist (char *invalid-unicode*)
  (setf (gethash char *invalid-unicode-hash*) char))

(defun report-character-errors (stream data)
  (loop for char across data
        when (gethash char *invalid-unicode-hash*)
        do (push :invalid-codepoint (html5-stream-errors stream))))

;;; Tokenizer
(defclass html-tokenizer ()
  ((stream :initarg :stream :reader tokenizer-stream)
   (cdata-switch-helper :initarg :cdata-switch-helper
                        :initform (constantly nil))
   (lowercase-element-name :initform t)
   (lowercase-attr-name :initform t)
   (escape-flag :initform nil)
   (last-four-chars :initform nil)
   (state :initform :data-state :accessor tokenizer-state)
   (escape :initform nil)
   (current-token :initform nil)
   (token-queue :initform nil)
   (temporary-buffer :initform nil)))

(defun make-html-tokenizer (source &key encoding cdata-switch-helper)
  "Convert SOURCE to a html-input-stream, wrap and return it in a html-tokenizer."
  (make-instance 'html-tokenizer
    :stream (make-html-input-stream source :override-encoding encoding)
    :cdata-switch-helper cdata-switch-helper))

(defun map-tokens (tokenizer function)
  "Return next token or NIL on eof"
  (with-slots (token-queue stream) tokenizer
    (loop while (run-state tokenizer) do
             (setf token-queue (nreverse token-queue))
             (loop while (html5-stream-errors stream)
                   do (funcall function (list :type :parse-error :data (pop (html5-stream-errors stream)))))
             (loop while token-queue
                   do (funcall function (pop token-queue))))))

(defun run-state (tokenizer)
  "Run the current state of TOKENIZER."
  (run-state* tokenizer (slot-value tokenizer 'state)))

(defgeneric run-state* (tokenizer state)
  (:documentation "Complete the given STATE for TOKENIZER."))

(defmacro defstate (state (&rest slots) &body body)
  `(defmethod run-state* (self (state (eql ,state)))
     (with-slots (,@slots) self
       (block nil
         ,@body
         t))))

(defun push-token (self token)
  "Push TOKEN to SELF."
  (with-slots (token-queue) self
    (push token token-queue)))

(defun push-token* (self type &rest data)
  "Push a token with :type type and :data the a string concatenation of data"
  (push-token self (list :type type
                         :data (apply #'nconcat (make-growable-string) data))))

(defun add-attribute (token name)
  (setf (getf token :data) (append (getf token :data)
                                   (list (cons (make-growable-string (string name))
                                               (make-growable-string))))))

(defun add-to-attr-name (token &rest data)
  (setf (caar (last (getf token :data)))
        (apply #'nconcat
               (caar (last (getf token :data)))
               data)))

(defun add-to-attr-value (token &rest data)
  (setf (cdar (last (getf token :data)))
        (apply #'nconcat
               (cdar (last (getf token :data)))
               data)))

(defun add-to (token indicator &rest data)
  (setf (getf token indicator)
        (apply #'nconcat
               (getf token indicator)
               data)))

(defun consume-number-entity (self is-hex)
  "This function returns either U+FFFD or the character based on the
decimal or hexadecimal representation. It also discards \";\" if present.
If not present a token (:type :parse-error) is emitted."
  (with-slots (stream) self
    (let ((allowed +digits+)
          (radix 10)
          (char-stack)
          (c)
          (char-as-int)
          (char))
      (when is-hex
        (setf allowed +hex-digits+)
        (setf radix 16))
      ;; Consume all the characters that are in range while making sure we
      ;; don't hit an EOF.
      (setf c (html5-stream-char stream))
      (loop while (and (find c allowed) (not (eql c +eof+))) do
               (push c char-stack)
               (setf c (html5-stream-char stream)))
      ;; Convert the set of characters consumed to an int.
      (setf char-as-int (parse-integer (coerce (nreverse char-stack) 'string) :radix radix))
      ;; Certain characters get replaced with others
      (cond ((find char-as-int +replacement-characters+)
             (setf char (getf +replacement-characters+ char-as-int))
             (push-token self `(:type :parse-error
                                :data :illegal-codepoint-for-numeric-entity
                                :datavars '(:char-as-int ,char-as-int))))
            ((or (<= #xD800 char-as-int #xDFFF)
                 (> char-as-int #x10FFFF))
             (setf char #\uFFFD)
             (push-token self `(:type :parse-error
                                :data :illegal-codepoint-for-numeric-entity
                                :datavars '(:char-as-int ,char-as-int))))
            (t
             ;; Python comment: Should speed up this check somehow (e.g. move the set to a constant)
             (when (or (<= #x0001 char-as-int #x0008)
                       (<= #x000E char-as-int #x001F)
                       (<= #x007F char-as-int #x009F)
                       (<= #xFDD0 char-as-int #xFDEF)
                       (find char-as-int
                             #(#x000B #xFFFE #xFFFF #x1FFFE
                               #x1FFFF #x2FFFE #x2FFFF #x3FFFE
                               #x3FFFF #x4FFFE #x4FFFF #x5FFFE
                               #x5FFFF #x6FFFE #x6FFFF #x7FFFE
                               #x7FFFF #x8FFFE #x8FFFF #x9FFFE
                               #x9FFFF #xAFFFE #xAFFFF #xBFFFE
                               #xBFFFF #xCFFFE #xCFFFF #xDFFFE
                               #xDFFFF #xEFFFE #xEFFFF #xFFFFE
                               #xFFFFF #x10FFFE #x10FFFF)))
               (push-token self `(:type :parse-error
                                  :data :illegal-codepoint-for-numeric-entity
                                  :datavars '(:char-as-int ,char-as-int))))
             ;; Assume char-code-limit >= 1114112
             (setf char (code-char char-as-int))))
      ;; Discard the ; if present. Otherwise, put it back on the queue and
      ;; invoke parseError on parser.
      (unless (eql c #\;)
        (push-token self `(:type :parse-error :data :numeric-entity-without-semicolon))
        (html5-stream-unget stream c))
      (string char))))

(defun consume-entity (self &key allowed-char from-attribute)
  (with-slots (stream current-token) self
    (let ((output "&")
          (stack (list (html5-stream-char stream))))
      (cond ((or (find (car stack) +space-characters+)
                 (find (car stack) '(+eof+ #\< #\&))
                 (and allowed-char (eql allowed-char (car stack))))
             (html5-stream-unget stream (car stack)))
            ((eql (car stack) #\#)
             (push (html5-stream-char stream) stack)
             (let ((is-hex (find (car stack) "xX")))
               (when is-hex
                 (push (html5-stream-char stream) stack))
               (cond ((find (car stack) (if is-hex +hex-digits+ +digits+))
                      (html5-stream-unget stream (car stack))
                      (setf output (consume-number-entity self is-hex)))
                     (t
                      (push-token self '(:type :parse-error :data :expected-numeric-entity))
                      (html5-stream-unget stream (pop stack))
                      (when is-hex
                        (html5-stream-unget stream (pop stack)))
                      (html5-stream-unget stream (pop stack))))))
            (t
             ;; Consume the maximum number of characters possible, with the
             ;; consumed characters matching one of the identifiers in the first
             ;; column of the named character references table
             ;; (in a case-sensitive manner).
             (let ((entity)
                   (match-at 0))
               (loop with node = *entities-tree*
                     for char = (car stack) then (car (push (html5-stream-char stream)
                                                            stack))
                     for next-node = (assoc char node)
                     while next-node
                     do (when (second next-node)
                          (setf entity (second next-node))
                          (setf match-at (length stack)))
                     do (setf node (cddr next-node)))
               (let ((next-char))
                 ;; Unconsume those characters that are not part of the match
                 ;; This unconsumes everything if there where no match
                 (loop until (= (length stack) match-at) do
                          (setf next-char (car stack))
                          (html5-stream-unget stream (pop stack)))
                 (cond ((not entity)
                        ;; If no match can be made, then no characters are consumed, and nothing is returned.
                        ;; Is this always a parse error really?
                        (push-token self '(:type :parse-error :data :expected-named-entity)))
                       ((and from-attribute
                             (not (eql #\; (car stack)))
                             (or (eql next-char #\=)
                                 (find next-char +digits+)
                                 (ascii-letter-p next-char)))
                                        ; Is this a parse error really?
                        (push-token self '(:type :parse-error :data :bogus))
                        (setf output (concatenate 'string "&" (reverse stack))))
                       (t
                        (unless (eql #\; (car stack))
                          (push-token self '(:type :parse-error
                                             :data :named-entity-without-semicolon)))
                        (setf output entity)))))))

      (cond (from-attribute
             (add-to-attr-value current-token output))
            (t
             (push-token* self (if (find (char output 0) +space-characters+)
                                   :space-characters
                                   :characters)
                          output))))))

(defun process-entity-in-attribute (self &key allowed-char)
  (consume-entity self :allowed-char allowed-char :from-attribute t))

(defun emit-current-token (self)
  "This method is a generic handler for emitting the tags. It also sets the state
to :data because that's what's needed after a token has been emitted."
  (with-slots (current-token state lowercase-element-name) self
    (let ((token current-token))
      ;; Add token to the queue to be yielded
      (when (find (getf token :type) +tag-token-types+)
        (when lowercase-element-name
          (setf (getf token :name) (ascii-upper-2-lower (getf token :name))))
        (when (eql (getf token :type) :end-tag)
          (when (getf token :data)
            (push-token self '(:type :parse-error :data :attributes-in-end-tag)))
          (when (getf token :self-closing)
            (push-token self '(:type :parse-error :data :self-closing-flag-on-end-tag)))))
      (push-token self token)
      (setf state :data-state))))

;;; Tokenizer States
;; Below are the various tokenizer states worked out.
(defstate :data-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\&)
           (setf state :entity-data-state))
          ((eql data #\<)
           (setf state :tag-open-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\u0000))
          ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          ((find data +space-characters+)
           ;; Directly after emitting a token you switch back to the "data
           ;; state". At that point spaceCharacters are important so they are
           ;; emitted separately.
           (push-token* self :space-characters
                        data
                        (html5-stream-chars-until stream +space-characters+ t))
           ;; No need to update lastFourChars here, since the first space will
           ;; have already been appended to lastFourChars and will have broken
           ;; any <!-- or --> sequences
           )
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\& #\< #\u0000)))))))

(defstate :entity-data-state (state)
  (consume-entity self)
  (setf state :data-state))

(defstate :rcdata-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\&)
           (setf state :character-reference-in-rcdata))
          ((eql data #\<)
           (setf state :rcdata-less-than-sign-state))
          ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((find data +space-characters+)
           ;; Directly after emitting a token you switch back to the "data
           ;; state". At that point spaceCharacters are important so they are
           ;; emitted separately.
           (push-token* self :space-characters
                        data
                        (html5-stream-chars-until stream +space-characters+ t))
           ;; No need to update lastFourChars here, since the first space will
           ;; have already been appended to lastFourChars and will have broken
           ;; any <!-- or --> sequences
           )
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\& #\<)))))))

(defstate :character-reference-in-rcdata (state)
  (consume-entity self)
  (setf state :rcdata-state))

(defstate :rawtext-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\<)
           (setf state :rawtext-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\< #\u0000)))))))

(defstate :script-data-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\<)
           (setf state :script-data-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\< #\u0000)))))))

(defstate :plaintext-state (stream)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data +eof+)
           ;; Tokenization ends.
           (return nil))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          (t
           (push-token* self :characters
                        data
                        (html5-stream-chars-until stream '(#\u0000)))))))

(defstate :tag-open-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\!)
           (setf state :markup-declaration-open-state))
          ((eql data #\/)
           (setf state :close-tag-open-state))
          ((ascii-letter-p data)
           (setf current-token (list :type :start-tag
                                     :name (make-array 1 :element-type 'character
                                                         :initial-element data
                                                         :fill-pointer t
                                                         :adjustable t)
                                     :data '()
                                     :self-closing nil
                                     :self-closing-acknowledged nil))
           (setf state :tag-name-state))
          ((eql data #\>)
           ;; XXX In theory it could be something besides a tag name. But
           ;; do we really care?
           (push-token self '(:type :parse-error :data :expected-tag-name-but-got-right-bracket))
           (push-token* self :characters "<>")
           (setf state :data-state))
          ((eql data #\?)
           ;; XXX In theory it could be something besides a tag name. But
           ;; do we really care?
           (push-token self '(:type :parse-error :data :expected-tag-name-but-got-question-mark))
           (html5-stream-unget stream data)
           (setf state :bogus-comment-state))
          (t
           ;; XXX
           (push-token self '(:type :parse-error :data :expected-tag-name))
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :data-state)))))

(defstate :close-tag-open-state
    (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (setf current-token (list :type :end-tag
                                     :name (make-array 1 :element-type 'character
                                                         :initial-element data
                                                         :fill-pointer t
                                                         :adjustable t)
                                     :data '()
                                     :self-closing nil))
           (setf state :tag-name-state))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :expected-closing-tag-but-got-right-bracket))
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-closing-tag-but-got-eof))
           (push-token* self :characters "</")
           (setf state :data-state))
          (t
           ;; XXX data can be _'_...
           (push-token self `(:type :parse-error :data :expected-closing-tag-but-got-char
                              :datavars (:data ,data)))
           (html5-stream-unget stream data)
           (setf state :bogus-comment-state))))
  t)

(defstate :tag-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-attribute-name-state))
          ((eql data #\>)
           (emit-current-token self))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-tag-name))
           (setf state :data-state))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (vector-push-extend #\uFFFD (getf current-token :name)))
          (t
           (vector-push-extend data (getf current-token :name))
           ;; (Don't use charsUntil here, because tag names are
           ;; very short and it's faster to not do anything fancy)
           ))))

(defstate :rcdata-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (setf temporary-buffer (make-growable-string))
           (setf state :rcdata-end-tag-open-state))
          (t
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :rcdata-state)))))

(defstate :rcdata-end-tag-open-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (nconcatf temporary-buffer (string data))
           (setf state :rcdata-end-tag-name-state))
          (t
           (push-token* self :characters "</")
           (html5-stream-unget stream data)
           (setf state :rcdata-state)))))

(defstate :rcdata-end-tag-name-state (stream state temporary-buffer current-token)
  (let ((appropriate (and current-token
                          (string-equal (getf current-token :name)
                                        temporary-buffer)))
        (data (html5-stream-char stream)))
    (cond ((and (find data +space-characters+)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :before-attribute-name-state))
          ((and (eql data #\/)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :self-closing-start-tag-state))
          ((and (eql data #\>)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (emit-current-token self)
           (setf state :data-state))
          ((ascii-letter-p data)
           (nconcatf temporary-buffer data))
          (t
           (push-token* self :characters "</" temporary-buffer)
           (html5-stream-unget stream data)
           (setf state :rcdata-state)))))

(defstate :rawtext-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (setf temporary-buffer (make-growable-string))
           (setf state :rawtext-end-tag-open-state))
          (t
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :rawtext-state)))))

(defstate :rawtext-end-tag-open-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (nconcatf temporary-buffer (string data))
           (setf state :rawtext-end-tag-name-state))
          (t
           (push-token* self :characters "</")
           (html5-stream-unget stream data)
           (setf state :rawtext-state)))))

(defstate :rawtext-end-tag-name-state (stream state temporary-buffer current-token)
  (let ((appropriate (and current-token
                          (string-equal (getf current-token :name)
                                        temporary-buffer)))
        (data (html5-stream-char stream)))
    (cond ((and (find data +space-characters+)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :before-attribute-name-state))
          ((and (eql data #\/)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :self-closing-start-tag-state))
          ((and (eql data #\>)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (emit-current-token self)
           (setf state :data-state))
          ((ascii-letter-p data)
           (nconcatf temporary-buffer data))
          (t
           (push-token* self :characters "</" temporary-buffer)
           (html5-stream-unget stream data)
           (setf state :rawtext-state)))))

(defstate :script-data-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (setf temporary-buffer (make-growable-string))
           (setf state :script-data-end-tag-open-state))
          ((eql data #\!)
           (push-token* self :characters "<!")
           (setf state :script-data-escape-start-state))
          (t
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-end-tag-open-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (nconcatf temporary-buffer data)
           (setf state :script-data-end-tag-name-state))
          (t
           (push-token* self :characters "</")
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-end-tag-name-state (stream state temporary-buffer current-token)
  (let ((appropriate (and current-token
                          (string-equal (getf current-token :name)
                                        temporary-buffer)))
        (data (html5-stream-char stream)))
    (cond ((and (find data +space-characters+)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :before-attribute-name-state))
          ((and (eql data #\/)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :self-closing-start-tag-state))
          ((and (eql data #\>)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (emit-current-token self)
           (setf state :data-state))
          ((ascii-letter-p data)
           (nconcatf temporary-buffer data))
          (t
           (push-token* self :characters "</" temporary-buffer)
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-escape-start-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-escape-start-dash-state))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-escape-start-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-escaped-dash-dash-state))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-state)))))

(defstate :script-data-escaped-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-escaped-dash-state))
          ((eql data #\<)
           (setf state :script-data-escaped-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((eql data +eof+)
           (setf state :data-state))
          (t
           (push-token* self :characters data (html5-stream-chars-until stream '(#\< #\- #\u0000)))))))

(defstate :script-data-escaped-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-escaped-dash-dash-state))
          ((eql data #\<)
           (setf state :script-data-escaped-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD)
           (setf state :script-data-escaped-state))
          ((eql data +eof+)
           (setf state :data-state))
          (t
           (push-token* self :characters data (html5-stream-chars-until stream '(#\< #\- #\u0000)))
           (setf state :script-data-escaped-state)))))

(defstate :script-data-escaped-dash-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-"))
          ((eql data #\<)
           (setf state :script-data-escaped-less-than-sign-state))
          ((eql data #\>)
           (push-token* self :characters ">")
           (setf state :script-data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD)
           (setf state :script-data-escaped-state))
          ((eql data +eof+)
           (setf state :data-state))
          (t
           (push-token* self :characters data (html5-stream-chars-until stream '(#\< #\- #\u0000)))
           (setf state :script-data-escaped-state)))))

(defstate :script-data-escaped-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (setf temporary-buffer (make-growable-string))
           (setf state :script-data-escaped-end-tag-open-state))
          ((ascii-letter-p data)
           (push-token* self :characters "<" data)
           (setf temporary-buffer (ascii-upper-2-lower (string data)))
           (setf state :script-data-double-escape-start-state))
          (t
           (push-token* self :characters "<")
           (html5-stream-unget stream data)
           (setf state :script-data-escaped-state)))))

(defstate :script-data-escaped-end-tag-open-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((ascii-letter-p data)
           (setf temporary-buffer (string data))
           (setf state :script-data-escaped-end-tag-name-state))
          (t
           (push-token* self :characters "</")
           (html5-stream-unget stream data)
           (setf state :script-data-escaped-state)))))

(defstate :script-data-escaped-end-tag-name-state (stream state temporary-buffer current-token)
  (let ((appropriate (and current-token
                          (string-equal (getf current-token :name)
                                        temporary-buffer)))
        (data (html5-stream-char stream)))
    (cond ((and (find data +space-characters+)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :before-attribute-name-state))
          ((and (eql data #\/)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (setf state :self-closing-start-tag-state))
          ((and (eql data #\>)
                appropriate)
           (setf current-token (list :type :end-tag
                                     :name temporary-buffer
                                     :data '()
                                     :self-closing nil))
           (emit-current-token self)
           (setf state :data-state))
          ((ascii-letter-p data)
           (nconcatf temporary-buffer data))
          (t
           (push-token* self :characters "</" temporary-buffer)
           (html5-stream-unget stream data)
           (setf state :script-data-escaped-state)))))

(defstate :script-data-double-escape-start-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((or (find data +space-characters+)
               (find data '(#\/ #\>)))
           (push-token* self :characters data)
           (if (string= (string-downcase temporary-buffer) "script")
               (setf state :script-data-double-escaped-state)
               (setf state :script-data-escaped-state)))
          ((ascii-letter-p data)
           (push-token* self :characters data)
           (nconcatf temporary-buffer (string data)))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-escaped-state)))))

(defstate :script-data-double-escaped-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-double-escaped-dash-state))
          ((eql data #\<)
           (push-token* self :characters "<")
           (setf state :script-data-double-escaped-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-script-in-script))
           (setf state :data-state))
          (t
           (push-token* self :characters data)))))

(defstate :script-data-double-escaped-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-double-escaped-dash-dash-state))
          ((eql data #\<)
           (push-token* self :characters "<")
           (setf state :script-data-double-escaped-less-than-sign-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD)
           (setf state :script-data-double-escaped-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-script-in-script))
           (setf state :data-state))
          (t
           (push-token* self :characters data)
           (setf state :script-data-double-escaped-state)))))

;; FIXME: Incorrectly named in Python code: scriptDataDoubleEscapedDashState (same the one above)
(defstate :script-data-double-escaped-dash-dash-state (stream state)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (push-token* self :characters "-")
           (setf state :script-data-double-escaped-dash-dash-state))
          ((eql data #\<)
           (push-token* self :characters "<")
           (setf state :script-data-double-escaped-less-than-sign-state))
          ((eql data #\>)
           (push-token* self :characters ">")
           (setf state :script-data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (push-token* self :characters #\uFFFD)
           (setf state :script-data-double-escaped-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-script-in-script))
           (setf state :data-state))
          (t
           (push-token* self :characters data)
           (setf state :script-data-double-escaped-state)))))

(defstate :script-data-double-escaped-less-than-sign-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\/)
           (push-token* self :characters "/")
           (setf temporary-buffer (make-growable-string))
           (setf state :script-data-double-escape-end-state))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-double-escaped-state)))))

(defstate :script-data-double-escape-end-state (stream state temporary-buffer)
  (let ((data (html5-stream-char stream)))
    (cond ((or (find data +space-characters+)
               (find data '(#\/ #\>)))
           (push-token* self :characters data)
           (if (string= (string-downcase temporary-buffer) "script")
               (setf state :script-data-escaped-state)
               (setf state :script-data-double-escaped-state)))
          ((ascii-letter-p data)
           (push-token* self :characters data)
           (nconcatf temporary-buffer data))
          (t
           (html5-stream-unget stream data)
           (setf state :script-data-double-escaped-state)))))

(defstate :before-attribute-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (html5-stream-chars-until stream +space-characters+ t))
          ((ascii-letter-p data)
           (add-attribute current-token data)
           (setf state :attribute-name-state))
          ((eql data #\>)
           (emit-current-token self))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((find data '(#\' #\" #\= #\<))
           (push-token self '(:type :parse-error :data :invalid-character-in-attribute-name))
           (add-attribute current-token data)
           (setf state :attribute-name-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-attribute current-token #\uFFFD)
           (setf state :attribute-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-attribute-name-but-got-eof))
           (setf state :data-state))
          (t
           (add-attribute current-token data)
           (setf state :attribute-name-state)))))

(defstate :attribute-name-state (stream state current-token lowercase-attr-name)
  (let ((data (html5-stream-char stream))
        (leaving-this-state t)
        (emit-token nil))
    (cond ((eql data #\=)
           (setf state :before-attribute-value-state))
          ((ascii-letter-p data)
           (add-to-attr-name current-token data
                             (html5-stream-chars-until stream +ascii-letters+ t))
           (setf leaving-this-state nil))
          ((eql data #\>)
           ;; XXX If we emit here the attributes are converted to a dict
           ;; without being checked and when the code below runs we error
           ;; because data is a dict not a list
           (setf emit-token t))
          ((find data +space-characters+)
           (setf state :after-attribute-name-state))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-name current-token #\uFFFD)
           (setf leaving-this-state nil))
          ((find data '(#\' #\" #\<))
           (push-token self '(:type :parse-error :data :invalid-character-in-attribute-name))
           (add-to-attr-name current-token data)
           (setf leaving-this-state nil))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-attribute-name))
           (setf state :data-state))
          (t
           (add-to-attr-name current-token data)
           (setf leaving-this-state nil)))
    (when leaving-this-state
      ;; Attributes are not dropped at this stage. That happens when the
      ;; start tag token is emitted so values can still be safely appended
      ;; to attributes, but we do want to report the parse error in time.
      (when lowercase-attr-name
        (setf (caar (last (getf current-token :data)))
              (ascii-upper-2-lower (caar (last (getf current-token :data))))))
      (loop for (name . value) in (butlast (getf current-token :data)) do
               (when (string= (caar (last (getf current-token :data))) name)
                 (push-token self '(:type :parse-error :data :duplicate-attribute))
                 (return)))
      ;; XXX Fix for above XXX
      (when emit-token
        (emit-current-token self)))))

(defstate :after-attribute-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (html5-stream-chars-until stream +space-characters+ t))
          ((eql data #\=)
           (setf state :before-attribute-value-state))
          ((eql data #\>)
           (emit-current-token self))
          ((ascii-letter-p data)
           (add-attribute current-token data)
           (setf state :attribute-name-state))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-attribute current-token #\uFFFD)
           (setf state :attribute-name-state))
          ((find data '(#\' #\" #\<))
           (push-token self '(:type :parse-error :data :invalid-character-after-attribute-name))
           (add-attribute current-token data)
           (setf state :attribute-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-end-of-tag-but-got-eof))
           (setf state :data-state))
          (t
           (add-attribute current-token data)
           (setf state :attribute-name-state)))))

(defstate :before-attribute-value-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (html5-stream-chars-until stream +space-characters+ t))
          ((eql data #\")
           (setf state :attribute-value-double-quoted-state))
          ((eql data #\&)
           (setf state :attribute-value-un-quoted-state)
           (html5-stream-unget stream data))
          ((eql data #\')
           (setf state :attribute-value-single-quoted-state))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :expected-attribute-value-but-got-right-bracket))
           (emit-current-token self))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-value current-token #\uFFFD)
           (setf state :attribute-value-un-quoted-state))
          ((find data '(#\= #\< #\`))
           (push-token self '(:type :parse-error :data :equals-in-unquoted-attribute-value))
           (add-to-attr-value current-token data)
           (setf state :attribute-value-un-quoted-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-attribute-value-but-got-eof))
           (setf state :data-state))
          (t
           (add-to-attr-value current-token data)
           (setf state :attribute-value-un-quoted-state)))))

(defstate :attribute-value-double-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\")
           (setf state :after-attribute-value-state))
          ((eql data #\&)
           (process-entity-in-attribute self :allowed-char #\"))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-value current-token #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-attribute-value-double-quote))
           (setf state :data-state))
          (t
           (add-to-attr-value current-token
                              data
                              (html5-stream-chars-until stream '(#\" #\&)))))))

(defstate :attribute-value-single-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\')
           (setf state :after-attribute-value-state))
          ((eql data #\&)
           (process-entity-in-attribute self :allowed-char #\'))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-value current-token #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-attribute-value-single-quote))
           (setf state :data-state))
          (t
           (add-to-attr-value current-token
                              data
                              (html5-stream-chars-until stream '(#\' #\&)))))))

(defstate :attribute-value-un-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-attribute-name-state))
          ((eql data #\&)
           (process-entity-in-attribute self :allowed-char #\>))
          ((eql data #\>)
           (emit-current-token self))
          ((find data '(#\" #\' #\= #\< #\`))
           (push-token self '(:type :parse-error :data :unexpected-character-in-unquoted-attribute-value))
           (add-to-attr-value current-token data))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to-attr-value current-token #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-attribute-value-no-quotes))
           (setf state :data-state))
          (t
           (add-to-attr-value current-token
                              data
                              (html5-stream-chars-until stream `(#\& #\> #\" #\' #\= #\< #\`
                                                                     ,@+space-characters+)))))))

(defstate :after-attribute-value-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-attribute-name-state))
          ((eql data #\>)
           (emit-current-token self))
          ((eql data #\/)
           (setf state :self-closing-start-tag-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :unexpected-EOF-after-attribute-value))
           (html5-stream-unget stream data)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-character-after-attribute-value))
           (html5-stream-unget stream data)
           (setf state :before-attribute-name-state)))))

(defstate :self-closing-start-tag-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\>)
           (setf (getf current-token :self-closing) t)
           (emit-current-token self))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :unexpected-EOF-after-solidus-in-tag))
           (html5-stream-unget stream data)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-character-after-soldius-in-tag))
           (html5-stream-unget stream data)
           (setf state :before-attribute-name-state)))))

(defstate :bogus-comment-state (stream state current-token)
  ;; Make a new comment token and give it as value all the characters
  ;; until the first > or EOF (charsUntil checks for EOF automatically)
  ;; and emit it.
  (let ((data (html5-stream-chars-until stream '(#\>))))
    (setf data (substitute #\uFFFD #\u0000 data))
    (push-token* self :comment data)
    ;; Eat the character directly after the bogus comment which is either a
    ;; ">" or an EOF.
    (html5-stream-char stream)
    (setf state :data-state)))

(defstate :markup-declaration-open-state (stream state current-token
                                                 cdata-switch-helper)
  (let ((char-stack (make-array 1
                                :initial-element (html5-stream-char stream)
                                :fill-pointer 1
                                :adjustable t)))
    (cond ((eql (aref char-stack (1- (length char-stack))) #\-)
           (vector-push-extend (html5-stream-char stream) char-stack)
           (when (eql (aref char-stack (1- (length char-stack))) #\-)
             (setf current-token (list :type :comment :data ""))
             (setf state :comment-start-state)
             (return t)))
          ((find (aref char-stack (1- (length char-stack))) '(#\d #\D))
           (let ((matched t))
             (loop for expected in '((#\o #\O) (#\c #\C) (#\t #\T) (#\y #\Y) (#\p #\P) (#\e #\E)) do
                      (vector-push-extend (html5-stream-char stream) char-stack)
                      (unless (find (aref char-stack (1- (length char-stack))) expected)
                        (setf matched nil)
                        (return)))
             (when matched
               (setf current-token (list :type :doctype
                                         :name ""
                                         :public-id nil
                                         :system-id nil
                                         :correct t))
               (setf state :doctype-state)
               (return t))))
          ((and (eql (aref char-stack (1- (length char-stack))) #\[)
                (funcall cdata-switch-helper))
           (let ((matched t))
             (loop for expected across "CDATA[" do
                      (vector-push-extend (html5-stream-char stream) char-stack)
                      (unless (eql (aref char-stack (1- (length char-stack))) expected)
                        (setf matched nil)
                        (return)))
             (when matched
               (setf state :cdata-section-state)
               (return t)))))
    (push-token self '(:type :parse-error :data :expected-dashes-or-doctype))
    (loop while (plusp (length char-stack)) do
             (html5-stream-unget stream (vector-pop char-stack)))
    (setf state :bogus-comment-state)))

(defstate :comment-start-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (setf state :comment-start-dash-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :incorrect-comment))
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data data)
           (setf state :comment-state)))))

(defstate :comment-start-dash-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (setf state :comment-end-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data "-" #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :incorrect-comment))
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data "-" data)
           (setf state :comment-state)))))

(defstate :comment-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (setf state :comment-end-dash-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data data
                   (html5-stream-chars-until stream '(#\- #\u0000)))))))

(defstate :comment-end-dash-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\-)
           (setf state :comment-end-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data "-" #\uFFFD))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment-end-dash))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data "-" data)
           (setf state :comment-state)))))

(defstate :comment-end-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data "--" #\uFFFD)
           (setf state :comment-state))
          ((eql data #\!)
           (push-token self '(:type :parse-error :data :unexpected-bang-after-double-dash-in-comment))
           (setf state :comment-end-bang-state))
          ((eql data #\-)
           (push-token self '(:type :parse-error :data :unexpected-dash-after-double-dash-in-comment))
           (add-to current-token :data data))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment-double-dash))
           (push-token self current-token)
           (setf state :data-state))
          (t
           ;; XXX
           (push-token self '(:type :parse-error :data :unexpected-char-in-comment))
           (add-to current-token :data "--" data)
           (setf state :comment-state)))))

(defstate :comment-end-bang-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\-)
           (add-to current-token :data "--!")
           (setf state :comment-end-dash-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :data "--!" #\uFFFD)
           (setf state :comment-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-comment-end-bang-state))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :data "--!" data)
           (setf state :comment-state)))))

(defstate :doctype-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-doctype-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-doctype-name-but-got-eof))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :need-space-after-doctype))
           (html5-stream-unget stream data)
           (setf state :before-doctype-name-state)))))

(defstate :before-doctype-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)) ; pass
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :expected-doctype-name-but-got-right-bracket))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :name #\uFFFD)
           (setf state :doctype-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :expected-doctype-name-but-got-eof))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (setf (getf current-token :name) (string data))
           (setf state :doctype-name-state)))))

(defstate :doctype-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf (getf current-token :name) (ascii-upper-2-lower (getf current-token :name)))
           (setf state :after-doctype-name-state))
          ((eql data #\>)
           (setf (getf current-token :name) (ascii-upper-2-lower (getf current-token :name)))
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :name #\uFFFD)
           (setf state :doctype-name-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype-name))
           (setf (getf current-token :correct) nil)
           (setf (getf current-token :name) (ascii-upper-2-lower (getf current-token :name)))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :name data)))))

(defstate :after-doctype-name-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)) ; pass
          ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (setf (getf current-token :correct) nil)
           (html5-stream-unget stream data)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (push-token self current-token)
           (setf state :data-state))
          (t
           (cond ((find data '(#\p #\P))
                  (let ((matched t))
                    (loop for expected in '((#\u #\U) (#\b #\B) (#\l #\L) (#\i #\I) (#\c #\C)) do
                             (setf data (html5-stream-char stream))
                             (unless (find data expected)
                               (setf matched nil)
                               (return)))
                    (when matched
                      (setf state :after-doctype-public-keyword-state)
                      (return t))))
                 ((find data '(#\s #\S))
                  (let ((matched t))
                    (loop for expected in '((#\y #\Y) (#\s #\S) (#\t #\T) (#\e #\E) (#\m #\M)) do
                             (setf data (html5-stream-char stream))
                             (unless (find data expected)
                               (setf matched nil)
                               (return)))
                    (when matched
                      (setf state :after-doctype-system-keyword-state)
                      (return t)))))
           ;; All the characters read before the current 'data' will be
           ;; [a-zA-Z], so they're garbage in the bogus doctype and can be
           ;; discarded; only the latest character might be '>' or EOF
           ;; and needs to be ungetted
           (html5-stream-unget stream data)
           (push-token self `(:type :parse-error :data :expected-space-or-right-bracket-in-doctype
                              :datavars (:data ,data)))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :after-doctype-public-keyword-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-doctype-public-identifier-state))
          ((find data '(#\' #\"))
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (html5-stream-unget stream data)
           (setf state :before-doctype-public-identifier-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (html5-stream-unget stream data)
           (setf state :before-doctype-public-identifier-state)))))

(defstate :before-doctype-public-identifier-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)) ; pass
          ((eql data #\")
           (setf (getf current-token :public-id) "")
           (setf state :doctype-public-identifier-double-quoted-state))
          ((eql data #\')
           (setf (getf current-token :public-id) "")
           (setf state :doctype-public-identifier-single-quoted-state))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :doctype-public-identifier-double-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\")
           (setf state :after-doctype-public-identifier-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :public-id #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :public-id data)))))

(defstate :doctype-public-identifier-single-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\')
           (setf state :after-doctype-public-identifier-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :public-id #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :public-id data)))))

(defstate :after-doctype-public-identifier-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :between-doctype-public-and-system-identifiers-state))
          ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\")
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-double-quoted-state))
          ((eql data #\')
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-single-quoted-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :between-doctype-public-and-system-identifiers-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)) ; pass
          ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data #\")
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-double-quoted-state))
          ((eql data #\')
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-single-quoted-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :after-doctype-system-keyword-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)
           (setf state :before-doctype-system-identifier-state))
          ((find data '(#\' #\"))
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (html5-stream-unget stream data)
           (setf state :before-doctype-system-identifier-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (html5-stream-unget stream data)
           (setf state :before-doctype-system-identifier-state)))))

(defstate :before-doctype-system-identifier-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)) ; pass
          ((eql data #\")
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-double-quoted-state))
          ((eql data #\')
           (setf (getf current-token :system-id) "")
           (setf state :doctype-system-identifier-single-quoted-state))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf (getf current-token :correct) nil)
           (setf state :bogus-doctype-state)))))

(defstate :doctype-system-identifier-double-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\")
           (setf state :after-doctype-system-identifier-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :system-id #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :system-id data)))))

(defstate :doctype-system-identifier-single-quoted-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\')
           (setf state :after-doctype-system-identifier-state))
          ((eql data #\u0000)
           (push-token self '(:type :parse-error :data :invalid-codepoint))
           (add-to current-token :system-id #\uFFFD))
          ((eql data #\>)
           (push-token self '(:type :parse-error :data :unexpected-end-of-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (add-to current-token :system-id data)))))

(defstate :after-doctype-system-identifier-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((find data +space-characters+)) ; pass
          ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           (push-token self '(:type :parse-error :data :eof-in-doctype))
           (setf (getf current-token :correct) nil)
           (push-token self current-token)
           (setf state :data-state))
          (t
           (push-token self '(:type :parse-error :data :unexpected-char-in-doctype))
           (setf state :bogus-doctype-state)))))

(defstate :bogus-doctype-state (stream state current-token)
  (let ((data (html5-stream-char stream)))
    (cond ((eql data #\>)
           (push-token self current-token)
           (setf state :data-state))
          ((eql data +eof+)
           ;; XXX EMIT
           (html5-stream-unget stream data)
           (push-token self current-token)
           (setf state :data-state))
          (t)))) ; pass

(defstate :cdata-section-state (stream state current-token)
  (let ((data '()))
    (loop
      (push (html5-stream-chars-until stream '(#\])) data)
      (let ((char-stack '())
            (matched t))
        (loop for expected across "]]>" do
                 (push (html5-stream-char stream) char-stack)
                 (cond ((eql (car char-stack) +eof+)
                        (pop char-stack)
                        (setf data (append char-stack data))
                        (return))
                       ((not (eql (car char-stack) expected))
                        (setf matched nil)
                        (setf data (append char-stack data))
                        (return))))
        (when matched
          (return))))
    (setf data (apply #'concatenate 'string (mapcar #'string (nreverse data))))
    ;; Deal with null here rather than in the parser
    (let ((null-count (count #\u0000 data)))
      (when (plusp null-count)
        (push-token self '(:type :parse-error :data :invalid-codepoint))
        (setf data (nsubstitute #\uFFFD #\u0000 data))))
    (when (plusp (length data))
      (push-token* self :characters data))
    (setf state :data-state)))

;;; simple-tree
;; A basic implementation of a DOM-core like thing
(defclass node ()
  ((type :initform :node :allocation :class :reader node-type)
   (name :initarg :name :initform nil :reader node-name)
   (namespace :initarg :namespace :initform nil :reader node-namespace)
   (parent :initform nil :reader node-parent)
   (value :initform nil :initarg :value
          :accessor node-value)
   (child-nodes :initform nil :accessor %node-child-nodes)
   (last-child :initform nil :accessor last-child)))

(defmethod (setf %node-child-nodes) :after (value (node node))
  (setf (last-child node) (last value)))

(defclass document (node)
  ((type :initform :document :allocation :class)))

(defclass document-fragment (document)
  ((type :initform :document-fragment :allocation :class)))

(defclass document-type (node)
  ((type :initform :document-type :allocation :class)
   (public-id :initarg :public-id :reader node-public-id)
   (system-id :initarg :system-id :reader node-system-id)))

(defclass text-node (node)
  ((type :initform :text :allocation :class)))

(defclass element (node)
  ((type :initform :element :allocation :class)
   (attributes :initform nil :accessor %node-attributes)))

(defclass comment-node (node)
  ((type :initform :comment :allocation :class)))

;;; Creating nodes
(defun make-document ()
  (make-instance 'document))

(defun make-fragment (document)
  (declare (ignore document))
  (make-instance 'document-fragment))

(defun make-doctype (document name public-id system-id)
  (declare (ignore document))
  (make-instance 'document-type :name name :public-id public-id :system-id system-id))

(defun make-comment (document data)
  (declare (ignore document))
  (make-instance 'comment-node :value data))

(defun make-element (document name namespace)
  (declare (ignore document))
  (make-instance 'element :name name :namespace namespace))

(defun make-text-node (document data)
  (declare (ignore document))
  (make-instance 'text-node :value data))

;;; Node methods
(defun node-first-child (node)
  (car (%node-child-nodes node)))

(defun node-last-child (node)
  (car (last-child node)))

(defun node-previous-sibling (node)
  (loop for (this next) on (%node-child-nodes (node-parent node))
        when (eql next node) do (return this)))

(defun node-next-sibling (node)
  (loop for (this next) on (%node-child-nodes (node-parent node))
        when (eql this node) do (return next)))

(defun node-remove-child (node child)
  (setf (%node-child-nodes node)
        (remove child (%node-child-nodes node)))
  (setf (slot-value child 'parent) nil))

(defun node-append-child (node child)
  (when (node-parent child)
    (node-remove-child (node-parent child) child))
  (setf (slot-value child 'parent) node)
  (if (%node-child-nodes node)
      (setf (last-child node)
            (push child (cdr (last-child node))))
      (setf (%node-child-nodes node)
            (list child)))
  (%node-child-nodes node))

(defun node-insert-before (node child insert-before)
  (let ((child-nodes (%node-child-nodes node)))
    (setf (slot-value child 'parent) node)
    (labels ((insert-before (child-nodes)
               (cond ((endp child-nodes)
                      (cons child nil))
                     ((eql (car child-nodes) insert-before)
                      (cons child child-nodes))
                     (t (rplacd child-nodes (insert-before (cdr child-nodes)))))))
      (setf (%node-child-nodes node)
            (insert-before child-nodes)))))

(defun element-attribute (node attribute &optional namespace)
  (cdr (assoc (cons attribute namespace)
              (%node-attributes node)
              :test #'equal)))

(defun (setf element-attribute) (new-value node attribute
                                 &optional namespace)
  (check-type attribute string)
  (check-type new-value string)
  (let ((old-attr (assoc (cons attribute namespace)
                         (%node-attributes node)
                         :test #'equal)))
    (if old-attr
        (setf (cdr old-attr) new-value)
        (push (cons (cons attribute namespace) new-value) (%node-attributes node)))))

;;; Traversing
(defun element-map-children (function node)
  (map nil function (%node-child-nodes node)))

(defun element-map-attributes* (function node)
  (loop for ((name . namespace) . value) in (%node-attributes node)
        do (funcall function name namespace value)))

(defun element-map-attributes (function node)
  (element-map-attributes*
   (lambda (name namespace value)
     (funcall function
              (if namespace
                  (format nil "~A:~A" (find-prefix namespace) name)
                  name)
              namespace
              value))
   node))

;; Printing for the ease of debugging
(defun node-count (tree)
  (typecase tree
    (element (1+ (apply #'+ (mapcar #'node-count (%node-child-nodes tree)))))
    ((or document document-fragment)
     (apply #'+ (mapcar #'node-count (%node-child-nodes tree))))
    (t 1)))

(defmethod print-object ((node document) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "nodes: ~A" (node-count node))))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A" (node-name node))))

(defmethod print-object ((node text-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (write (node-value node) :stream stream :length 30)))

;;; html5-parser-class
(defvar *parser*)

(defclass html-parser ()
  ((html-namespace :initform (find-namespace "html"))
   (strict :initarg :strict)
   (inner-html-mode)
   (container :initform "div")
   (tokenizer)
   (document :initform (make-document))
   (errors :initform '())
   (phase :accessor parser-phase)
   first-start-tag
   compat-mode
   inner-html
   last-phase
   original-phase
   before-rcdata-phase
   (character-tokens :initform nil)
   frameset-ok
   open-elements
   active-formatting-elements
   head-pointer
   form-pointer
   insert-from-table
   (in-body-process-space-characters-mode :initform :non-pre)))

;;; tree-help
(defmacro pop-end (place)
  "Pop from the end of list"
  (let ((old-list (gensym)))
    `(let ((,old-list ,place))
       (prog1 (car (last ,old-list))
         (setf ,place (butlast ,old-list))))))

(defmacro push-end (object place)
  "Push to the end of list"
  `(progn
     ;; (log:trace! "~&push ~S to ~S" ',object ',place)
     (setf ,place (append ,place (list ,object)))))

(defun document* ()
  (slot-value *parser* 'document))

(defun node-clone* (node)
  (ecase (node-type node)
    (:document
     (make-document))
    (:document-fragment
     (make-fragment (document*)))
    (:document-type
     (make-doctype (document*)
                   (node-name node)
                   (node-public-id node)
                   (node-system-id node)))
    (:comment
     (make-comment (document*) (node-value node)))
    (:text
     (make-text-node (document*) (node-value node)))
    (:element
     (let ((clone (make-element (document*) (node-name node) (node-namespace node))))
       (element-map-attributes*
        (lambda (name namespace value)
          (setf (element-attribute clone name namespace) value))
        node)
       clone))))

(defun node-name-tuple (node)
  (cons (or (node-namespace node)
            (find-namespace "html"))
        (node-name node)))

(defun node-name-tuple-values (node)
  (values (or (node-namespace node)
              (find-namespace "html"))
          (node-name node)))

(defun node-has-content (node)
  (not (null (node-first-child node))))

(defun node-attributes= (node1 node2)
  (labels ((has-all-attributes-of (node1 node2)
             (element-map-attributes*
              (lambda (name namespace value)
                (unless (equal value
                               (element-attribute node2 name namespace))
                  (return-from has-all-attributes-of nil)))
              node1)
             t))
    (and (has-all-attributes-of node1 node2)
         (has-all-attributes-of node2 node1))))

(defun node-append-child* (node child)
  (let ((last-child (node-last-child node)))
    (if (and (eql :text (node-type child))
             last-child
             (eql :text (node-type last-child)))
        (nconcatf (node-value last-child)
                  (node-value child))
        (node-append-child node child))))

(defun node-insert-before* (node child insert-before)
  (when (eql :text (node-type child))
    (let ((prev-child (node-previous-sibling insert-before)))
      (when (and prev-child
                 (eql :text (node-type prev-child)))
        (node-remove-child node prev-child)
        (setf child (make-text-node
                     (document*)
                     (concatenate 'string
                                  (node-value prev-child)
                                  (node-value child)))))))
  (node-insert-before node child insert-before))

(defun node-reparent-children (node new-parent)
  (element-map-children (lambda (child)
                          (node-append-child new-parent child))
                        node))

(defun node-insert-text (node data &optional insert-before)
  (if insert-before
      (node-insert-before* node (make-text-node (document*) data) insert-before)
      (node-append-child* node (make-text-node (document*) data))))

(defun last-open-element ()
  (with-slots (open-elements) *parser*
    (car (last open-elements))))

(defun create-element (token)
  "Create an element but don't insert it anywhere"
  (with-slots (html-namespace) *parser*
    (let ((element (make-element (document*)
                                 (getf token :name)
                                 (or (getf token :namespace)
                                     html-namespace))))
      (loop for (name . value) in (getf token :data)
            do (if (consp name)
                   (setf (element-attribute element (second name) (third name)) value)
                   (setf (element-attribute element name) value)))
      element)))


(defun insert-root (token)
  (with-slots (open-elements) *parser*
    (let ((element (create-element token)))
      (assert element)
      (push-end element open-elements)
      (node-append-child (document*) element))))

(defun insert-doctype (token)
  (node-append-child (document*)
                     (make-doctype (document*)
                                   (getf token :name)
                                   (getf token :public-id)
                                   (getf token :system-id))))

(defun insert-comment (token &optional parent)
  (with-slots (open-elements) *parser*
    (unless parent
      (setf parent (car (last open-elements))))
    (node-append-child parent (make-comment (document*) (getf token :data)))))

(defun insert-element-normal (token)
  (with-slots (open-elements) *parser*
    (let ((element (create-element token)))
      (node-append-child (last-open-element) element)
      (push-end element open-elements)
      element)))

(defun insert-element-table (token)
  (with-slots (open-elements) *parser*
    (if (not (member (node-name (last-open-element))
                     +table-insert-mode-elements+ :test #'string=))
        (insert-element-normal token)
        (let ((element (create-element token)))
          ;; We should be in the InTable mode. This means we want to do
          ;; special magic element rearranging
          (multiple-value-bind (parent insert-before)
              (get-table-misnested-nodeposition)
            (if (not insert-before)
                (node-append-child* parent element)
                (node-insert-before* parent element insert-before))
            (push-end element open-elements))
          element))))

(defun insert-element (token)
  (with-slots (insert-from-table) *parser*
    (if insert-from-table
        (insert-element-table token)
        (insert-element-normal token))))

(defun parser-insert-text (data &optional parent)
  "Insert text data."
  (with-slots (open-elements insert-from-table) *parser*
    (unless parent
      (setf parent (car (last open-elements))))
    (cond ((or (not insert-from-table)
               (and insert-from-table
                    (not (member (node-name (last-open-element))
                                 +table-insert-mode-elements+ :test #'string=))))
           (node-insert-text parent data))
          (t
           ;; We should be in the InTable mode. This means we want to do
           ;; special magic element rearranging
           (multiple-value-bind (parent insert-before)
               (get-table-misnested-nodeposition)
             (node-insert-text parent data insert-before))))))

(defun get-table-misnested-nodeposition ()
  "Get the foster parent element, and sibling to insert before
    (or None) when inserting a misnested table node"
  (with-slots (open-elements) *parser*
    ;; The foster parent element is the one which comes before the most
    ;; recently opened table element
    (let ((last-table (find "table" open-elements :key #'node-name :test #'string= :from-end t))
          (foster-parent nil)
          (insert-before nil))
      (cond (last-table
             ;; XXX - we should really check that this parent is actually a
             ;; node here
             (if (node-parent last-table)
                 (setf foster-parent (node-parent last-table)
                       insert-before last-table)
                 (setf foster-parent (elt open-elements (1- (position last-table open-elements))))))
            (t
             (setf foster-parent (first open-elements))))
      (values foster-parent insert-before))))

(defun generate-implied-end-tags (&optional exclude)
  (with-slots (open-elements) *parser*
    (let ((name (node-name (last-open-element))))
      ;; XXX td, th and tr are not actually needed
      (when (and (member name '("dd" "dt" "li" "option" "optgroup" "p" "rp" "rt") :test #'string=)
                 (not (equal name exclude)))
        (pop-end open-elements)
        ;; XXX This is not entirely what the specification says. We should
        ;; investigate it more closely.
        (generate-implied-end-tags exclude)))))

(defun reconstruct-active-formatting-elements ()
  ;; Within this algorithm the order of steps described in the
  ;; specification is not quite the same as the order of steps in the
  ;; code. It should still do the same though.
  (with-slots (active-formatting-elements open-elements) *parser*
    ;; Step 1: stop the algorithm when there's nothing to do.
    (unless active-formatting-elements
      (return-from reconstruct-active-formatting-elements))
    ;; Step 2 and step 3: we start with the last element. So i is -1.
    (let* ((i (1- (length active-formatting-elements)))
           (entry (elt active-formatting-elements i)))
      (when (or (eql entry :marker)
                (member entry open-elements))
        (return-from reconstruct-active-formatting-elements))
      ;; Step 6
      (loop while (and (not (eql entry :marker))
                       (not (member entry open-elements))) do
               (when (zerop i)
                 ;; This will be reset to 0 below
                 (setf i -1)
                 (return))
               (decf i)
               ;; Step 5: let entry be one earlier in the list.
               (setf entry (elt active-formatting-elements i)))

      (loop
        ;; Step 7
        (incf i)
        ;; Step 8
        (setf entry (elt active-formatting-elements i))
        ;; Step 9
        (let* ((element (insert-element (list :type :start-tag
                                              :name (node-name entry)
                                              :namespace (node-namespace entry)))))
          (element-map-attributes* (lambda (name namespace value)
                                     (setf (element-attribute element name namespace) value))
                                   entry)
          ;; Step 10
          (setf (elt active-formatting-elements i) element)
          ;; Step 11
          (when (eql element (car (last active-formatting-elements)))
            (return)))))))

(defun clear-active-formatting-elements ()
  (with-slots (active-formatting-elements) *parser*
    (loop for entry = (pop-end active-formatting-elements)
          while (and active-formatting-elements
                     (not (eql entry :marker))))))

(defun element-in-active-formatting-elements (name)
  "Check if an element exists between the end of the active
   formatting elements and the last marker. If it does, return it, else
   return false"
  (with-slots (active-formatting-elements) *parser*
    (loop for item in (reverse active-formatting-elements) do
          ;; Check for Marker first because if it's a Marker it doesn't have a
          ;; name attribute.
             (when (eql item :marker)
               (return nil))
             (when (string= (node-name item) name)
               (return item)))))

(defun scope-tree ()
  (load-time-value
   (flet ((unflatten (alist)
            "Turn an alist into a tree."
            (let ((alist2
                    (mapcar #'list
                            (remove-duplicates (mapcar #'car alist)
                                               :test #'equal))))
              (loop for (key . value) in alist
                    do (push value (cdr (assoc key alist2
                                               :test #'equal))))
              ;; Put the XHTML ns first.
              (sort alist2 #'<
                    :key (lambda (pair)
                           (position (car pair)
                                     '("http://www.w3.org/1999/xhtml"
                                       "http://www.w3.org/2000/svg"
                                       "http://www.w3.org/1998/Math/MathML")
                                     :test #'string=))))))
     (let ((html (find-namespace "html")))
       `((nil . ,(unflatten +scoping-elements+))
         ("button" . ,(unflatten
                       `(,@+scoping-elements+
                         (,html . "button"))))
         ("list" . ,(unflatten
                     `(,@+scoping-elements+
                       (,html . "ol")
                       (,html . "ul"))))
         ("table" . ((,html "html" "table")))
         ("select" . ((,html "optgroup" "option"))))))))

(defun element-in-scope (target &optional variant)
  (let ((list-elements
          (cdr (assoc variant (scope-tree) :test #'equal)))
        (invert (equal "select" variant)))
    (dolist (node (reverse (slot-value *parser* 'open-elements)))
      (when (or (and (stringp target)
                     (string= (node-name node) target))
                (eql node target))
        (return-from element-in-scope t))

      (multiple-value-bind (ns name)
          (node-name-tuple-values node)
        (let ((found (member name (cdr (assoc ns list-elements :test #'string=))
                             :test #'string=)))
          (when invert
            (setf found (not found)))
          (when found
            (return-from element-in-scope nil)))))
    (error "We should never reach this point")))

;;; Parser
;; external interface
(defun parse-html5 (source &key encoding strictp container dom)
  (parse-html5-from-source source
                           :encoding encoding
                           :strictp strictp
                           :container container
                           :dom dom))

(defun parse-html5-fragment (source &key encoding strictp (container "div") dom)
  (parse-html5-from-source source
                           :encoding encoding
                           :strictp strictp
                           :container container
                           :dom dom))

(defgeneric transform-html5-dom (to-type node &key)
  (:method ((to-type cons) node &key)
    (apply #'transform-html5-dom (car to-type) node (cdr to-type)))
  (:method (to-type node &key &allow-other-keys)
    (error "No TRANSFORM-HTML5-DOM method defined for dom type ~S." to-type)))

;; internal
(defun parse-html5-from-source (source &key container encoding strictp dom)
  (let ((*parser* (make-instance 'html-parser
                    :strict strictp)))
    (parser-parse source
                  :fragment-p container
                  :encoding encoding)
    (with-slots (open-elements errors) *parser*
      (let ((document
              (if container
                  (let ((fragment (make-fragment (document*))))
                    (node-reparent-children (first open-elements) fragment)
                    fragment)
                  (document*))))
        (values (if dom
                    (transform-html5-dom dom document)
                    document)
                (reverse errors))))))

(defvar *phase*)

(defun cdata-switch-helper ()
  (and (last-open-element)
       (not (equal (node-namespace (last-open-element))
                   (slot-value *parser* 'html-namespace)))))

(defun parser-parse (source &key fragment-p encoding)
  (with-slots (inner-html-mode container tokenizer)
      *parser*
    (setf inner-html-mode fragment-p)
    (when (stringp fragment-p)
      (setf container fragment-p))
    (setf tokenizer (make-html-tokenizer source
                                         :encoding encoding

                                         :cdata-switch-helper #'cdata-switch-helper))
    (parser-reset)
    (loop
      ;; The input stream will throw please-reparse with result true
      ;; if the encoding is changed
      while (catch 'please-reparse
              (main-loop)
              nil)
      do (parser-reset))))

(defun parser-reset ()
  (with-slots (open-elements active-formatting-elements
               head-pointer form-pointer insert-from-table
               first-start-tag errors compat-mode inner-html-mode
               inner-html container tokenizer phase last-phase
               before-rcdata-phase frameset-ok
               html-namespace)
      *parser*
    (setf open-elements '())
    (setf active-formatting-elements '())
    (setf head-pointer nil)
    (setf form-pointer nil)
    (setf insert-from-table nil)
    (setf first-start-tag nil)
    (setf errors '())
    (setf compat-mode :no-quirks)
    (cond (inner-html-mode
           (setf inner-html (string-downcase container))
           (cond ((member inner-html +cdata-elements+ :test #'string=)
                  (setf (slot-value tokenizer 'state) :rcdata-state))
                 ((member inner-html +rcdata-elements+ :test #'string=)
                  (setf (slot-value tokenizer 'state) :rawtext-state))
                 ((string= inner-html "plaintext")
                  (setf (slot-value tokenizer 'state) :plaintext-state)))
           (insert-root (implied-tag-token "html" :start-tag))
           (setf phase :before-head)
           (reset-insertion-mode))
          (t
           (setf inner-html nil)
           (setf phase :initial)))

    (setf last-phase nil)
    (setf before-rcdata-phase nil)
    (setf frameset-ok t)))

(defun is-html-integration-point (element)
  (if (and (string= (node-name element) "annotation-xml")
           (string= (node-namespace element) (find-namespace "mathml")))
      (and (element-attribute element "encoding")
           (member (ascii-upper-2-lower (element-attribute element "encoding"))
                   '("text/html" "application/xhtml+xml")
                   :test #'string=))
      (member (node-name-tuple element)
              +html-integration-point-elements+
              :test #'equal)))

(defun is-math-ml-text-integration-point (element)
  (member (node-name-tuple element)
          +mathml-text-integration-point-elements+
          :test #'equal))

(defun main-loop ()
  (with-slots (tokenizer phase)
      *parser*
    (map-tokens tokenizer (lambda (token)
                            (process-token (normalize-token token))))
    (loop with reprocess = t
          with phases = '()
          while reprocess do
             (push phase phases)
             (setf reprocess (process-eof nil :phase phase))
             (when reprocess
               (assert (not (member phase phases)))))))

(defun process-token (token)
  (with-slots (tokenizer last-open-element html-namespace)
      *parser*
    (let ((new-token token)
          (type))
      (loop while new-token do
               (let* ((current-node (last-open-element))
                      (current-node-namespace (if current-node (node-namespace current-node)))
                      (current-node-name (if current-node (node-name current-node))))

                 (setf type (getf new-token :type))

                 (cond ((eql type :parse-error)
                        (parser-parse-error (getf token :data) (getf token :datavars))
                        (setf new-token nil))
                       (t
                        (let (phase)
                          (if (or (null (slot-value *parser* 'open-elements))
                                  (equal current-node-namespace html-namespace)
                                  (and (is-math-ml-text-integration-point current-node)
                                       (or (and (eql type :start-tag)
                                                (not (member (getf token :name) '("mglyph" "malignmark") :test #'string=)))
                                           (eql type :characters)
                                           (eql type :space-characters)))
                                  (and (equal current-node-namespace (find-namespace "mathml"))
                                       (equal current-node-name "annotation-xml")
                                       (eql type :start-tag)
                                       (equal (getf token :name) "svg"))
                                  (and (is-html-integration-point current-node)
                                       (member type '(:start-tag :characters :space-characters))))
                              (setf phase (slot-value *parser* 'phase))
                              (setf phase :in-foreign-content))
                                        ;(format t "~&phase ~S token ~S~%" phase new-token)
                          (setf new-token
                                (ecase type
                                  (:characters
                                   (process-characters new-token :phase phase))
                                  (:space-characters
                                   (process-space-characters new-token :phase phase))
                                  (:start-tag
                                   (process-start-tag new-token :phase phase))
                                  (:end-tag
                                   (process-end-tag new-token :phase phase))
                                  (:comment
                                   (process-comment new-token :phase phase))
                                  (:doctype
                                   (process-doctype new-token :phase phase))))
                                        ;(format t "   phase returned ~S new-token ~S~%" phase new-token)
                          ))))
               (when (and (eql type :start-tag)
                          (getf token :self-closing)
                          (not (getf token :self-closing-acknowledged)))
                 (parser-parse-error :non-void-element-with-trailing-solidus
                                     `(:name ,(getf token :name))))))))

(defun parser-parse-error (error-code &optional datavars)
  (with-slots (errors) *parser*
    (push (list error-code datavars) errors)))

;; TODO rename to a longer and more descriptive name when we are done writing the code
(defun perror (error-code &rest datavars)
  (parser-parse-error error-code datavars))

(defun normalize-token (token)
  (when (getf token :start-tag)
    ;; Remove duplicate attributes
    (setf (getf token :data) (remove-duplicates (getf token :data)
                                                :key #'car
                                                :test #'string=
                                                :from-end t)))
  token)

(defun adjust-attributes (token replacements)
  (setf (getf token :data)
        (loop for (name . value) in (getf token :data)
              collect (cons (or (cdr (assoc name replacements :test #'string=))
                                name)
                            value))))

(defun adjust-math-ml-attributes (token)
  (adjust-attributes token '(("definitionurl" ."definitionURL"))))

(defun adjust-svg-attributes (token)
  (adjust-attributes token '(("attributename" . "attributeName")
                             ("attributetype" . "attributeType")
                             ("basefrequency" . "baseFrequency")
                             ("baseprofile" . "baseProfile")
                             ("calcmode" . "calcMode")
                             ("clippathunits" . "clipPathUnits")
                             ("contentscripttype" . "contentScriptType")
                             ("contentstyletype" . "contentStyleType")
                             ("diffuseconstant" . "diffuseConstant")
                             ("edgemode" . "edgeMode")
                             ("externalresourcesrequired" . "externalResourcesRequired")
                             ("filterres" . "filterRes")
                             ("filterunits" . "filterUnits")
                             ("glyphref" . "glyphRef")
                             ("gradienttransform" . "gradientTransform")
                             ("gradientunits" . "gradientUnits")
                             ("kernelmatrix" . "kernelMatrix")
                             ("kernelunitlength" . "kernelUnitLength")
                             ("keypoints" . "keyPoints")
                             ("keysplines" . "keySplines")
                             ("keytimes" . "keyTimes")
                             ("lengthadjust" . "lengthAdjust")
                             ("limitingconeangle" . "limitingConeAngle")
                             ("markerheight" . "markerHeight")
                             ("markerunits" . "markerUnits")
                             ("markerwidth" . "markerWidth")
                             ("maskcontentunits" . "maskContentUnits")
                             ("maskunits" . "maskUnits")
                             ("numoctaves" . "numOctaves")
                             ("pathlength" . "pathLength")
                             ("patterncontentunits" . "patternContentUnits")
                             ("patterntransform" . "patternTransform")
                             ("patternunits" . "patternUnits")
                             ("pointsatx" . "pointsAtX")
                             ("pointsaty" . "pointsAtY")
                             ("pointsatz" . "pointsAtZ")
                             ("preservealpha" . "preserveAlpha")
                             ("preserveaspectratio" . "preserveAspectRatio")
                             ("primitiveunits" . "primitiveUnits")
                             ("refx" . "refX")
                             ("refy" . "refY")
                             ("repeatcount" . "repeatCount")
                             ("repeatdur" . "repeatDur")
                             ("requiredextensions" . "requiredExtensions")
                             ("requiredfeatures" . "requiredFeatures")
                             ("specularconstant" . "specularConstant")
                             ("specularexponent" . "specularExponent")
                             ("spreadmethod" . "spreadMethod")
                             ("startoffset" . "startOffset")
                             ("stddeviation" . "stdDeviation")
                             ("stitchtiles" . "stitchTiles")
                             ("surfacescale" . "surfaceScale")
                             ("systemlanguage" . "systemLanguage")
                             ("tablevalues" . "tableValues")
                             ("targetx" . "targetX")
                             ("targety" . "targetY")
                             ("textlength" . "textLength")
                             ("viewbox" . "viewBox")
                             ("viewtarget" . "viewTarget")
                             ("xchannelselector" . "xChannelSelector")
                             ("ychannelselector" . "yChannelSelector")
                             ("zoomandpan" . "zoomAndPan"))))

(defun adjust-foreign-attributes (token)
  (adjust-attributes token `(("xlink:actuate" . ("xlink" "actuate" ,(find-namespace "xlink")))
                             ("xlink:arcrole" . ("xlink" "arcrole" ,(find-namespace "xlink")))
                             ("xlink:href" . ("xlink" "href" ,(find-namespace "xlink")))
                             ("xlink:role" . ("xlink" "role" ,(find-namespace "xlink")))
                             ("xlink:show" . ("xlink" "show" ,(find-namespace "xlink")))
                             ("xlink:title" . ("xlink" "title" ,(find-namespace "xlink")))
                             ("xlink:type" . ("xlink" "type" ,(find-namespace "xlink")))
                             ("xml:base" . ("xml" "base" ,(find-namespace "xml")))
                             ("xml:lang" . ("xml" "lang" ,(find-namespace "xml")))
                             ("xml:space" . ("xml" "space" ,(find-namespace "xml")))
                             ("xmlns" . (nil "xmlns" ,(find-namespace "xmlns")))
                             ("xmlns:xlink" . ("xmlns" "xlink" ,(find-namespace "xmlns"))))))

(defun reset-insertion-mode ()
  (with-slots (inner-html html-namespace phase open-elements) *parser*
    (let ((last nil)
          (new-phase nil)
          (new-modes '(("select" . :in-select)
                       ("td" . :in-cell)
                       ("th" . :in-cell)
                       ("tr" . :in-row)
                       ("tbody" . :in-table-body)
                       ("thead" . :in-table-body)
                       ("tfoot" . :in-table-body)
                       ("caption" . :in-caption)
                       ("colgroup" . :in-column-group)
                       ("table" . :in-table)
                       ("head" . :in-body)
                       ("body" . :in-body)
                       ("frameset" . :in-frameset)
                       ("html" . :before-head))))
      (loop for node in (reverse open-elements)
            for node-name = (node-name node)
            do
               (when (eql node (first open-elements))
                 (assert inner-html)
                 (setf last t)
                 (setf node-name inner-html))
               ;; Check for conditions that should only happen in the innerHTML
               ;; case
               (when (member node-name '("select" "colgroup" "head" "html") :test #'string=)
                 (assert inner-html))

               (unless (and (not last)
                            (string/= (node-namespace node) html-namespace))
                 (let ((match (cdr (assoc node-name new-modes :test #'string=))))
                   (when match
                     (setf new-phase match)
                     (return))
                   (when last
                     (setf new-phase :in-body)
                     (return)))))
      (setf phase new-phase))))

(defun parse-rc-data-raw-text (token content-type)
  (assert (member content-type '(:rawtext :rcdata)))
  (with-slots (tokenizer original-phase phase) *parser*
    (insert-element token)
    (setf (tokenizer-state tokenizer) (ecase content-type
                                        (:rawtext :rawtext-state)
                                        (:rcdata :rcdata-state)))
    (setf original-phase phase)
    (setf phase :text)
    nil))

;; Phases
(defun implied-tag-token (name &optional (type :end-tag))
  (list :type type :name name :data '() :self-closing nil))

(defun implied-tag-token/full (name type
                               &key (attributes '()) (self-closing nil))
  (list :type type :name name :data attributes :self-closing self-closing))

(eval-when (:compile-toplevel :execute)
  (defun phase-process-method-name (function-name)
    (intern (concatenate 'string
                         "%"
                         (symbol-name function-name))
            (symbol-package function-name))))

(defvar *phase-indent* 0)

(defun call-phase-method (name phase token)
                                        ;(format *trace-output* "~&~vTcall: ~S ~S ~S" *phase-indent* name phase token)
                                        ;(break)
  (let ((result (let ((*phase-indent* (+ 4 *phase-indent*)))
                  (funcall name phase token))))
                                        ;(format *trace-output* "~&~vTreturn: ~S ~S" *phase-indent* name result)
    result))

(defmacro define-phase-process-functions (&body defs)
  `(progn
     ,@(loop for function-name in defs
             for method-name = (phase-process-method-name function-name)
             collect `(defgeneric ,method-name (phase token))
             collect `(defun ,function-name (token &key (phase *phase*))
                        (call-phase-method #',method-name phase token)))))

(define-phase-process-functions
  add-formatting-element
  end-tag-applet-marquee-object
  end-tag-block
  end-tag-body
  end-tag-br
  end-tag-caption
  end-tag-col
  end-tag-colgroup
  end-tag-form
  end-tag-formatting
  end-tag-frameset
  end-tag-head
  end-tag-heading
  end-tag-html
  end-tag-html-body-br
  end-tag-ignore
  end-tag-imply
  end-tag-imply-head
  end-tag-list-item
  end-tag-optgroup
  end-tag-option
  end-tag-other
  end-tag-p
  end-tag-script
  end-tag-select
  end-tag-table
  end-tag-table-cell
  end-tag-table-row-group
  end-tag-tr
  insert-text
  process-characters
  process-comment
  process-doctype
  process-end-tag
  process-eof
  process-space-characters
  process-start-tag
  start-tag-a
  start-tag-applet-marquee-object
  start-tag-base-link-command
  start-tag-body
  start-tag-button
  start-tag-caption
  start-tag-close-p
  start-tag-col
  start-tag-colgroup
  start-tag-form
  start-tag-formatting
  start-tag-frame
  start-tag-frameset
  start-tag-from-head
  start-tag-head
  start-tag-heading
  start-tag-hr
  start-tag-html
  start-tag-i-frame
  start-tag-image
  start-tag-imply-tbody
  start-tag-input
  start-tag-is-index
  start-tag-list-item
  start-tag-math
  start-tag-meta
  start-tag-misplaced
  start-tag-no-script-no-frames-style
  start-tag-nobr
  start-tag-noframes
  start-tag-opt
  start-tag-optgroup
  start-tag-option
  start-tag-other
  start-tag-param-source
  start-tag-plaintext
  start-tag-pre-listing
  start-tag-process-in-head
  start-tag-rawtext
  start-tag-row-group
  start-tag-rp-rt
  start-tag-script
  start-tag-select
  start-tag-style-script
  start-tag-svg
  start-tag-table
  start-tag-table-cell
  start-tag-table-element
  start-tag-table-other
  start-tag-textarea
  start-tag-title
  start-tag-tr
  start-tag-void-formatting
  start-tag-xmp)

(defmacro def (phase name (&rest slots) &body body)
  `(defmethod ,(phase-process-method-name name) ((*phase* (eql ,phase)) token)
     (with-slots (,@slots) *parser*
       ,@body)))

(defmacro tagname-dispatch (phase name &body cases)
  `(def ,phase ,name ()
     (let ((tagname (getf token :name)))
       (declare (ignorable tagname))
       ,(let* ((default '(error "Unhandled tag ~S" tagname))
               (string-cases
                 (loop for (tagnames function) in cases
                       append (cond ((stringp tagnames)
                                     `((,tagnames (,function token))))
                                    ((consp tagnames)
                                     (loop for tag in tagnames
                                           collect `(,tag (,function token))))
                                    ((eql 'default tagnames)
                                     (setf default `(,function token))
                                     nil)
                                    (t (error "Invalid tag name clause ~S" tagnames))))))
          (if (not string-cases)
              default
              `(string-case
                   (tagname :default ,default)
                 ,@string-cases))))))

;; Default methods
(defmethod %process-comment (*phase* token)
  ;; For most phases the following is correct. Where it's not it will be
  ;; overridden.
  (insert-comment token (last-open-element))
  nil)

(defmethod %process-doctype (*phase* token)
  (parser-parse-error :unexpected-doctype)
  nil)

(defmethod %process-characters (*phase* token)
  (parser-insert-text (getf token :data))
  nil)

(defmethod %process-space-characters (*phase* token)
  (parser-insert-text (getf token :data))
  nil)

(defmethod %start-tag-html (*phase* token)
  (with-slots (first-start-tag open-elements)
      *parser*
    (when (and (not first-start-tag)
               (string= (getf token :name) "html"))
      (parser-parse-error :non-html-root))
    ;; XXX Need a check here to see if the first start tag token emitted is
    ;; this token... If it's not, invoke self.parser.parseError().
    (let ((root-element (first open-elements)))
      (loop for (name . value) in (getf token :data)
            do (unless (element-attribute root-element name)
                 (setf (element-attribute root-element name) value))))
    (setf first-start-tag nil)
    nil))

;; InitialPhase
(def :initial process-space-characters ()
  nil)

(def :initial process-comment ()
  (insert-comment token (document*))
  nil)

(def :initial process-doctype (compat-mode phase)
  (destructuring-bind (&key name public-id system-id correct &allow-other-keys)
      token

    (when (or (string/= name "html")
              public-id
              (and system-id (string/= system-id "about:legacy-compat")))
      (parser-parse-error :unknown-doctype))

    (unless public-id
      (setf public-id ""))

    (insert-doctype token)

    (setf public-id (ascii-upper-2-lower public-id))

    (cond ((or (not correct)
               (string/= name "html")
               (cl-ppcre:scan +quirks-mode-doctypes-regexp+ public-id)
               (member public-id '("-//w3o//dtd w3 html strict 3.0//en//"
                                   "-/w3c/dtd html 4.0 transitional/en"
                                   "html")
                       :test #'string=)
               (and (not system-id)
                    (cl-ppcre:scan '(:sequence :start-anchor (:alternation
                                                              "-//w3c//dtd html 4.01 frameset//"
                                                              "-//w3c//dtd html 4.01 transitional//"))
                                   public-id))
               (and system-id
                    (equal (ascii-upper-2-lower system-id)
                           "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd")))
           (setf compat-mode :quirks))
          ((or (cl-ppcre:scan '(:sequence :start-anchor (:alternation
                                                         "-//w3c//dtd xhtml 1.0 frameset//"
                                                         "-//w3c//dtd xhtml 1.0 transitional//"))
                              public-id)
               (and system-id
                    (cl-ppcre:scan '(:sequence :start-anchor (:alternation
                                                              "-//w3c//dtd html 4.01 frameset//"
                                                              "-//w3c//dtd html 4.01 transitional//"))
                                   public-id)))
           (setf compat-mode :limited-quirks)))
    (setf phase :before-html)
    nil))

(flet ((anything-else ()
         (with-slots (compat-mode phase)
             *parser*
           (setf compat-mode :quirks)
           (setf phase :before-html))))

  (def :initial process-characters ()
    (parser-parse-error :expected-doctype-but-got-chars)
    (anything-else)
    token)

  (def :initial process-start-tag ()
    (parser-parse-error :expected-doctype-but-got-start-tag
                        (list :name (getf token :name)))
    (anything-else)
    token)

  (def :initial process-end-tag ()
    (parser-parse-error :expected-doctype-but-got-end-tag
                        (list :name (getf token :name)))
    (anything-else)
    token)

  (def :initial process-eof ()
    (parser-parse-error :expected-doctype-but-got-eof)
    (anything-else)
    t))

;; BeforeHtmlPhase
(flet ((insert-html-element ()
         (insert-root (implied-tag-token "html" :start-tag))
         (setf (parser-phase *parser*) :before-head)))


  (def :before-html process-eof ()
    (insert-html-element)
    t)

  (def :before-html process-comment ()
    (insert-comment token (document*))
    nil)

  (def :before-html process-space-characters ()
    nil)

  (def :before-html process-characters ()
    (insert-html-element)
    token)

  (def :before-html process-start-tag (first-start-tag)
    (when (string= (getf token :name) "html")
      (setf first-start-tag t))
    (insert-html-element)
    token)

  (def :before-html process-end-tag ()
    (cond ((not (member (getf token :name) '("head" "body" "html" "br") :test #'string=))
           (parser-parse-error :unexpected-end-tag-before-html `(:name ,(getf token :name)))
           nil)
          (t
           (insert-html-element)
           token))))

;; BeforeHeadPhase
(tagname-dispatch :before-head process-start-tag
  ("html" start-tag-html)
  ("head" start-tag-head token)
  (default start-tag-other))

(tagname-dispatch :before-head process-end-tag
  (("head" "body" "html" "br") end-tag-imply-head)
  (default end-tag-other))

(def :before-head process-eof ()
  (start-tag-head (implied-tag-token "head" :start-tag))
  t)

(def :before-head process-space-characters ()
  nil)

(def :before-head process-characters ()
  (start-tag-head (implied-tag-token "head" :start-tag))
  token)

(def :before-head start-tag-html ()
  (process-start-tag token :phase :in-body))

(def :before-head start-tag-head (head-pointer)
  (insert-element token)
  (setf head-pointer (last-open-element))
  (setf (parser-phase *parser*) :in-head)
  nil)

(def :before-head start-tag-other ()
  (start-tag-head (implied-tag-token "head" :start-tag))
  token)

(def :before-head end-tag-imply-head ()
  (start-tag-head (implied-tag-token "head" :start-tag))
  token)

(def :before-head end-tag-other ()
  (parser-parse-error :end-tag-after-implied-root `(:name ,(getf token :name)))
  nil)

;; InHeadPhase
(tagname-dispatch :in-head process-start-tag
  ("html" start-tag-html)
  ("title" start-tag-title)
  (("noscript" "noframes" "style") start-tag-no-script-no-frames-style)
  ("script" start-tag-script)
  (("base" "basefont" "bgsound" "command" "link") start-tag-base-link-command)
  ("meta" start-tag-meta)
  ("head" start-tag-head)
  (default start-tag-other))

(tagname-dispatch :in-head process-end-tag
  ("head" end-tag-head)
  (("br" "html" "body") end-tag-html-body-br)
  (default end-tag-other))

(flet ((anything-else ()
         (end-tag-head (implied-tag-token "head"))))
  ;; the real thing
  (def :in-head process-eof ()
    (anything-else)
    t)

  (def :in-head process-characters ()
    (anything-else)
    token)

  (def :in-head start-tag-html ()
    (process-start-tag token :phase :in-body))

  (def :in-head start-tag-head ()
    (parser-parse-error :two-heads-are-not-better-than-one)
    nil)

  (def :in-head start-tag-base-link-command (open-elements)
    (insert-element token)
    (pop-end open-elements)
    (setf (getf token :self-closing-acknowledged) t)
    nil)

  (defun parse-content-attr (string)
    "The algorithm for extracting an encoding from a meta element"
    (let ((position 0))                 ; Step 1
      (labels ((char-at (index)
                 (and (< position (length string))
                      (char string index)))
               (skip-space ()
                 (loop while (member (char-at position) +space-characters+)
                       do (incf position))))
        ;; Step 2
        (loop
          (setf position (search "charset" string :start2 position))
          (unless position
            (return-from parse-content-attr))
          ;; Set position to after charset
          (incf position 7)
          ;; Step 3
          (skip-space)
          ;; Step 4
          (when (eql (char-at position) #\=)
            (return))
          (decf position))
        ;; Step 5
        (incf position)
        (skip-space)
        ;; Step 6
        (let ((next-char (char-at position)))
          (cond ((or (eql #\' next-char)
                     (eql #\" next-char))
                 (incf position)
                 (let ((end (position next-char string :start position)))
                   (when end
                     (subseq string position end))))
                (next-char
                 (let ((start position))
                   (loop until (or (= position (length string))
                                   (member (char-at position) +space-characters+))
                         do (incf position))
                   (subseq string start position))))))))


  (def :in-head start-tag-meta (tokenizer open-elements)
    (insert-element token)
    (pop-end open-elements)
    (setf (getf token :self-closing-acknowledged) t)

    (let ((attributes (getf token :data)))
      (when (eql (cdr (html5-stream-encoding (tokenizer-stream tokenizer))) :tentative)
        (cond ((assoc "charset" attributes :test #'string=)
               (html5-stream-change-encoding (tokenizer-stream tokenizer)
                                             (cdr (assoc "charset" attributes :test #'string=))))
              ((and (assoc "http-equiv" attributes :test #'string=)
                    (ascii-istring= (cdr (assoc "http-equiv" attributes :test #'string=))
                                    "Content-Type")
                    (assoc "content" attributes :test #'string=))
               (let* ((content (cdr (assoc "content" attributes :test #'string=)))
                      (new-encoding (parse-content-attr content)))
                 (if new-encoding
                     (html5-stream-change-encoding (tokenizer-stream tokenizer)
                                                   new-encoding)
                     (parser-parse-error :invalid-encoding-declaration
                                         `(:content ,content))))))))
    nil)

  (def :in-head start-tag-title ()
    (parse-rc-data-raw-text token :rcdata)
    nil)

  (def :in-head start-tag-no-script-no-frames-style ()
    ;; Need to decide whether to implement the scripting-disabled case
    (parse-rc-data-raw-text token :rawtext))

  (def :in-head start-tag-script (tokenizer original-phase phase)
    (insert-element token)
    (setf (tokenizer-state tokenizer) :script-data-state)
    (setf original-phase phase)
    (setf phase :text)
    nil)

  (def :in-head start-tag-other ()
    (anything-else)
    token)

  (def :in-head end-tag-head (phase open-elements)
    (let ((node (pop-end open-elements)))
      (assert (string= (node-name node) "head") ()  "Expected head got ~S" (node-name node))
      (setf phase :after-head)
      nil))

  (def :in-head end-tag-html-body-br ()
    (anything-else)
    token)

  (def :in-head end-tag-other ()
    (parser-parse-error :unexpected-end-tag `(:name ,(getf token :name)))
    nil))

;; XXX If we implement a parser for which scripting is disabled we need to
;; implement this phase.

;; InHeadNoScriptPhase

;; AfterHeadPhase
(tagname-dispatch :after-head process-start-tag
  ("html" start-tag-html)
  ("body" start-tag-body)
  ("frameset" start-tag-frameset)
  (("base" "basefont" "bgsound" "link" "meta"
           "noframes" "script" "style" "title")
   start-tag-from-head)
  ("head" start-tag-head)
  (default start-tag-other))

(tagname-dispatch :after-head process-end-tag
  (("body" "html" "br") end-tag-html-body-br)
  (default end-tag-other))

(flet ((anything-else ()
         (with-slots (phase frameset-ok) *parser*
           (insert-element (implied-tag-token "body" :start-tag))
           (setf phase :in-body)
           (setf frameset-ok t))))

  (def :after-head process-eof ()
    (anything-else)
    t)

  (def :after-head process-characters ()
    (anything-else)
    token)

  (def :after-head start-tag-html ()
    (process-start-tag token :phase :in-body))

  (def :after-head start-tag-body (phase frameset-ok)
    (setf frameset-ok nil)
    (insert-element token)
    (setf phase :in-body)
    nil)

  (def :after-head start-tag-frameset (phase)
    (insert-element token)
    (setf phase :in-frameset)
    nil)

  (def :after-head start-tag-from-head (head-pointer open-elements)
    (parser-parse-error :unexpected-start-tag-out-of-my-head
                        `(:name ,(getf token :name)))
    (push-end head-pointer open-elements)
    (process-start-tag token :phase :in-head)
    (loop for node in (reverse open-elements)
          do (when (string= "head" (node-name node))
               (setf open-elements
                     (remove node open-elements :test #'equal))
               (return)))
    nil)

  (def :after-head start-tag-head ()
    (parser-parse-error :unexpected-start-tag
                        `(:name ,(getf token :name)))
    nil)

  (def :after-head start-tag-other ()
    (anything-else)
    token)

  (def :after-head end-tag-html-body-br ()
    (anything-else)
    token)

  (def :after-head end-tag-other ()
    (parser-parse-error :unexpected-end-tag
                        `(:name ,(getf token :name)))
    nil))

;; InBodyPhase
(tagname-dispatch :in-body process-start-tag
  ("html" start-tag-html)
  (("base" "basefont" "bgsound" "command" "link"
           "meta" "noframes" "script" "style" "title")
   start-tag-process-in-head)
  ("body" start-tag-body)
  ("frameset" start-tag-frameset)
  (("address" "article" "aside" "blockquote" "center" "details"
              "dir" "div" "dl" "fieldset" "figcaption" "figure"
              "footer" "header" "hgroup" "menu" "nav" "ol" "p"
              "section" "summary" "ul")
   start-tag-close-p)
  (#.+heading-elements+ start-tag-heading)
  (("pre" "listing") start-tag-pre-listing)
  ("form" start-tag-form)
  (("li" "dd" "dt") start-tag-list-item)
  ("plaintext" start-tag-plaintext)
  ("a" start-tag-a)
  (("b" "big" "code" "em" "font" "i" "s" "small" "strike"
        "strong" "tt" "u")
   start-tag-formatting)
  ("nobr" start-tag-nobr)
  ("button" start-tag-button)
  (("applet" "marquee" "object") start-tag-applet-marquee-object)
  ("xmp" start-tag-xmp)
  ("table" start-tag-table)
  (("area" "br" "embed" "img" "keygen" "wbr")
   start-tag-void-formatting)
  (("param" "source" "track") start-tag-param-source)
  ("input" start-tag-input)
  ("hr" start-tag-hr)
  ("image" start-tag-image)
  ("isindex" start-tag-is-index)
  ("textarea" start-tag-textarea)
  ("iframe" start-tag-i-frame)
  (("noembed" "noscript") start-tag-rawtext)
  ("select" start-tag-select)
  (("rp" "rt") start-tag-rp-rt)
  (("option" "optgroup") start-tag-opt)
  (("math") start-tag-math)
  (("svg") start-tag-svg)
  (("caption" "col" "colgroup" "frame" "head"
              "tbody" "td" "tfoot" "th" "thead"
              "tr")
   start-tag-misplaced)
  (default start-tag-other))

(tagname-dispatch :in-body process-end-tag
  ("body" end-tag-body)
  ("html" end-tag-html)
  (("address" "article" "aside" "blockquote" "button" "center"
              "details" "dir" "div" "dl" "fieldset" "figcaption" "figure"
              "footer" "header" "hgroup" "listing" "menu" "nav" "ol" "pre"
              "section" "summary" "ul")
   end-tag-block)
  ("form" end-tag-form)
  ("p" end-tag-p)
  (("dd" "dt" "li") end-tag-list-item)
  (#.+heading-elements+ end-tag-heading)
  (("a" "b" "big" "code" "em" "font" "i" "nobr" "s" "small"
        "strike" "strong" "tt" "u")
   end-tag-formatting)
  (("applet" "marquee" "object") end-tag-applet-marquee-object)
  ("br" end-tag-br)
  (default end-tag-other))

(flet ((is-matching-formatting-element (node1 node2)
         (and (equal (node-name node1) (node-name node2))
              (equal (node-namespace node1) (node-namespace node2))
              (node-attributes= node1 node2))))

  (def :in-body add-formatting-element (reverse active-formatting-elements)
    (insert-element token)
    (let ((element (last-open-element))
          matching-elements)
      (loop for node in (reverse active-formatting-elements)
            do (if (eq node :marker)
                   (return)
                   (when (is-matching-formatting-element node element)
                     (push-end node matching-elements))))
      (assert (<= (length matching-elements) 3))
      (when (= (length matching-elements) 3)
        (setf active-formatting-elements
              (remove (car (last matching-elements))
                      active-formatting-elements)))
      (assert element)
      (push-end element active-formatting-elements))
    nil))

(def :in-body process-eof (open-elements)
  (let ((allowed-elements '("dd" "dt" "li" "p" "tbody" "td"
                            "tfoot" "th" "thead" "tr" "body" "html")))
    (loop for node in (reverse open-elements)
          do (when (not (member (node-name node)
                                allowed-elements
                                :test #'string=))
               (parser-parse-error :expected-closing-tag-but-got-eof)
               (return))))
  nil)

(def :in-body process-characters (frameset-ok)
  (let ((data (getf token :data)))
    (if (equal data (string #\u0000))
        nil
        (progn
          (reconstruct-active-formatting-elements)
          (parser-insert-text data)
          ;;This must be bad for performance
          (when (and frameset-ok
                     (notevery (lambda (char)
                                 (find char +space-characters+))
                               data))
            (setf frameset-ok nil))
          nil))))

(def :in-body process-space-characters (in-body-process-space-characters-mode)
  (ecase in-body-process-space-characters-mode
    (:non-pre
     (reconstruct-active-formatting-elements)
     (parser-insert-text (getf token :data)))
    (:drop-newline
     (let ((data (getf token :data)))
       (setf in-body-process-space-characters-mode :non-pre)
       (when (and (plusp (length data))
                  (char= #\Newline (char data 0))
                  (member (node-name (last-open-element))
                          '("pre" "listing" "textarea")
                          :test #'string=)
                  (not (node-has-content (last-open-element))))
         (setf data (subseq data 1)))
       (when (plusp (length data))
         (reconstruct-active-formatting-elements)
         (parser-insert-text data)))))
  nil)

(def :in-body start-tag-process-in-head ()
  (process-start-tag token :phase :in-head))

(def :in-body start-tag-body (frameset-ok open-elements)
  (parser-parse-error :unexpected-start-tag
                      `(:name ,(getf token :name)))
  (if (or (= 1 (length open-elements))
          (string/= (node-name (second open-elements)) "body"))
      (assert (slot-value *parser* 'inner-html))
      (progn
        (setf frameset-ok nil)
        (loop for (name . value) in (getf token :data)
              do (unless (element-attribute (second open-elements) name)
                   (setf (element-attribute (second open-elements) name) value)))))
  nil)

(def :in-body start-tag-frameset (frameset-ok phase open-elements)
  (parser-parse-error :unexpected-start-tag
                      `(:name ,(getf token :name)))
  (cond ((or (= 1 (length open-elements))
             (string/= (node-name (second open-elements)) "body"))
         (assert (slot-value *parser* 'inner-html)))
        ((not frameset-ok)
         nil)
        (t
         (when (node-parent (second open-elements))
           (node-remove-child (node-parent (second open-elements))
                              (second open-elements)))
         (loop until (string= (node-name (last-open-element))
                              "html")
               do (pop-end open-elements))
         (insert-element token)
         (setf phase :in-frameset)))
  nil)

(def :in-body start-tag-close-p ()
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (insert-element token)
  nil)

(def :in-body start-tag-pre-listing (in-body-process-space-characters-mode frameset-ok)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (insert-element token)
  (setf frameset-ok nil)
  (setf in-body-process-space-characters-mode :drop-newline)
  nil)

(def :in-body start-tag-form (form-pointer)
  (if form-pointer
      (parser-parse-error :unexpected-start-tag
                          `(:name ,(getf token :name)))
      (progn
        (when (element-in-scope "p" "button")
          (end-tag-p (implied-tag-token "p")))
        (insert-element token)
        (setf form-pointer (last-open-element))))
  nil)

(def :in-body start-tag-list-item (phase frameset-ok open-elements)
  (setf frameset-ok nil)
  (let ((stop-names (cond ((string= (getf token :name) "li")
                           '("li"))
                          ((string= (getf token :name) "dt")
                           '("dt" "dd"))
                          ((string= (getf token :name) "dd")
                           '("dt" "dd")))))
    (loop for node in (reverse open-elements)
          do (cond ((member (node-name node) stop-names :test #'string=)
                    (process-end-tag (implied-tag-token (node-name node)) :phase phase)
                    (return))
                   ((and (member (node-name-tuple node) +special-elements+
                                 :test #'equal)
                         (not (member (node-name node)
                                      '("address" "div" "p")
                                      :test #'string=)))
                    (return)))))
  (when (element-in-scope "p" "button")
    (process-end-tag (implied-tag-token "p") :phase phase))
  (insert-element token)
  nil)

(def :in-body start-tag-plaintext (tokenizer)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (insert-element token)
  (setf (tokenizer-state tokenizer) :plaintext-state)
  nil)

(def :in-body start-tag-heading (open-elements)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (when (member (node-name (last-open-element)) +heading-elements+
                :test #'string=)
    (perror :unexpected-start-tag :name (getf token :name))
    (pop-end open-elements))
  (insert-element token)
  nil)

(def :in-body start-tag-a (open-elements active-formatting-elements)
  (let ((afe-a-element (element-in-active-formatting-elements "a")))
    (when afe-a-element
      (perror :unexpected-start-tag-implies-end-tag
              :start-name "a" :end-name "a")
      (end-tag-formatting (implied-tag-token "a"))
      (when (member afe-a-element open-elements)
        (setf open-elements
              (remove afe-a-element open-elements)))
      (when (member afe-a-element active-formatting-elements)
        (setf active-formatting-elements
              (remove afe-a-element active-formatting-elements))))
    (reconstruct-active-formatting-elements)
    (add-formatting-element token))
  nil)

(def :in-body start-tag-formatting ()
  (reconstruct-active-formatting-elements)
  (add-formatting-element token)
  nil)

(def :in-body start-tag-nobr ()
  (reconstruct-active-formatting-elements)
  (when (element-in-scope "nobr")
    (perror :unexpected-start-tag-implies-end-tag
            :start-name "nobr" :end-name "nobr")
    (process-end-tag (implied-tag-token "nobr"))
    ;; XXX Need tests that trigger the following
    (reconstruct-active-formatting-elements))
  (add-formatting-element token)
  nil)

(def :in-body start-tag-button (frameset-ok)
  (cond ((element-in-scope "button")
         (perror :unexpected-start-tag-implies-end-tag
                 :start-name "button" :end-name "button")
         (process-end-tag (implied-tag-token "button"))
         token)
        (t
         (reconstruct-active-formatting-elements)
         (insert-element token)
         (setf frameset-ok nil)
         nil)))

(def :in-body start-tag-applet-marquee-object (frameset-ok active-formatting-elements)
  (reconstruct-active-formatting-elements)
  (insert-element token)
  (push-end :marker active-formatting-elements)
  (setf frameset-ok nil)
  nil)

(def :in-body start-tag-xmp (frameset-ok)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (reconstruct-active-formatting-elements)
  (setf frameset-ok nil)
  (parse-rc-data-raw-text token :rawtext)
  nil)

(def :in-body start-tag-table (frameset-ok compat-mode phase)
  (when (not (eq compat-mode :quirks))
    (when (element-in-scope "p" "button")
      (end-tag-p (implied-tag-token "p"))))
  (insert-element token)
  (setf frameset-ok nil)
  (setf phase :in-table)
  nil)

(def :in-body start-tag-void-formatting (frameset-ok open-elements)
  (reconstruct-active-formatting-elements)
  (insert-element token)
  (pop-end open-elements)
  (setf (getf token :self-closing-acknowledged) t)
  (setf frameset-ok nil)
  nil)

(def :in-body start-tag-input (frameset-ok)
  (let ((old-frameset-ok frameset-ok))
    (start-tag-void-formatting token)
    (let ((type (assoc "type" (getf token :data) :test #'string=)))
      (when (and type
                 (string= (ascii-upper-2-lower (cdr type)) "hidden"))
        ;;input type=hidden doesn't change framesetOK
        (setf frameset-ok old-frameset-ok))))
  nil)

(def :in-body start-tag-param-source (open-elements)
  (insert-element token)
  (pop-end open-elements)
  (setf (getf token :self-closing-acknowledged) t)
  nil)

(def :in-body start-tag-hr (frameset-ok open-elements)
  (when (element-in-scope "p" "button")
    (end-tag-p (implied-tag-token "p")))
  (insert-element token)
  (pop-end open-elements)
  (setf (getf token :self-closing-acknowledged) t)
  (setf frameset-ok nil)
  nil)

(def :in-body start-tag-image ()
  (perror :unexpected-start-tag-treated-as
          :original-name "image" :new-name "img")
  (process-start-tag (implied-tag-token/full
                      "img" :start-tag
                      :attributes (getf token :data)
                      :self-closing (getf token :self-closing)))
  nil)

(def :in-body start-tag-is-index (form-pointer)
  (block nil
    (perror :deprecated-tag :name "isindex")
    (when form-pointer
      (return nil))
    (let (attrs)
      (when (assoc "action" (getf token :data) :test #'string=)
        (setf attrs (list (assoc "action" (getf token :data) :test #'string=))))
      (process-start-tag (implied-tag-token/full "form" :start-tag
                                                 :attributes attrs)))
    (process-start-tag (implied-tag-token "hr" :start-tag))
    (process-start-tag (implied-tag-token "label" :start-tag))
    ;; XXX Localization ...
    (let ((prompt (if (assoc "prompt" (getf token :data) :test #'string=)
                      (cdr (assoc "prompt" (getf token :data) :test #'string=))
                      "This is a searchable index. Enter search keywords: ")))
      (process-characters (list :type :characters :data prompt)))
    (let ((attrs (append (remove-if (lambda (el)
                                      (member (car el) '("action" "prompt" "name")
                                              :test #'string=))
                                    (copy-list (getf token :data)))
                         (copy-list '(("name" . "isindex"))))))
      (process-start-tag (implied-tag-token/full "input" :start-tag
                                                 :attributes attrs
                                                 :self-closing
                                                 (getf token :self-closing))))
    (process-end-tag (implied-tag-token "label"))
    (process-start-tag (implied-tag-token "hr" :start-tag))
    (process-end-tag (implied-tag-token "form")))
  nil)

(def :in-body start-tag-textarea (tokenizer
                                  in-body-process-space-characters-mode
                                  frameset-ok)
  (insert-element token)
  (setf (tokenizer-state tokenizer) :rcdata-state)
  (setf in-body-process-space-characters-mode :drop-newline)
  (setf frameset-ok nil)
  nil)

(def :in-body start-tag-i-frame (frameset-ok)
  (setf frameset-ok nil)
  (start-tag-rawtext token)
  nil)

(def :in-body start-tag-rawtext ()
  ;; iframe, noembed noframes, noscript(if scripting enabled)
  (parse-rc-data-raw-text token :rawtext)
  nil)

(def :in-body start-tag-opt (phase)
  (when (string= (node-name (last-open-element)) "option")
    (process-end-tag (implied-tag-token "option") :phase phase))
  (reconstruct-active-formatting-elements)
  (insert-element token)
  nil)

(def :in-body start-tag-select (frameset-ok)
  (reconstruct-active-formatting-elements)
  (insert-element token)
  (setf frameset-ok nil)
  (if (member (parser-phase *parser*) '(:in-table :in-caption :in-column-group
                                        :in-table-body :in-row :in-cell))
      (setf (parser-phase *parser*) :in-select-in-table)
      (setf (parser-phase *parser*) :in-select))
  nil)

(def :in-body start-tag-rp-rt ()
  (when (element-in-scope "ruby")
    (generate-implied-end-tags)
    (when (string/= (node-name (last-open-element)) "ruby")
      (perror :expected-ruby-tag)))
  (insert-element token)
  nil)

(def :in-body start-tag-math (open-elements)
  (reconstruct-active-formatting-elements)
  (adjust-math-ml-attributes token)
  (adjust-foreign-attributes token)
  (setf (getf token :namespace) (find-namespace "mathml"))
  (insert-element token)
  ;;Need to get the parse error right for the case where the token
  ;;has a namespace not equal to the xmlns attribute
  (when (getf token :self-closing)
    (pop-end open-elements)
    (setf (getf token :self-closing-acknowledged) t))
  nil)

(def :in-body start-tag-svg (open-elements)
  (reconstruct-active-formatting-elements)
  (adjust-svg-attributes token)
  (adjust-foreign-attributes token)
  (setf (getf token :namespace) (find-namespace "svg"))
  (insert-element token)
  ;;Need to get the parse error right for the case where the token
  ;;has a namespace not equal to the xmlns attribute
  (when (getf token :self-closing)
    (pop-end open-elements)
    (setf (getf token :self-closing-acknowledged) t))
  nil)

(def :in-body start-tag-misplaced ()
  ;; Elements that should be children of other elements that have a
  ;; different insertion mode; here they are ignored
  ;; "caption", "col", "colgroup", "frame", "frameset", "head",
  ;; "option", "optgroup", "tbody", "td", "tfoot", "th", "thead",
  ;; "tr", "noscript"
  (perror :unexpected-start-tag-ignored :name (getf token :name))
  nil)

(def :in-body start-tag-other ()
  (reconstruct-active-formatting-elements)
  (insert-element token)
  nil)

(def :in-body end-tag-p (open-elements)
  (cond ((not (element-in-scope "p" "button"))
         (start-tag-close-p (implied-tag-token "p" :start-tag))
         (perror :unexpected-end-tag :name "p")
         (end-tag-p (implied-tag-token "p")))
        (t
         (generate-implied-end-tags "p")
         (when (string/= (node-name (last-open-element)) "p")
           (perror :unexpected-end-tag :name "p"))
         (let ((node (pop-end open-elements)))
           (loop until (string= (node-name node) "p")
                 do (setf node (pop-end open-elements))))))
  nil)

(def :in-body end-tag-body (open-elements)
  (block nil
    (when (not (element-in-scope "body"))
      (perror :unexpected-scope)
      (return nil))
    (when (string/= (node-name (last-open-element)) "body")
      (loop for node in (cddr open-elements)
            do (when (member (node-name node)
                             '("dd" "dt" "li" "optgroup" "option" "p" "rp"
                               "rt" "tbody" "td" "tfoot" "th" "thead" "tr"
                               "body" "html")
                             :test #'string=)
                 ;;Not sure this is the correct name for the parse error
                 (perror :expected-one-end-tag-but-got-another
                         :expected-name "body" :got-name (node-name node))
                 (return)))))
  (setf (parser-phase *parser*) :after-body)
  nil)

(def :in-body end-tag-html ()
  ;;We repeat the test for the body end tag token being ignored here
  (cond ((element-in-scope "body")
         (end-tag-body (implied-tag-token "body"))
         token)
        (t nil)))

(def :in-body end-tag-block (in-body-process-space-characters-mode open-elements)
  ;;Put us back in the right whitespace handling mode
  (when (string= (getf token :name) "pre")
    (setf in-body-process-space-characters-mode :non-pre))
  (let ((in-scope (element-in-scope (getf token :name))))
    (when in-scope
      (generate-implied-end-tags))
    (when (string/= (node-name (last-open-element))
                    (getf token :name))
      (perror :end-tag-too-early :name (getf token :name)))
    (when in-scope
      (let ((node (pop-end open-elements)))
        (loop until (string= (node-name node) (getf token :name))
              do (setf node (pop-end open-elements))))))
  nil)

(def :in-body end-tag-form (form-pointer open-elements)
  (let ((node form-pointer))
    (setf form-pointer nil)
    (if (or (null node) (not (element-in-scope (node-name node))))
        (perror :unexpected-end-tag :name "form")
        (progn
          (generate-implied-end-tags)
          (when (not (equal (last-open-element) node))
            (perror :end-tag-too-early-ignored :name "form"))
          (setf open-elements
                (remove node open-elements)))))
  nil)

;; Note:
;;   - A token is a plist.
;;   - A property is an alist.
;;   - A node is an object.
;;   - An element is a node.
(def :in-body end-tag-list-item (open-elements)
  (let ((variant (if (string= (getf token :name) "li")
                     "list"
                     nil)))
    (if (not (element-in-scope (getf token :name) variant))
        (perror :unexpected-end-tag :name (getf token :name))
        (progn
          (generate-implied-end-tags (getf token :name))
          (when (string/= (node-name (last-open-element))
                          (getf token :name))
            (perror :end-tag-too-early :name (getf token :name)))
          (let ((node (pop-end open-elements)))
            (loop until (string= (node-name node) (getf token :name))
                  do (setf node (pop-end open-elements)))))))
  nil)

(def :in-body end-tag-heading (open-elements)
  (loop for item in +heading-elements+
        do (when (element-in-scope item)
             (generate-implied-end-tags)
             (return)))
  (when (string/= (node-name (last-open-element))
                  (getf token :name))
    (perror :end-tag-too-early :name (getf token :name)))
  (loop for item in +heading-elements+
        do (when (element-in-scope item)
             (let ((item (pop-end open-elements)))
               (loop until (member (node-name item) +heading-elements+
                                   :test #'string=)
                     do (setf item (pop-end open-elements))))))
  nil)

(defmacro insert-elt-at (object index place)
  (let ((tmp (gensym "TMP"))
        (object-symbol (gensym "OBJECT"))
        (index-symbol (gensym "INDEX")))
    `(let ((,object-symbol ,object)
           (,index-symbol ,index)
           (,tmp ,place))
       (setf ,place (append (subseq ,tmp 0 (min ,index-symbol (length ,tmp)))
                            (list ,object-symbol)
                            (nthcdr ,index-symbol ,tmp))))))

(def :in-body end-tag-formatting (active-formatting-elements open-elements)
  ;; The much-feared adoption agency algorithm
  ;; http://www.whatwg.org/specs/web-apps/current-work/#adoptionAgency
  ;; XXX Better parseError messages appreciated.
  (loop named outer
        with name = (getf token :name)
        with outer-loop-counter = 0
        with formatting-element
        with afe-index
        with furthest-block
        with bookmark
        with last-node
        with inner-loop-counter
        with index
        with node
        with common-ancestor
        with clone
        while (< outer-loop-counter 8)
        do
           (incf outer-loop-counter)
           ;; Step 1 paragraph 1
           (setf formatting-element
                 (element-in-active-formatting-elements name))
           (cond ((or (not formatting-element)
                      (and (member formatting-element
                                   open-elements)
                           (not (element-in-scope
                                 (node-name formatting-element)))))
                  (perror :adoption-agency-1.1 :name name)
                  (return-from outer nil))
                 ;; Step 1 paragraph 2
                 ((not (member formatting-element
                               open-elements))
                  (perror :adoption-agency-1.2 :name name)
                  (setf active-formatting-elements
                        (remove formatting-element active-formatting-elements))
                  (return-from outer nil)))
           ;; Step 1 paragraph 3
           (unless (eql formatting-element
                        (last-open-element))
             (perror :adoption-agency-1.3 :name name))
           ;; Step 2
           ;; Start of the adoption agency algorithm proper
           (setf afe-index (position formatting-element
                                     open-elements))
           (setf furthest-block nil)
           (loop for element in (subseq open-elements
                                        afe-index)
                 do (when (member (node-name-tuple element)
                                  +special-elements+
                                  :test #'equal)
                      (setf furthest-block element)
                      (return)))
           ;; Step 3
           (when (null furthest-block)
             (loop for element = (pop-end open-elements)
                   until (eql formatting-element element)
                   finally (setf active-formatting-elements
                                 (remove element
                                         active-formatting-elements)))
             (return-from outer nil))
           (setf common-ancestor (elt open-elements (- afe-index 1)))
           ;; Step 5
           ;;if furthestBlock.parent:
           ;;    furthestBlock.parent.removeChild(furthestBlock)

           ;; Step 5
           ;; The bookmark is supposed to help us
           ;; identify where to reinsert nodes in step
           ;; 12. We have to ensure that we reinsert
           ;; nodes after the node before the active
           ;; formatting element.  Note the bookmark can
           ;; move in step 7.4
           (setf bookmark (position formatting-element
                                    active-formatting-elements))
           ;; Step 6
           (setf node furthest-block)
           (setf last-node node)
           (setf inner-loop-counter 0)
           (setf index (position node open-elements))
           (loop named inner
                 while (< inner-loop-counter 3)
                 do
                    (block continue
                      (incf inner-loop-counter)
                      ;; Node is element before node in open elements
                      (decf index)
                      (setf node (elt open-elements index))
                      (when (not (member node active-formatting-elements))
                        (setf open-elements
                              (remove node open-elements))
                        (return-from continue))
                      ;; Step 6.3
                      (when (eql node formatting-element)
                        (return-from inner))
                      ;; Step 6.4
                      (when (eql last-node furthest-block)
                        (setf bookmark (1+ (position node
                                                     active-formatting-elements))))
                      ;; Step 6.5
                      (setf clone (node-clone* node))
                      ;; Replace node with clone
                      (symbol-macrolet
                          ((af active-formatting-elements)
                           (oe open-elements))
                        (setf (elt af (position node af)) clone)
                        (setf (elt oe (position node oe)) clone))
                      (setf node clone)
                      ;; Step 6.6
                      ;; Remove lastNode from its parents, if any
                      (when (node-parent last-node)
                        (node-remove-child (node-parent last-node)
                                           last-node))
                      (node-append-child node last-node)
                      ;; Step 7.7
                      (setf last-node node))) ; End of inner loop
           ;; Step 7
           ;; Foster parent lastNode if commonAncestor is a
           ;; table, tbody, tfoot, thead, or tr we need to
           ;; foster parent the lastNode
           (when (node-parent last-node)
             (node-remove-child (node-parent last-node)
                                last-node))

           (if (member (node-name common-ancestor)
                       '("table" "tbody" "tfoot" "thead" "tr")
                       :test #'string=)
               (multiple-value-bind (parent insert-before)
                   (get-table-misnested-nodeposition)
                 (node-insert-before* parent last-node insert-before))
               (node-append-child* common-ancestor last-node))
           ;; Step 8
           (setf clone (node-clone* formatting-element))
           ;; Step 9
           (node-reparent-children furthest-block clone)
           ;; Step 10
           (node-append-child* furthest-block clone)
           ;; Step 11
           (setf active-formatting-elements
                 (remove formatting-element
                         active-formatting-elements))
           (insert-elt-at clone bookmark active-formatting-elements)
           ;; Step 12
           (setf open-elements
                 (remove formatting-element
                         open-elements))
           (insert-elt-at clone
                          (1+ (position furthest-block
                                        open-elements))
                          open-elements))
  nil)

(def :in-body end-tag-applet-marquee-object (open-elements)
  (when (element-in-scope (getf token :name))
    (generate-implied-end-tags))
  (when (string/= (node-name (last-open-element))
                  (getf token :name))
    (perror :end-tag-too-early :name (getf token :name)))
  (when (element-in-scope (getf token :name))
    (let ((element (pop-end open-elements)))
      (loop until (string= (node-name element) (getf token :name))
            do (setf element (pop-end open-elements))))
    (clear-active-formatting-elements))
  nil)

(def :in-body end-tag-br (open-elements)
  (perror :unexpected-end-tag-treated-as
          :original-name "br" :new-name "br element")
  (reconstruct-active-formatting-elements)
  (insert-element (implied-tag-token "br" :start-tag))
  (pop-end open-elements)
  nil)

(def :in-body end-tag-other (open-elements)
  (loop for node in (reverse open-elements)
        do (cond ((string= (node-name node) (getf token :name))
                  (generate-implied-end-tags (getf token :name))
                  (when (string/= (node-name (last-open-element))
                                  (getf token :name))
                    (perror :unexpected-end-tag :name (getf token :name)))
                  (loop while (not (eq node
                                       (pop-end open-elements))))
                  (return))
                 (t
                  (when (member (node-name-tuple node) +special-elements+
                                :test #'equal)
                    (perror :unexpected-end-tag :name (getf token :name))
                    (return)))))
  nil)

;; TextPhase
(tagname-dispatch :text process-start-tag
  (default start-tag-other))

(tagname-dispatch :text process-end-tag
  ("script" end-tag-script)
  (default end-tag-other))

(def :text process-characters ()
  (parser-insert-text (getf token :data))
  nil)

(def :text process-eof (phase original-phase open-elements)
  (perror :expected-named-closing-tag-but-got-eof
          (node-name (last-open-element)))
  (pop-end open-elements)
  (setf phase original-phase)
  t)

(def :text start-tag-other ()
  (error "Tried to process start tag ~S in RCDATA/RAWTEXT mode" (getf token :name)))

(def :text end-tag-script (phase original-phase open-elements)
  (assert (string= (node-name (pop-end open-elements))
                   "script"))
  (setf phase original-phase)
  ;; The rest of this method is all stuff that only happens if
  ;; document.write works
  nil)

(def :text end-tag-other (phase original-phase open-elements)
  (pop-end open-elements)
  (setf phase original-phase)
  nil)

;; InTablePhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-table
(tagname-dispatch :in-table process-start-tag
  ("html" start-tag-html)
  ("caption" start-tag-caption)
  ("colgroup" start-tag-colgroup)
  ("col" start-tag-col)
  (("tbody" "tfoot" "thead") start-tag-row-group)
  (("td" "th" "tr") start-tag-imply-tbody)
  ("table" start-tag-table)
  (("style" "script") start-tag-style-script)
  ("input" start-tag-input)
  ("form" start-tag-form)
  (default start-tag-other))

(tagname-dispatch :in-table process-end-tag
  ("table" end-Tag-Table)
  (("body" "caption" "col" "colgroup" "html" "tbody" "td"
           "tfoot" "th" "thead" "tr") end-Tag-Ignore)
  (default end-tag-other))

(flet ((clear-stack-to-table-context ()
         ;; clear the stack back to a table context
         (loop until (member (node-name (last-open-element))
                             '("table" "html")
                             :test #'string=)
               do
               ;;(perror :unexpected-implied-end-tag-in-table
               ;;        :name (node-name* (last-open-element)))
                  (pop-end (slot-value *parser* 'open-elements)))
         ;; When the current node is <html> it's an innerHTML case
         ))

  (def :in-table process-eof (inner-html)
    (if (string/= (node-name (last-open-element)) "html")
        (perror :eof-in-table)
        (assert inner-html))
    ;; Stop parsing
    nil)

  (def :in-table process-space-characters (phase original-phase)
    (setf original-phase phase)
    (setf phase :in-table-text)
    (process-space-characters token :phase phase)
    nil)

  (def :in-table process-characters (phase original-phase)
    (setf original-phase phase)
    (setf phase :in-table-text)
    (process-characters token :phase phase)
    nil)

  (def :in-table insert-text (insert-from-table)
    ;; If we get here there must be at least one non-whitespace character
    ;; Do the table magic!
    (setf insert-from-table t)
    (process-characters token :phase :in-body)
    (setf insert-from-table nil)
    nil)

  (def :in-table start-tag-caption (phase active-formatting-elements)
    (clear-stack-to-table-context)
    (push-end :marker active-formatting-elements)
    (insert-element token)
    (setf phase :in-caption)
    nil)

  (def :in-table start-tag-colgroup (phase)
    (clear-stack-to-table-context)
    (insert-element token)
    (setf phase :in-column-group)
    nil)

  (def :in-table start-tag-col ()
    (start-tag-colgroup (implied-tag-token "colgroup" :start-tag))
    token)

  (def :in-table start-tag-row-group (phase)
    (clear-stack-to-table-context)
    (insert-element token)
    (setf phase :in-table-body)
    nil)

  (def :in-table start-tag-imply-tbody ()
    (start-tag-row-group (implied-tag-token "tbody" :start-tag))
    token)

  (def :in-table start-tag-table (phase inner-html)
    (perror :unexpected-start-tag-implies-end-tag
            :start-name "table"
            :end-name "table")
    (process-end-tag (implied-tag-token "table") :phase phase)
    (unless inner-html
      token))

  (def :in-table start-tag-style-script ()
    (process-start-tag token :phase :in-head))

  (def :in-table start-tag-input (open-elements)
    (let ((type (assoc "type" (getf token :data) :test #'string=)))
      (cond ((and type
                  (string= (ascii-upper-2-lower (cdr type)) "hidden"))
             (perror :unexpected-hidden-input-in-table)
             (insert-element token)
             ;; XXX associate with form
             (pop-end open-elements))
            (t
             (start-tag-other token))))
    nil)

  (def :in-table start-tag-form (form-pointer open-elements)
    (perror :unexpected-form-in-table)
    (unless form-pointer
      (insert-element token)
      (setf form-pointer (last-open-element))
      (pop-end open-elements))
    nil)

  (def :in-table start-tag-other (insert-from-table)
    (perror :unexpected-start-tag-implies-table-voodoo :name (getf token :name))
    ;; Do the table magic!
    (setf insert-from-table t)
    (process-start-tag token :phase :in-body)
    (setf insert-from-table nil)
    nil)

  (def :in-table end-tag-table (inner-html open-elements)
    (cond ((element-in-scope "table" "table")
           (generate-implied-end-tags)
           (unless (equal (node-name (last-open-element)) "table")
             (perror :end-tag-too-early-named
                     :got-name "table"
                     :expected-name (node-name (last-open-element))))
           (loop until (equal (node-name (last-open-element)) "table")
                 do (pop-end open-elements))
           (pop-end open-elements)
           (reset-insertion-mode))
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :end-tag-table-in-table-inner-html-case)))
    nil)

  (def :in-table end-tag-ignore ()
    (perror :unexpected-end-tag :name (getf token :name))
    nil)

  (def :in-table end-tag-other (insert-from-table)
    (perror :unexpected-end-tag-implies-table-voodoo :name (getf token :name))
    ;; Do the table magic!
    (setf insert-from-table t)
    (process-end-tag token :phase :in-body)
    (setf insert-from-table nil)
    nil))

;; InTableTextPhase
(defun flush-characters ()
  (with-slots (character-tokens) *parser*
    (let ((data (apply #'concatenate 'string
                       (loop for item in (reverse character-tokens)
                             collect (getf item :data)))))
      (if (not (only-space-characters-p data))
          (insert-text (list :type :characters
                             :data data)
                       :phase :in-table)
          (parser-insert-text data)))
    (setf character-tokens nil)))

(def :in-table-text process-comment (phase original-phase)
  (flush-characters)
  (setf phase original-phase)
  token)

(def :in-table-text process-eof (phase original-phase)
  (flush-characters)
  (setf phase original-phase)
  t)

(def :in-table-text process-characters (character-tokens)
  (unless (equal (getf token :data) (string #\u0000))
    (push token character-tokens))
  nil)

(def :in-table-text process-space-characters (character-tokens)
  ;; pretty sure we should never reach here
  (push token character-tokens)
  nil)

(def :in-table-text process-start-tag (phase original-phase)
  (flush-characters)
  (setf phase original-phase)
  token)

(def :in-table-text process-end-tag (phase original-phase)
  (flush-characters)
  (setf phase original-phase)
  token)

;; InCaptionPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-caption
(tagname-dispatch :in-caption process-start-tag
  ("html" start-tag-html)
  (("caption" "col" "colgroup" "tbody" "td" "tfoot" "th"
              "thead" "tr") start-tag-table-element)
  (default start-tag-other))

(tagname-dispatch :in-caption process-end-tag
  ("caption" end-tag-caption)
  ("table" end-tag-table)
  (("body" "col" "colgroup" "html" "tbody" "td" "tfoot" "th"
           "thead" "tr") end-tag-ignore)
  (default end-tag-other))

(flet ((ignore-end-tag-caption ()
         (not (element-in-scope "caption" "table"))))

  (def :in-caption process-eof ()
    (process-eof token :phase :in-body))

  (def :in-caption process-characters ()
    (process-characters token :phase :in-body))

  (def :in-caption start-tag-table-element (phase)
    (perror :start-tag-table-element-in-caption)
    ;; XXX Have to duplicate logic here to find out if the tag is ignored
    (prog1 (unless (ignore-end-tag-caption)
             token)
      (process-end-tag (implied-tag-token "caption") :phase phase)))

  (def :in-caption start-tag-other ()
    (process-start-tag token :phase :in-body))

  (def :in-caption end-tag-caption (phase inner-html open-elements)
    (cond ((not (ignore-end-tag-caption))
           ;; AT this code is quite similar to endTagTable in "InTable"
           (generate-implied-end-tags)
           (unless (equal (node-name (last-open-element)) "caption")
             (perror :expected-one-end-tag-but-got-another
                     :got-name "caption"
                     :expected-name (node-name (last-open-element))))
           (loop until (equal (node-name (last-open-element)) "caption")
                 do (pop-end open-elements))
           (clear-active-formatting-elements)
           (setf phase :in-table))
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :end-tag-caption-in-caption-inner-html-mode)))
    nil)

  (def :in-caption end-tag-table (phase)
    (perror :end-tag-table-in-caption)
    (prog1 (unless (ignore-end-tag-caption)
             token)
      (process-end-tag (implied-tag-token "caption") :phase phase)))

  (def :in-caption end-tag-ignore ()
    (perror :unexpected-end-tag :name (getf token :name))
    nil)

  (def :in-caption end-tag-other ()
    (process-end-tag token :phase :in-body)))

;; InColumnGroupPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-column
(tagname-dispatch :in-column-group process-start-tag
  ("html" start-tag-html)
  ("col" start-tag-col)
  (default start-tag-other))

(tagname-dispatch :in-column-group process-end-tag
  ("colgroup" end-tag-colgroup)
  ("col" end-tag-col)
  (default end-tag-other))

(flet ((ignore-end-tag-colgroup ()
         (string= (node-name (last-open-element)) "html")))

  (def :in-column-group process-eof (inner-html)
    (cond ((string= (node-name (last-open-element)) "html")
           (assert inner-html)
           nil)
          (t
           (let ((ignore-end-tag (ignore-end-tag-colgroup)))
             (end-tag-colgroup (implied-tag-token "colgroup"))
             (not ignore-end-tag)))))

  (def :in-column-group process-characters ()
    (prog1 (unless (ignore-end-tag-colgroup)
             token)
      (end-tag-colgroup (implied-tag-token "colgroup"))))

  (def :in-column-group start-tag-col (open-elements)
    (insert-element token)
    (pop-end open-elements)
    nil)

  (def :in-column-group start-tag-other ()
    (prog1 (unless (ignore-end-tag-colgroup)
             token)
      (end-tag-colgroup (implied-tag-token "colgroup"))))

  (def :in-column-group end-tag-colgroup (phase open-elements)
    (cond ((ignore-end-tag-colgroup)
           ;; innerHTML case
           (perror :end-tag-colgroup-in-column-group-inner-html-mode))
          (t
           (pop-end open-elements)
           (setf phase :in-table)))
    nil)

  (def :in-column-group end-tag-col ()
    (perror :no-end-tag :name "col")
    nil)

  (def :in-column-group end-tag-other ()
    (prog1 (unless (ignore-end-tag-colgroup)
             token)
      (end-tag-colgroup (implied-tag-token "colgroup")))))

;; InTableBodyPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-table0
(tagname-dispatch :in-table-body process-start-tag
  ("html" start-tag-html)
  ("tr" start-tag-tr)
  (("td" "th") start-tag-table-cell)
  (("caption" "col" "colgroup" "tbody" "tfoot" "thead") start-tag-table-other)
  (default start-tag-other))

(tagname-dispatch :in-table-body process-end-tag
  (("tbody" "tfoot" "thead") end-Tag-Table-Row-Group)
  ("table" end-Tag-Table)
  (("body" "caption" "col" "colgroup" "html" "td" "th" "tr") end-Tag-Ignore)
  (default end-tag-other))

(flet ((clear-stack-to-table-body-context ()
         (loop until (member (node-name (last-open-element))
                             '("tbody" "tfoot" "thead" "html")
                             :test #'string=)
               do
               ;;(perror :unexpected-implied-end-tag-in-table
               ;;        :name (node-name (last-open-element)))
                  (pop-end (slot-value *parser* 'open-elements)))
         (when (string= (node-name (last-open-element)) "html")
           (assert (slot-value *parser* 'inner-html)))))

  (def :in-table-body process-eof ()
    (process-eof token :phase :in-table))

  (def :in-table-body process-space-characters ()
    (process-space-characters token :phase :in-table))

  (def :in-table-body process-characters ()
    (process-characters token :phase :in-table))

  (def :in-table-body start-tag-tr (phase)
    (clear-stack-to-table-body-context)
    (insert-element token)
    (setf phase :in-row)
    nil)

  (def :in-table-body start-tag-table-cell ()
    (perror :unexpected-cell-in-table-body :name (getf token :name))
    (start-tag-tr (implied-tag-token "tr" :start-tag))
    token)

  (def :in-table-body start-tag-table-other (inner-html)
    ;; XXX AT Any ideas on how to share this with endTagTable?
    (cond ((or (element-in-scope "tbody" "table")
               (element-in-scope "thead" "table")
               (element-in-scope "tfoot" "table"))
           (clear-stack-to-table-body-context)
           (end-tag-table-row-group
            (implied-tag-token (node-name (last-open-element))))
           token)
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :start-tag-table-other-in-table-body-inner-html-mode)
           nil)))

  (def :in-table-body start-tag-other ()
    (process-start-tag token :phase :in-table))

  (def :in-table-body end-tag-table-row-group (phase open-elements)
    (cond ((element-in-scope (getf token :name) "table")
           (clear-stack-to-table-body-context)
           (pop-end open-elements)
           (setf phase :in-table))
          (t
           (perror :unexpected-end-tag-in-table-body :name (getf token :name))))
    nil)

  (def :in-table-body end-tag-table (inner-html)
    (cond ((or (element-in-scope "tbody" "table")
               (element-in-scope "thead" "table")
               (element-in-scope "tfoot" "table"))
           (clear-stack-to-table-body-context)
           (end-tag-table-row-group
            (implied-tag-token (node-name (last-open-element))))
           token)
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :end-tag-table-other-in-table-body-inner-html-mode)
           nil)))

  (def :in-table-body end-tag-ignore ()
    (perror :unexpected-end-tag-in-table-body :name (getf token :name))
    nil)

  (def :in-table-body end-tag-other ()
    (process-end-tag token :phase :in-table)))

;; InRowPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-row
(tagname-dispatch :in-row process-start-tag
  ("html" start-tag-html)
  (("td" "th") start-tag-table-cell)
  (("caption" "col" "colgroup" "tbody" "tfoot" "thead" "tr")
   start-tag-table-other)
  (default start-tag-other))

(tagname-dispatch :in-row process-end-tag
  ("tr" end-tag-tr)
  ("table" end-tag-table)
  (("tbody" "tfoot" "thead") end-tag-table-row-group)
  (("body" "caption" "col" "colgroup" "html" "td" "th") end-tag-ignore)
  (default end-tag-other))

;; helper methods (XXX unify this with other table helper methods)
(flet ((clear-stack-to-table-row-context ()
         (loop until (member (node-name (last-open-element))
                             '("tr" "html")
                             :test #'string=)
               do
                  (perror :unexpected-implied-end-tag-in-table-row
                          :name (node-name (last-open-element)))
                  (pop-end (slot-value *parser* 'open-elements))))

       (ignore-end-tag-tr ()
         (not (element-in-scope "tr" "table"))))

  ;; the rest
  (def :in-row process-eof ()
    (process-eof token :phase :in-table)
    nil)

  (def :in-row process-space-characters ()
    (process-space-characters token :phase :in-table))

  (def :in-row process-characters ()
    (process-characters token :phase :in-table))

  (def :in-row start-tag-table-cell (phase active-formatting-elements)
    (clear-stack-to-table-row-context)
    (insert-element token)
    (setf phase :in-cell)
    (push-end :marker active-formatting-elements)
    nil)

  (def :in-row start-tag-table-other ()
    (let ((ignore-end-tag (ignore-end-tag-tr)))
      (end-tag-tr (implied-tag-token "tr"))
      ;; XXX how are we sure it's always ignored in the innerHTML case?
      (unless ignore-end-tag
        token)))

  (def :in-row start-tag-other ()
    (process-start-tag token :phase :in-table))

  (def :in-row end-tag-tr (phase inner-html open-elements)
    (cond ((not (ignore-end-tag-tr))
           (clear-stack-to-table-row-context)
           (pop-end open-elements)
           (setf phase :in-table-body))
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :end-tag-tr-inner-html-mode)))
    nil)

  (def :in-row end-tag-table ()
    (let ((ignore-end-tag (ignore-end-tag-tr)))
      (end-tag-tr (implied-tag-token "tr"))
      ;; Reprocess the current tag if the tr end tag was not ignored
      ;; XXX how are we sure it's always ignored in the innerHTML case?
      (unless ignore-end-tag
        token)))

  (def :in-row end-tag-table-row-group ()
    (cond ((element-in-scope (getf token :name) "table")
           (end-tag-tr (implied-tag-token "tr"))
           token)
          (t
           (perror :end-tag-table-row-group-something-wrong)
           nil)))

  (def :in-row end-tag-ignore ()
    (perror :unexpected-end-tag-in-table-row (getf token :name))
    nil)

  (def :in-row end-tag-other ()
    (process-end-tag token :phase :in-table)))

;; InCellPhase
;; http://www.whatwg.org/specs/web-apps/current-work/#in-cell
(tagname-dispatch :in-cell process-start-tag
  ("html" start-tag-html)
  (("caption" "col" "colgroup" "tbody" "td" "tfoot" "th" "thead" "tr")
   start-tag-table-other)
  (default start-tag-other))

(tagname-dispatch :in-cell process-end-tag
  (("td" "th") end-tag-table-cell)
  (("body" "caption" "col" "colgroup" "html") end-tag-ignore)
  (("table" "tbody" "tfoot" "thead" "tr") end-tag-imply)
  (default end-tag-other))

(flet ((close-cell ()
         (if (element-in-scope "td" "table")
             (end-tag-table-cell (implied-tag-token "td"))
             (if (element-in-scope "th" "table")
                 (end-tag-table-cell (implied-tag-token "th"))))))

  (def :in-cell process-eof ()
    (process-eof token :phase :in-body)
    nil)

  (def :in-cell process-characters ()
    (process-characters token :phase :in-body))

  (def :in-cell start-tag-table-other (inner-html)
    (cond ((or (element-in-scope "td" "table")
               (element-in-scope "th" "table"))
           (close-cell)
           token)
          (t
           ;; innerHTML case
           (assert inner-html)
           (perror :start-tag-table-other-in-inner-html-mode)
           nil)))

  (def :in-cell start-tag-other ()
    (process-start-tag token :phase :in-body))

  (def :in-cell end-tag-table-cell (phase open-elements)
    (cond ((element-in-scope (getf token :name) "table")
           (generate-implied-end-tags (getf token :name))
           (cond ((not (equal (node-name (last-open-element))
                              (getf token :name)))
                  (perror :unexpected-cell-end-tag :name (getf token :name))
                  (loop until (equal (node-name (pop-end open-elements))
                                     (getf token :name))))
                 (t
                  (pop-end open-elements)))
           (clear-active-formatting-elements)
           (setf phase :in-row))
          (t
           (perror :unexpected-end-tag :name (getf token :name))))
    nil)

  (def :in-cell end-tag-ignore ()
    (perror :unexpected-end-tag :name (getf token :name))
    nil)

  (def :in-cell end-tag-imply ()
    (cond ((element-in-scope (getf token :name) "table")
           (close-cell)
           token)
          (t
           ;; sometimes innerHTML case
           (perror :end-tag-imply-sometimes-inner-html-case)
           nil)))

  (def :in-cell end-tag-other ()
    (process-end-tag token :phase :in-body)))

;; InSelectPhase
(tagname-dispatch :in-select process-start-tag
  ("html" start-tag-html)
  ("option" start-tag-option)
  ("optgroup" start-tag-optgroup)
  ("select" start-tag-select)
  (("input" "keygen" "textarea") start-tag-input)
  ("script" start-tag-script)
  (default start-tag-other))

(tagname-dispatch :in-select process-end-tag
  ("option" end-tag-option)
  ("optgroup" end-tag-optgroup)
  ("select" end-tag-select)
  (default end-tag-other))

;; http://www.whatwg.org/specs/web-apps/current-work/#in-select
(def :in-select process-eof (inner-html)
  (if (not (equal (node-name (last-open-element)) "html"))
      (perror :eof-in-select)
      (assert inner-html))
  nil)

(def :in-select process-characters ()
  (unless (equal (getf token :data) (string #\u0000))
    (parser-insert-text (getf token :data)))
  nil)

(def :in-select start-tag-option (open-elements)
  ;; We need to imply </option> if <option> is the current node.
  (when (equal (node-name (last-open-element)) "option")
    (pop-end open-elements))
  (insert-element token)
  nil)

(def :in-select start-tag-optgroup (open-elements)
  (when (equal (node-name (last-open-element)) "option")
    (pop-end open-elements))
  (when (equal (node-name (last-open-element)) "optgroup")
    (pop-end open-elements))
  (insert-element token)
  nil)

(def :in-select start-tag-select ()
  (perror :unexpected-select-in-select)
  (end-tag-select (implied-tag-token "select"))
  nil)

(def :in-select start-tag-input (inner-html)
  (perror :unexpected-input-in-select)
  (cond ((element-in-scope "select" "select")
         (end-tag-select (implied-tag-token "select"))
         token)
        (t
         (assert inner-html)
         nil)))

(def :in-select start-tag-script ()
  (process-start-tag token :phase :in-head))

(def :in-select start-tag-other ()
  (perror :unexpected-start-tag-in-select :name (getf token :name))
  nil)

(def :in-select end-tag-option (open-elements)
  (if (equal (node-name (last-open-element)) "option")
      (pop-end open-elements)
      (perror :unexpected-end-tag-in-select :name (getf token :name)))
  nil)

(def :in-select end-tag-optgroup (open-elements)
  ;; </optgroup> implicitly closes <option>
  (when  (and (equal (node-name (last-open-element)) "option")
              (equal (node-name (elt open-elements
                                     (- (length open-elements) 2)))
                     "optgroup"))
    (pop-end open-elements))
  ;; It also closes </optgroup>
  (if (equal (node-name (last-open-element)) "optgroup")
      (pop-end open-elements)
      ;; But nothing else
      (perror :unexpected-end-tag-in-select :name (getf token :name)))
  nil)

(def :in-select end-tag-select (inner-html open-elements)
  (cond ((element-in-scope "select" "select")
         (loop until (equal (node-name (pop-end open-elements))
                            "select"))
         (reset-insertion-mode))
        (t
         ;; innerHTML case
         (assert inner-html)
         (perror :end-tag-select-in-inner-html-mode)))
  nil)

(def :in-select end-tag-other ()
  (perror :unexpected-end-tag-in-select :name (getf token :name))
  nil)

;; InSelectInTablePhase
(tagname-dispatch :in-select-in-table process-start-tag
  (("caption" "table" "tbody" "tfoot" "thead" "tr" "td" "th") start-tag-table)
  (default start-tag-other))

(tagname-dispatch :in-select-in-table process-end-tag
  (("caption" "table" "tbody" "tfoot" "thead" "tr" "td" "th") end-tag-table)
  (default end-tag-other))

(def :in-select-in-table process-eof ()
  (process-eof token :phase :in-select)
  nil)

(def :in-select-in-table process-characters ()
  (process-characters token :phase :in-select))

(def :in-select-in-table start-tag-table ()
  (perror :unexpected-table-element-start-tag-in-select-in-table :name (getf token :name))
  (end-tag-other (implied-tag-token "select"))
  token)

(def :in-select-in-table start-tag-other ()
  (process-start-tag token :phase :in-select))

(def :in-select-in-table end-tag-table ()
  (perror :unexpected-table-element-end-tag-in-select-in-table :name (getf token :name))
  (cond ((element-in-scope (getf token :name) "table")
         (end-tag-other (implied-tag-token "select"))
         token)
        (t
         nil)))

(def :in-select-in-table end-tag-other ()
  (process-end-tag token :phase :in-select))

;; InForeignContentPhase
(defparameter +breakout-elements+
  '("b" "big" "blockquote" "body" "br"
    "center" "code" "dd" "div" "dl" "dt"
    "em" "embed" "h1" "h2" "h3"
    "h4" "h5" "h6" "head" "hr" "i" "img"
    "li" "listing" "menu" "meta" "nobr"
    "ol" "p" "pre" "ruby" "s"  "small"
    "span" "strong" "strike"  "sub" "sup"
    "table" "tt" "u" "ul" "var"))

(defun adjust-svg-tag-names (token)
  (let ((replacement (cdr
                      (assoc (getf token :name)
                             '(("altglyph" . "altGlyph")
                               ("altglyphdef" . "altGlyphDef")
                               ("altglyphitem" . "altGlyphItem")
                               ("animatecolor" . "animateColor")
                               ("animatemotion" . "animateMotion")
                               ("animatetransform" . "animateTransform")
                               ("clippath" . "clipPath")
                               ("feblend" . "feBlend")
                               ("fecolormatrix" . "feColorMatrix")
                               ("fecomponenttransfer" . "feComponentTransfer")
                               ("fecomposite" . "feComposite")
                               ("feconvolvematrix" . "feConvolveMatrix")
                               ("fediffuselighting" . "feDiffuseLighting")
                               ("fedisplacementmap" . "feDisplacementMap")
                               ("fedistantlight" . "feDistantLight")
                               ("feflood" . "feFlood")
                               ("fefunca" . "feFuncA")
                               ("fefuncb" . "feFuncB")
                               ("fefuncg" . "feFuncG")
                               ("fefuncr" . "feFuncR")
                               ("fegaussianblur" . "feGaussianBlur")
                               ("feimage" . "feImage")
                               ("femerge" . "feMerge")
                               ("femergenode" . "feMergeNode")
                               ("femorphology" . "feMorphology")
                               ("feoffset" . "feOffset")
                               ("fepointlight" . "fePointLight")
                               ("fespecularlighting" . "feSpecularLighting")
                               ("fespotlight" . "feSpotLight")
                               ("fetile" . "feTile")
                               ("feturbulence" . "feTurbulence")
                               ("foreignobject" . "foreignObject")
                               ("glyphref" . "glyphRef")
                               ("lineargradient" . "linearGradient")
                               ("radialgradient" . "radialGradient")
                               ("textpath" . "textPath"))
                             :test #'string=))))
    (when replacement
      (setf (getf token :name) replacement))))

(defparameter +only-space-characters-regexp+
  (cl-ppcre:create-scanner `(:sequence :start-anchor
                                       (:greedy-repetition
                                        0 nil
                                        (:alternation ,@(coerce +space-characters+ 'list)))
                                       :end-anchor)
                           :multi-line-mode t))

(defun only-space-characters-p (string)
  (cl-ppcre:scan +only-space-characters-regexp+ string))

(def :in-foreign-content process-characters (frameset-ok)
  (cond ((equal (getf token :data) (string #\u0000))
         (setf (getf token :data) (string #\uFFFD)))
        ((and frameset-ok
              (not (only-space-characters-p (getf token :data))))
         (setf frameset-ok nil)))
  (process-characters token :phase nil)
  nil)

(def :in-foreign-content process-start-tag (html-namespace open-elements)
  (block nil
    (let ((current-node (last-open-element)))
      (cond ((or (member (getf token :name) +breakout-elements+ :test #'string=)
                 (and (string= (getf token :name) "font")
                      (intersection (mapcar #'car (getf token :data))
                                    '("color" "face" "size")
                                    :test #'string=)))
             (parser-parse-error :unexpected-html-element-in-foreign-content
                                 (getf token :name))
             (loop until (or (is-html-integration-point (last-open-element))
                             (is-math-ml-text-integration-point (last-open-element))
                             (equal (node-namespace (last-open-element))
                                    html-namespace))
                   do (pop-end open-elements))
             (return token))
            (t
             (cond ((equal (node-namespace current-node) (find-namespace "mathml"))
                    (adjust-math-ml-attributes token))
                   ((equal (node-namespace current-node) (find-namespace "svg"))
                    (adjust-svg-tag-names token)
                    (adjust-svg-attributes token)))
             (adjust-foreign-attributes token)
             (setf (getf token :namespace) (node-namespace current-node))
             (insert-element token)
             (when (getf token :self-closing)
               (pop-end open-elements)
               (setf (getf token :self-closing-acknowledged) t)))))
    nil))

(def :in-foreign-content process-end-tag (phase original-phase html-namespace open-elements)
  (let ((new-token)
        (node-index (1- (length open-elements)))
        (node (last-open-element)))
    (unless (string= (node-name node) (getf token :name))
      (parser-parse-error :unexpected-end-tag (getf token :name)))

    (loop
      (when (string= (ascii-upper-2-lower (node-name node)) (getf token :name))
        ;; XXX this isn't in the spec but it seems necessary
        (when (eql phase :in-table-text)
          (flush-characters)
          (setf phase original-phase))
        (loop until (eql (pop-end open-elements) node)
              do (assert open-elements))
        (setf new-token nil)
        (return))
      (decf node-index)
      (setf node (elt open-elements node-index))
      (when (equal (node-namespace node)
                   html-namespace)
        (setf new-token (process-end-tag token :phase phase))
        (return)))
    new-token))

;; AfterBodyPhase
(tagname-dispatch :after-body process-start-tag
  ("html" start-tag-html)
  (default start-tag-other))

(tagname-dispatch :after-body process-end-tag
  ("html" end-tag-html)
  (default end-tag-other))

(def :after-body process-eof ()
  ;; Stop parsing
  nil)

(def :after-body process-comment (open-elements)
  ;; This is needed because data is to be appended to the <html> element
  ;; here and not to whatever is currently open.
  (insert-comment token (first open-elements))
  nil)

(def :after-body process-characters (phase)
  (parser-parse-error :unexpected-char-after-body)
  (setf phase :in-body)
  token)

(def :after-body start-tag-html ()
  (process-start-tag token :phase :in-body))

(def :after-body start-tag-other (phase)
  (parser-parse-error :unexpected-start-tag-after-body
                      `(:name ,(getf token :name)))
  (setf phase :in-body)
  token)

(def :after-body end-tag-html (inner-html phase)
  (if inner-html
      (parser-parse-error :unexpected-end-tag-after-body-innerhtml)
      (setf phase :after-after-body))
  nil)

(def :after-body end-tag-other (phase)
  (parser-parse-error :unexpected-end-tag-after-body
                      `(:name ,(getf token :name)))
  (setf phase :in-body)
  token)

;; InFramesetPhase
(tagname-dispatch :in-frameset process-start-tag
  ("html" start-tag-html)
  ("frameset" start-tag-frameset)
  ("frame" start-tag-frame)
  ("noframes"start-tag-noframes)
  (default start-tag-other))

(tagname-dispatch :in-frameset process-end-tag
  ("frameset" end-tag-frameset)
  (default end-tag-other))


(def :in-frameset process-eof (inner-html)
  (if (string/= (node-name (last-open-element)) "html")
      (parser-parse-error :eof-in-frameset)
      (assert inner-html))
  nil)

(def :in-frameset process-characters ()
  (parser-parse-error :unexpected-char-in-frameset)
  nil)

(def :in-frameset start-tag-frameset ()
  (insert-element token)
  nil)

(def :in-frameset start-tag-frame (open-elements)
  (insert-element token)
  (pop-end open-elements)
  nil)

(def :in-frameset start-tag-noframes ()
  (process-start-tag token :phase :in-body))

(def :in-frameset start-tag-other ()
  (parser-parse-error :unexpected-start-tag-in-frameset
                      `(:name ,(getf token :name)))
  nil)

(def :in-frameset end-tag-frameset (phase inner-html open-elements)
  (if (string= (node-name (last-open-element)) "html")
      ;; innerHTML case
      (parser-parse-error :unexpected-frameset-in-frameset-innerhtml)
      (pop-end open-elements))

  (when (and (not inner-html)
             (string/= (node-name (last-open-element)) "frameset"))
    ;; If we're not in innerHTML mode and the the current node is not a
    ;; "frameset" element (anymore) then switch.
    (setf phase :after-frameset))
  nil)

(def :in-frameset end-tag-other ()
  (parser-parse-error :unexpected-end-tag-in-frameset
                      `(:name ,(getf token :name)))
  nil)

;; AfterFramesetPhase
(tagname-dispatch :after-frameset process-start-tag
  ("html" start-tag-html)
  ("noframes" start-tag-noframes)
  (default start-tag-other))

(tagname-dispatch :after-frameset process-end-tag
  ("html" end-tag-html)
  (default end-tag-other))

(def :after-frameset process-eof ()
  ;; Stop parsing
  nil)

(def :after-frameset process-characters ()
  (parser-parse-error :unexpected-char-after-frameset)
  nil)

(def :after-frameset start-tag-noframes ()
  (process-start-tag token :phase :in-head))

(def :after-frameset start-tag-other ()
  (parser-parse-error :unexpected-start-tag-after-frameset
                      `(:name ,(getf token :name)))
  nil)

(def :after-frameset end-tag-html (phase)
  (setf phase :after-after-frameset)
  nil)

(def :after-frameset end-tag-other ()
  (parser-parse-error :unexpected-end-tag-after-frameset
                      `(:name ,(getf token :name)))
  nil)

;; AfterAfterBodyPhase
(tagname-dispatch :after-after-body process-start-tag
  ("html" start-tag-html)
  (default start-tag-other))

(def :after-after-body process-eof ()
  nil)

(def :after-after-body process-comment ()
  (insert-comment token (document*))
  nil)

(def :after-after-body process-space-characters ()
  (process-space-characters token :phase :in-body))

(def :after-after-body process-characters (phase)
  (parser-parse-error :expected-eof-but-got-char)
  (setf phase :in-body)
  token)

(def :after-after-body start-tag-html ()
  (process-start-tag token :phase :in-body))

(def :after-after-body start-tag-other (phase)
  (parser-parse-error :expected-eof-but-got-start-tag
                      `(:name (getf token :name)))
  (setf phase :in-body)
  token)

(def :after-after-body process-end-tag (phase)
  (parser-parse-error :expected-eof-but-got-end-tag
                      `(:name (getf token :name)))
  (setf phase :in-body)
  token)

;; AfterAfterFramesetPhase
(tagname-dispatch :after-after-frameset process-start-tag
  ("html" start-tag-html)
  ("noframes" start-tag-noframes)
  (default start-tag-other))

(def :after-after-frameset process-eof ()
  nil)

(def :after-after-frameset process-comment ()
  (insert-comment token (document*))
  nil)

(def :after-after-frameset process-space-characters ()
  (process-space-characters token :phase :in-body))

(def :after-after-frameset process-characters ()
  (parser-parse-error :expected-eof-but-got-char)
  nil)

(def :after-after-frameset start-tag-html ()
  (process-start-tag token :phase :in-body))

(def :after-after-frameset start-tag-noframes ()
  (process-start-tag token :phase :in-head))

(def :after-after-frameset start-tag-other ()
  (parser-parse-error :expected-eof-but-got-start-tag
                      `(:name (getf token :name)))
  nil)

(def :after-after-frameset process-end-tag ()
  (parser-parse-error :expected-eof-but-got-end-tag
                      `(:name (getf token :name)))
  nil)

;;; toxml
(defun xml-escape-name (name)
  "Escapes a node name (element, attribute, doctype) by replacing any
character not valid in XML name by Uxxxxxx, where x is the code point
as six hex digits. This encoding is reversable, since the HTML parser
down cases all characters in names.

See: https://www.w3.org/TR/html5/syntax.html#coercing-an-html-dom-into-an-infoset"
  (if (and (xml-name-start-char-p (char name 0))
           (every #'xml-name-char-p name))
      name
      (with-output-to-string (out)
        (loop for first = t then nil
              for c across name do
                 (if (if first
                         (xml-name-start-char-p c)
                         (xml-name-char-p c))
                     (princ c out)
                     (format out "U~:@(~6,'0X~)" (char-code c)))))))

(defun xml-unescape-name (name)
  "Reverert escaping done by xml-unescape-name."
  (cl-ppcre:regex-replace-all
   "U[0-9A-F]{6}"
   name
   (lambda (u)
     (string (code-char (parse-integer u :start 1 :radix 16))))
   :simple-calls t))

(defun xml-name-start-char-p (c)
  (or (char<= #\a c #\z)
      (char= #\_ c)
      (char<= #\A c #\Z)
      (char<= (code-char #xC0) c (code-char #xD6))
      (char<= (code-char #xD8) c (code-char #xF6))
      (char<= (code-char #xF8) c (code-char #x2FF))
      (char<= (code-char #x370) c (code-char #x37D))
      (char<= (code-char #x37F) c (code-char #x1FFF))
      (char<= (code-char #x200C) c (code-char #x200D))
      (char<= (code-char #x2070) c (code-char #x218F))
      (char<= (code-char #x2C00) c (code-char #x2FEF))
      (char<= (code-char #x3001) c (code-char #xD7FF))
      (char<= (code-char #xF900) c (code-char #xFDCF))
      (char<= (code-char #xFDF0) c (code-char #xFFFD))
      (char<= (code-char #x10000) c (code-char #xEFFFF))))

(defun xml-name-char-p (c)
  (or (xml-name-start-char-p c)
      (char= #\- c)
      (char= #\. c)
      (char<= #\0 c #\9)
      (char= (code-char #xB7) c)
      (char<= (code-char #x0300) c (code-char #x036F))
      (char<= (code-char #x203F) c (code-char #x2040))))

;;; XML DOM
(defmethod transform-html5-dom ((to-type (eql :xml)) node
                                &key namespace comments)
  "Convert a node into an DAT/XML-compatible tree of conses, starting
at. If the node is a document-fragement a list of XML trees is returned."
  (labels ((node-to-xml (node parent-ns xlink-defined)
             (ecase (node-type node)
               (:document
                (let (root)
                  (element-map-children (lambda (n)
                                          (when (string= (node-name n) "html")
                                            (setf root n)))
                                        node)
                  (assert root)
                  (node-to-xml root parent-ns xlink-defined)))
               (:document-fragment
                (let (xml-nodes)
                  (element-map-children (lambda (node)
                                          (push (node-to-xml node parent-ns xlink-defined)
                                                xml-nodes))
                                        node)
                  (nreverse xml-nodes)))
               (:element
                (let (attrs children)
                  (element-map-attributes (lambda (name node-namespace value)
                                            (when (and namespace
                                                       (not xlink-defined)
                                                       (equal node-namespace (find-namespace "xlink")))
                                              (push '#.(list "xmlns:xlink" (find-namespace "xlink")) attrs)
                                              (setf xlink-defined t))
                                            (push (list (if node-namespace
                                                            name
                                                            (xml-escape-name name))
                                                        value)
                                                  attrs))
                                          node)
                  (element-map-children (lambda (c)
                                          (push c children))
                                        node)

                  (apply #'list
                         (if (and namespace
                                  (not (equal parent-ns (node-namespace node))))
                             (cons (node-name node) (node-namespace node))
                             (xml-escape-name (node-name node)))
                         attrs
                         (mapcar (lambda (c)
                                   (node-to-xml c (node-namespace node) xlink-defined))
                                 (nreverse children)))))
               (:text
                (node-value node))
               (:comment
                (when comments
                  (list :comment nil (node-value node)))))))
    (node-to-xml node nil nil)))

(defmethod transform-html5-dom ((to-type (eql :xml-ns)) node &key)
  (transform-html5-dom :xml node :namespace t))

;;; DAT proto
(defmethod deserialize (from (fmt (eql :html)) &key encoding strictp container dom)
  (declare (ignore fmt))
  (parse-html5 from :encoding encoding :strictp strictp :container container :dom dom))
