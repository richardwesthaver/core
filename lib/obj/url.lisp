;;; obj/url.lisp --- Universal Resource Locators

;; Some conveniences for URLs.

;;; Commentary:

;; This package mostly just implements the bare-minimum provided by QURI:
;; URL-ENCODE and URL-DECODE

;;; Code:
(in-package :obj/url)

(define-condition uri-unexpected-end (uri-error simple-error)
  ((state :initarg :state :initform nil))
  (:report (lambda (c s)
             (format s "Parsing ended unexpectedly~:[~;~:* at ~A~]"
                     (slot-value c 'state)))))

(define-condition no-next-state (uri-error simple-error) ())

(define-condition url-decoding-error (uri-error) ())

;;; String Utils
(defun starts-with-scheme-p (string)
  "Check whether the string STRING represents a URL which starts with
a scheme, i.e. something like 'https://' or 'mailto:'."
  (loop with scheme-char-seen-p = nil
        for c across string
        when (or (char-not-greaterp #\a c #\z)
                 (digit-char-p c)
                 (member c '(#\+ #\- #\.) :test #'char=))
        do (setq scheme-char-seen-p t)
        else return (and scheme-char-seen-p
                         (char= c #\:))))

;;; Array Utils
(defmacro with-array-parsing ((elem p seq &optional (start 0) end key) &body body)
  `(let (,elem)
     (%with-array-parsing (,elem ,p ,seq ,start ,end ,key) ,@body)))

(defmacro %with-array-parsing ((elem p seq &optional (start 0) end key) &body body)
  (with-gensyms (g-end no-next-state last key-fn)
    (let ((eof-exists nil))
      `(let (,@(and key `((,key-fn ,key)))
             (,p ,start)
             (,g-end (locally (declare #+sbcl (muffle-conditions compiler-note))
                       (or ,end (length ,seq)))))
         (declare (ignorable ,p ,g-end))
         ,@(loop for (exp . rest) on body
                 while (and (listp exp) (eq (car exp) 'declare))
                 collect exp
                 do (setq body rest))
         (macrolet ((goto (tag &optional (amount 1))
                      `(locally (declare (optimize (speed 3) (safety 0)))
                         (incf ,',p ,amount)
                         ,@(if (eql amount 0)
                               ()
                               `((when (= ,',p ,',g-end)
                                   (go :eof))
                                 (setq ,',elem
                                       ,',(if key
                                              `(if ,key-fn
                                                   (funcall ,key-fn (aref ,seq ,p))
                                                   (aref ,seq ,p))
                                              `(aref ,seq ,p)))))
                         (go ,tag))))
           (tagbody
              (when (= ,p ,g-end)
                (go :eof))
              (locally (declare (optimize (speed 3) (safety 0)))
                (setq ,elem ,@(if key
                                  `((if ,key-fn
                                        (funcall ,key-fn (aref ,seq ,p))
                                        (aref ,seq ,p)))
                                  `((aref ,seq ,p)))))
              ,@(loop for (tagpart . rest) on body
                      for (tag . part) = tagpart
                      if (eq tag :eof)
                      append (progn
                               (setf eof-exists t)
                               `(,@tagpart
                                 (go ,last)))
                      else
                      append
                         (list tag
                               `(macrolet ((redo (&optional (amount 1))
                                             `(goto ,',tag ,amount))
                                           (gonext (&optional (amount 1))
                                             `(goto ,',(or (caar rest) no-next-state)
                                                    ,amount)))
                                  ,@part
                                  (error 'uri-unexpected-end :state ',tag))))

              ,no-next-state
              (error 'no-next-state)

              ,@(if eof-exists
                    ()
                    '(:eof))

              ,last))))))

;;; Encode
(definline url-encode-params (params &key (external-format *default-external-format*)
                                     space-to-plus
                                     (percent-encode t))
  (declare (optimize (speed 3)))
  (check-type params list)
  (flet ((maybe-encode (string)
           (if percent-encode
               (url-encode string
                           :external-format external-format
                           :space-to-plus space-to-plus)
               string)))
    (with-output-to-string (s)
      (loop for ((field . value) . rest) on params do
               (write-string (maybe-encode field) s)
               (when value
                 (write-char #\= s)
                 (check-type value (or string number octet-vector))
                 (write-string (maybe-encode
                                (if (numberp value)
                                    (with-standard-io-syntax
                                      (write-to-string value))
                                    value))
                               s))
               (when rest
                 (write-char #\& s))))))


(declaim ((simple-array character (16)) *hexdigit-char*))
(defvar *hexdigit-char*
  (let ((ary (make-array 16 :element-type 'character)))
    (loop for char across "0123456789ABCDEF"
          for i from 0
          do (setf (aref ary i) char))
    ary))

(defun int-to-hexchar (byte)
  (declare ((unsigned-byte 8) byte)
           (optimize (speed 3) (safety 0)))
  (let ((res (make-string 2)))
    (multiple-value-bind (quotient remainder)
        (floor byte 16)
      (setf (aref res 0) (aref *hexdigit-char* quotient)
            (aref res 1) (aref *hexdigit-char* remainder)))
    res))

(defun unreservedp (byte)
  (declare ((unsigned-byte 8) byte)
           (optimize (speed 3) (safety 0)))
  (or (<= (char-code #\A) byte (char-code #\Z))
      (<= (char-code #\a) byte (char-code #\z))
      (<= (char-code #\0) byte (char-code #\9))
      #.`(or ,@(loop for char across "-._~"
                     collect `(= byte ,(char-code char))))))

(declaim ((simple-array string (97)) %byte-to-string))
(defvar %byte-to-string
  (let ((ary (make-array 97 :element-type 'string :initial-element "")))
    (loop for i from 0 to 96
          unless (unreservedp i)
          do (setf (aref ary i) (int-to-hexchar i)))
    ary))

(defun url-encode (data &key (external-format *default-external-format*)
                             (start 0)
                             end
                             space-to-plus)
  (declare ((or string octet-vector) data)
           (integer start)
           (optimize (speed 3) (safety 2)))
  (let* ((octets (if (stringp data)
                     (string-to-octets data :external-format external-format :start start :end end)
                     data))
         (res (make-array (* (length octets) 3) :element-type 'character :fill-pointer t))
         (i 0))
    (declare (octet-vector octets)
             (string res)
             (integer i))
    (loop for byte of-type (unsigned-byte 8) across octets do
             (cond
               ((and space-to-plus
                     (= byte #.(char-code #\Space)))
                (setf (aref res i) #\+)
                (incf i))
               ((< byte #.(char-code #\a))
                (locally (declare (optimize (speed 3) (safety 0)))
                  (let ((converted (aref %byte-to-string byte)))
                    (if (zerop (length converted))
                        (progn
                          (setf (aref res i) (code-char byte))
                          (incf i))
                        (progn
                          (setf (aref res i) #\%)
                          (incf i)
                          (replace res converted :start1 i)
                          (incf i 2))))))
               ((unreservedp byte)
                (setf (aref res i) (code-char byte))
                (incf i))
               (t
                (setf (aref res i) #\%)
                (incf i)
                (replace res (int-to-hexchar byte) :start1 i)
                (incf i 2))))
    (setf (fill-pointer res) i)
    res))

;;; Decode
(definline url-decode-params (data &key (delimiter #\&)
                                   (external-format *default-external-format*)
                                   (start 0)
                                   end
                                   lenient
                                   (percent-decode t))
  (declare ((or string octet-vector) data)
           (integer start)
           (character delimiter)
           (optimize (speed 3) (safety 2)))
  (let ((end (or end (length data)))
        (start-mark nil)
        (=-mark nil))
    (declare (integer end))
    (std/macs:collecting
      (labels ((maybe-decode (string external-format start end)
                 (if percent-decode
                     (url-decode string
                                 :external-format external-format
                                 :start start
                                 :end end
                                 :lenient lenient)
                     (subseq string start end)))
               (collect-pair (p)
                 (tagbody
                    (handler-bind ((url-decoding-error
                                     (lambda (error)
                                       (declare (ignore error))
                                       (when lenient
                                         (go continue)))))
                      (std/macs::collect
                          (cons (maybe-decode data external-format start-mark =-mark)
                                (maybe-decode data external-format (1+ =-mark) p))))
                  continue)
                 (setq start-mark nil
                       =-mark nil))
               (collect-field (p)
                 (tagbody
                    (handler-bind ((url-decoding-error
                                     (lambda (error)
                                       (declare (ignore error))
                                       (when lenient
                                         (go continue)))))
                      (std/macs::collect
                          (cons (maybe-decode data external-format start-mark p)
                                nil)))
                  continue)
                 (setq start-mark nil)))
        (with-array-parsing (char p data start end (and (not (stringp data)) #'code-char))
          (start
           (setq start-mark p)
           (if lenient
               (cond
                 ((char= char #\=)
                  (setq =-mark p)
                  (goto parsing-value))
                 ((char= char delimiter)
                  (redo)))
               (when (or (char= char #\=)
                         (char= char delimiter))
                 (error 'uri-malformed-urlencoded-string)))
           (gonext))
          (parsing-field
           (cond
             ((char= char #\=)
              (setq =-mark p)
              (gonext))
             ((char= char delimiter)
              ;; field only
              (collect-field p)
              (goto start)))
           (redo))
          (parsing-value
           (cond
             ((char= char #\=)
              (unless lenient
                (error 'uri-malformed-urlencoded-string)))
             ((char= char delimiter)
              (collect-pair p)
              (goto start)))
           (redo))
          (:eof
           (cond
             (=-mark (collect-pair p))
             (start-mark (collect-field p)))))))))

(defun url-decode (data &key (external-format *default-external-format*)
                             (start 0)
                             end
                             lenient)
  (declare ((or string octet-vector) data)
           (integer start)
           (optimize (speed 3) (safety 2)))
  (let* ((end (or end (length data)))
         (buffer (make-array (- end start)
                             :element-type '(unsigned-byte 8)))
         (i 0)
         parsing-encoded-part)
    (declare (integer end i)
             (octet-vector buffer))
    (flet ((write-to-buffer (byte)
             (declare (optimize (speed 3) (safety 0)))
             (setf (aref buffer i) byte)
             (incf i)))
      (with-array-parsing (char p data start end (and (not (stringp data)) #'code-char))
        (parsing
         (cond
           ((char= char #\%)
            (gonext))
           ((char= char #\+)
            (write-to-buffer #.(char-code #\Space))
            (redo))
           (t
            (write-to-buffer (char-code char))
            (redo))))
        (parsing-encoded-part
         (setq parsing-encoded-part char)
         (gonext))
        (parsing-encoded-part-second
         (handler-bind ((url-decoding-error
                          (lambda (error)
                            (declare (ignore error))
                            (when lenient
                              (write-to-buffer #.(char-code #\%))
                              (write-to-buffer (char-code parsing-encoded-part))
                              (write-to-buffer (char-code char))
                              (setq parsing-encoded-part nil)
                              (goto parsing)))))
           (write-to-buffer
            (+ (* 16 (hexchar-to-int parsing-encoded-part))
               (hexchar-to-int char))))
         (setq parsing-encoded-part nil)
         (goto parsing))
        (:eof
         (when parsing-encoded-part
           (error 'url-decoding-error)))))
    ;;  TODO 2025-06-13: handle leniency
    (octets-to-string buffer :end i :external-format external-format)))

;;; Serde
(defmethod serialize (self (fmt (eql :url)) &rest args)
  (apply 'url-encode self args))

(defmethod deserialize (self (fmt (eql :url)) &rest args)
  (apply 'url-decode self args))

;; (deserialize (serialize "foo://test" :url) :url)

;;; url-rewrite
;; rewrite URLs in an HTML document streamed from *STANDARD-INPUT*.
(defvar *url-rewrite-tags*
  '(("a" . "href")
    ("area" . "href")
    ("frame" . "src")
    ("img" . "src")
    ("input" . "src")
    ("form" . "action")
    ("iframe" . "src"))
  "The tag/attribute combinations where URL-rewriting should happen.")

(defvar *url-rewrite-fill-tags*
  '(("form" . "action"))
  "The tag/attribute combinations where URL-rewriting should
optionally add an attribute.")

(defun add-get-param-to-url (url name value)
  "URL is assumed to be a http URL. The pair of NAME and VALUE will be
added as a GET parameter to this URL. Assumes that there's no other
parameter of the same name. Only checks if #\? is part of the string
to decide how to attach the new parameter to the end of the string."
  ;; possible bug: doesn't check for #\? which is written as, say,
  ;; "&x3f;" - also, is there any other way a question mark could be a
  ;; legitimate part of a URL?
  (concatenate 'string
               url
               (if (find #\? url :test #'char=)
                 "&amp;"
                 "?")
               name
               "="
               (url-encode value)))

(definline peek-char* ()
  "PEEK-CHAR with input stream bound to *STANDARD-INPUT* and returning
NIL on EOF."
  (peek-char nil nil nil))

(definline whitespacep (c)
  "Checks whether C is a whitespace character."
  (find c '(#\Space #\Tab #\Newline #\Linefeed #\Return #\Page)))

(definline letterp (c)
  "Checks whether C is a character between A and Z
\(case-insensitive)."
  (and (characterp c)
       (or (char<= #\a c #\z)
           (char<= #\A c #\Z))))

(definline name-char-p (c)
  "Checks whether C is a name constituent character in the sense of
HTML."
  (and (characterp c)
       (or (letterp c)
           (digit-char-p c)
           (char= c #\-)
           (char= c #\.))))

(defun comment-start-p ()
  "Checks whether *STANDARD-OUTPUT* currently 'looks at' the string
\"--\".  Will move the position within the stream by one unless the
first characters it sees is not a hyphen."
  (unless (eql (peek-char*) #\-)
    ;; if the first character isn't #\- we can return immediately
    (return-from comment-start-p nil))
  ;; otherwise read the #\- so we can check the next character
  (read-char)
  (eql (peek-char*) #\-))

(defun read-while (predicate &key (skip t) (write-through t))
  "Reads characters from *STANDARD-INPUT* while PREDICATE returns a
true value for each character.  Returns the string which was read
unless SKIP is true.  Writes all characters read to *STANDARD-OUTPUT*
if WRITE-THROUGH is true.  On EOF the string read so far is returned."
  (let ((collector (or skip
                       (make-array 0
                                   :element-type 'character
                                   :fill-pointer t
                                   :adjustable t))))
    (handler-case
      (loop while (funcall predicate (peek-char)) do
            (let ((char (read-char)))
              (when write-through
                (write-char char))
              (unless skip
                (vector-push-extend char collector)))
            finally (return (and (not skip) collector)))
      (end-of-file ()
        (and (not skip) collector)))))

(defvar *url-rewrite-string-hash*
  (make-hash-table :test #'equal)
  "Hash tables used internally by URL::READ-UNTIL to cache offset arrays.")

(defun read-until (string &key (skip t) (write-through t))
  "Reads characters from *STANDARD-INPUT* up to and including STRING.
Returns the string which was read \(excluding STRING) unless SKIP is
true.  Writes all characters read to *STANDARD-OUTPUT* if
WRITE-THROUGH is true.  On EOF the string read so far is returned."
  (let* ((length (length string))
         (offsets
           ;; we first check whether some substring which starts
           ;; STRING can be found again later in STRING - this is
           ;; necessary because we only peek one character ahead
           (cond ((gethash string *url-rewrite-string-hash*))
                 (t (setf (gethash string *url-rewrite-string-hash*)
                            ;; the resulting array of offsets is
                            ;; cached in *FIND-STRING-HASH* so we can
                            ;; use it again in case READ-UNTIL is
                            ;; called with the same STRING argument
                            (loop with offsets = (make-array length
                                                             :initial-element nil)
                                  for i from 1 below length
                                  ;; check if STRING starting from 0
                                  ;; has something in common with
                                  ;; STRING starting from I
                                  for mismatch = (mismatch string string
                                                           :start1 i :test #'char=)
                                  when (> mismatch i)
                                  ;; if this is the case remember the
                                  ;; length of the match plus the
                                  ;; character which must follow in
                                  ;; OFFSETS
                                  do (push (cons (char string (- mismatch i))
                                                 (1+ (- mismatch i)))
                                           (svref offsets i))
                                  finally (return offsets))))))
         (collector (or skip
                        (make-array 0
                                    :element-type 'character
                                    :fill-pointer t
                                    :adjustable t))))
    (handler-case
      (loop for i = 0 then (cond (match (1+ i))
                                 ;; if there is an offset (see above)
                                 ;; we don't have to start from the
                                 ;; beginning of STRING
                                 ((cdr (assoc c (svref offsets i))))
                                 (t 0))
            for c = (peek-char)
            for match = (char= c (char string i))
            while (or (not match) (< (1+ i) length)) do
            (cond (skip (read-char))
                  (t (vector-push-extend (read-char) collector)))
            when write-through do
            (write-char c)
            finally (if write-through
                      (write-char (read-char))
                      (read-char))
            (unless skip
              ;; decrement the fill pointer because collector now also
              ;; contains STRING itself
              (decf (fill-pointer collector) (1- length)))
            (return (and (not skip) collector)))
      (end-of-file ()
        (and (not skip) collector)))))

(definline skip-whitespace (&key (skip t) (write-through t))
  "Read characters from *STANDARD-INPUT* as long as they are
whitespace. Returns the string which was read unless SKIP is true. On
EOF the string read so far is returned. Writes all characters read to
*STANDARD-OUTPUT* if WRITE-THROUGH is true."
  (read-while #'whitespace-p
              :skip skip
              :write-through write-through))

(definline read-delimited-string (&key (skip t) (write-through t))
  "Reads and returns as its first value a string from
*STANDARD-INPUT*. The string is either delimited by ' or \" in which
case the delimiters aren't part of the string but the second return
value is the delimiter character or it is assumed to extend to the
next character which is not a name constituent \(see NAME-CHAR-P). On
EOF the string read so far is returned. If SKIP is true NIL is
returned. Writes all characters read to *STANDARD-OUTPUT* if
WRITE-THROUGH is true."
  ;; note that this function has no means to signal to the caller
  ;; that it encountered EOF before the closing delimiter was read,
  ;; i.e. "'foo' bar='baz'" and "'foo" yield the same result, namely
  ;; the values "foo" and #\'
  (handler-case
    (let* ((peek-char (peek-char))
           (delimiter (find peek-char '(#\' #\"))))
      (when delimiter
        (read-char)
        (when write-through
          (write-char delimiter)))
      (multiple-value-prog1
        (values
         (read-while (if delimiter
                       (lambda (c) (char/= c delimiter))
                       (lambda (c) (name-char-p c)))
                     :skip skip
                     :write-through write-through)
         delimiter)
        (when delimiter
          (read-char)
          (when write-through
            (write-char delimiter)))))
    (end-of-file ()
      ;; this can only happen if the very first PEEK-CHAR fails,
      ;; otherwise EOF is handled by READ-WHILE
      nil)))

(definline read-name (&key (skip t) (write-through t))
  "Read characters from *STANDARD-INPUT* as long as they are name
constituents. Returns the string which was read unless SKIP is
true. On EOF the string read so far is returned. Writes all characters
read to *STANDARD-OUTPUT* if WRITE-THROUGH is true."
  (read-while #'name-char-p :skip skip :write-through write-through))

(defun read-attribute (&key (skip t) (write-through t))
  "Read characters from *STANDARD-INPUT* assuming that they constitue
a SGML-style attribute/value pair. Returns three values - the name of
the attribute, its value, and the whole string which was read. On EOF
the string(s) read so far is/are returned. If SKIP is true NIL is
returned. Writes all characters read to *STANDARD-OUTPUT* if
WRITE-THROUGH is true."
  (let* ((name (read-name :skip skip
                          :write-through write-through))
         (whitespace1 (skip-whitespace :skip skip
                                       :write-through write-through)))
    (cond ((eql (peek-char*) #\=)
            (read-char)
            (when write-through
              (write-char #\=))
            (let ((whitespace2 (skip-whitespace :skip skip :write-through write-through)))
              (multiple-value-bind (value delimiter)
                  (read-delimited-string :skip skip :write-through write-through)
                (let ((delimiter-string (if delimiter (string delimiter) "")))
                  (if skip
                    nil
                    (values name value
                            (concatenate 'string
                                         name whitespace1 "=" whitespace2
                                         delimiter-string value delimiter-string)))))))
          (t (if skip
               nil
               (values name nil
                       (concatenate 'string name whitespace1)))))))

(defun skip-comment ()
  "Skip SGML comment from *STANDARD-INPUT*, i.e. a string enclosed in
'--' on both sides. Returns no values. Writes all characters read to
*STANDARD-OUTPUT*. This function assumes \(without checking) that the
current position of *STANDARD-INPUT* is at the beginning of a comment,
after the first hyphen - see COMMENT-START-P."
  (read-char)
  (write-string "--")
  (read-until "--")
  (values))

(defun rewrite-urls (rewrite-fn &optional (test-fn (complement #'starts-with-scheme-p)))
  "Reads an \(X)HTML document from *STANDARD-INPUT* and writes it back
to *STANDARD-OUTPUT*. Any attribute value which is in one of the
positions denoted by *URL-REWRITE-TAGS* is rewritten by REWRITE-FN if
it passes the test denoted by the optional function TEST-FN which
defaults to the complement of STARTS-WITH-SCHEME-P.

This function aims to yield correct results for correct \(X)HTML input
and it also tries hard to never signal an error although it may warn
if it encounters syntax errors. It will NOT detect any possible error
nor is there any warranty that it will work correctly with faulty
input."
  (loop
    ;; read (and write back) until we see a #\< which is a candidate
    ;; for a tag or a markup declaration
    (read-until "<")
    ;; get next char without reading it
    (let ((peek-char (peek-char*)))
      (cond ((null peek-char)
              ;; stop if EOF
              (return-from rewrite-urls))
            ((char= peek-char #\!)
              ;; we've seen "<!" so this might be a markup declaration
              ;; - first write #\! back
              (write-char (read-char))
              ;; peek at next char
              (let ((peek-char (peek-char*)))
                (cond ((null peek-char)
                        ;; stop if EOF
                        (return-from rewrite-urls))
                      ((eql peek-char #\>)
                        ;; "<!>" is nothing special, just write the
                        ;; char and go back to the start of the loop
                        (write-char (read-char)))
                      ((letterp peek-char)
                        ;; a letter, so this should be something like
                        ;; <!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2
                        ;; Final//EN"> - we just check for names and
                        ;; delimited strings separated by whitespace
                        ;; until we see the next #\>
                        (read-name)
                        (skip-whitespace)
                        (block parameter-loop
                          (loop
                            (let ((peek-char (peek-char*)))
                              (cond ((null peek-char)
                                      ;; stop if EOF
                                      (warn "EOF in markup declaration")
                                      (return-from rewrite-urls))
                                    ((char= peek-char #\>)
                                      ;; a #\> ends the markup
                                      ;; declaration - write it back
                                      ;; and exit the loop
                                      (write-char (read-char))
                                      (return-from parameter-loop))
                                    ((or (letterp peek-char)
                                         (find peek-char '(#\' #\") :test #'char=))
                                      ;; a delimiter or a letter, so
                                      ;; we expect a delimited string
                                      (read-delimited-string)
                                      (skip-whitespace))
                                    ((comment-start-p)
                                      ;; a comment - skip it and write it back
                                      (skip-comment))
                                    (t
                                      ;; something else - this is an error
                                      ;; so we warn and exit the loop
                                      (warn "Unexpected character ~S in markup declaration"
                                            peek-char)
                                      (return-from parameter-loop)))))))
                      ((comment-start-p)
                        ;; we've seen "<!--" so this starts a comment declaration
                        ;; - we'll read comments which are possibly separated
                        ;; by whitespace
                        (block comment-loop
                          (loop
                            (skip-comment)
                            (skip-whitespace)
                            (let ((peek-char (peek-char*)))
                              (cond ((null peek-char)
                                      ;; stop if EOF
                                      (warn "EOF in comment declaration")
                                      (return-from rewrite-urls))
                                    ((char= peek-char #\>)
                                      ;; a #\> ends the comment
                                      ;; declaration - write it back
                                      ;; and exit the loop
                                      (write-char (read-char))
                                      (return-from comment-loop))
                                    ;; a comment - do nothing
                                    ((comment-start-p))
                                    (t
                                      ;; something else - this is an error
                                      ;; so we warn and exit the loop
                                      (warn "Unexpected character ~S in comment declaration"
                                            peek-char)
                                      (return-from comment-loop)))))))
                      (t
                        ;; neither markup declaration nor comment declaration,
                        ;; so this was just "<!"
                        (write-char (read-char))))))
            ((char= peek-char #\/)
              (write-char (read-char))
              (let ((peek-char (peek-char*)))
                (cond ((null peek-char)
                        ;; stop if EOF
                        (warn "EOF in end-tag")
                        (return-from rewrite-urls))
                      ((letterp peek-char)
                        ;; a letter, so this is supposed to start a name -
                        ;; read it and skip whitespace following it
                        (let ((name (read-name :skip nil)))
                          (skip-whitespace)
                          (let ((peek-char (peek-char*)))
                            (cond ((null peek-char)
                                    ;; stop if EOF
                                    (warn "EOF after </~A" name)
                                    (return-from rewrite-urls))
                                  ((char/= (peek-char*) #\>)
                                    ;; we expect to see #\> here - if not
                                    ;; we warn but do nothing else
                                    (warn "Expected #\> after </~A" name))
                                  (t
                                    ;; end of end tag, just consume the #\>
                                    (write-char (read-char)))))))
                      (t
                        ;; not a letter, so this is an error -
                        ;; we warn and ignore this
                        (warn "Unexpected character ~S after </"
                              peek-char)))))
            ((letterp peek-char)
              ;; a letter so we expect a start tag, possibly followed by
              ;; attributes - first read name, check if it's mentioned
              ;; in *URL-REWRITE-TAGS*, and find the name of the
              ;; corresponding attribute
              (let* ((name (read-name :skip nil))
                     (rewrite-attribute (and name
                                             (cdr (assoc name *url-rewrite-tags*
                                                         :test #'string-equal))))
                     attribute-found-p)
                (flet ((maybe-write-attribute (&optional value
                                                         (rewrite-attribute
                                                          (and (not attribute-found-p)
                                                               (cdr (assoc name
                                                                           *url-rewrite-fill-tags*
                                                                           :test #'string-equal)))))
                         ;; write the name of the attribute
                         ;; REWRITE-ATTRIBUTE and its (rewritten)
                         ;; value VALUE to *STANDARD-OUTPUT* if DO-IT
                         ;; is true - the default value for DO-IT
                         ;; means to only write the attribute if it
                         ;; has to be added
                         (when rewrite-attribute
                           (unless attribute-found-p
                             (write-char #\Space))
                           (write-string rewrite-attribute)
                           (write-char #\=)
                           (let ((delimiter (if (find #\' value :test #'char=)
                                              #\" #\')))
                             (write-char delimiter)
                             (write-string (funcall rewrite-fn value))
                             (write-char delimiter)))))
                  (skip-whitespace)
                  (block attribute-loop
                    (loop
                      (let ((peek-char (peek-char*)))
                        (cond ((null peek-char)
                                ;; stop if EOF
                                (warn "EOF before ~A tag was closed" name)
                                (return-from rewrite-urls))
                              ((eql peek-char #\>)
                                ;; end of tag - exit attribute loop
                                (maybe-write-attribute)
                                (write-char (read-char))
                                (return-from attribute-loop))
                              ((eql peek-char #\/)
                                ;; we've seen #\/, so this might be the XHTML way
                                ;; to end a stand-alone tag
                                (write-char (read-char))
                                (cond ((eql (peek-char*) #\>)
                                        ;; yes, it is - exit this loop
                                        (maybe-write-attribute)
                                        (write-char (read-char)))
                                      (t
                                        ;; no, it's not - so this is an error
                                        (warn "Unexpected #\/ in ~A tag" name)))
                                ;; exit attribute loop in any case
                                (return-from attribute-loop))
                              ((letterp peek-char)
                                ;; a letter - this should start an attribute
                                (multiple-value-bind (name value string)
                                    ;; no need to cons up return values if we're
                                    ;; not going to rewrite anyway
                                    (read-attribute :skip (null rewrite-attribute)
                                                    :write-through (null rewrite-attribute))
                                  (cond ((and rewrite-attribute
                                              (string-equal name rewrite-attribute))
                                          ;; remember that we've seen the
                                          ;; attribute in question
                                          (setq attribute-found-p t)
                                          ;; if this an attribute which should be
                                          ;; rewritten do it and write the whole
                                          ;; stuff to *STANDARD-OUT* explicitly
                                          (cond ((funcall test-fn value)
                                                  (maybe-write-attribute value name))
                                                (t
                                                  ;; otherwise write it back
                                                  (write-string string))))
                                        (rewrite-attribute
                                          ;; we didn't rewrite this attribute but we
                                          ;; have to write it back to *STANDARD-OUTPUT*
                                          ;; because READ-ATTRIBUTE didn't do it
                                          (write-string string))))
                                (skip-whitespace))
                              (t
                                ;; an error - exit the attribute loop
                                (warn "Unexpected character ~A after <~A" peek-char name)
                                (return-from attribute-loop)))))))))
            (t
              ;; anything else means this is just #\<, no markup
              (write-char (read-char)))))))

;;; Multiaddr (mURI)
;; a ground-up implementation of libp2p multiaddr.
;; ref: https://github.com/multiformats/multiaddr

;; A multiaddr is represented internally as a list of KV pairs:
;; ((ip4 . "0.0.0.0") (udp . 44200) (dm . "ping")) ;= dm://0.0.0.0:44200/ping
