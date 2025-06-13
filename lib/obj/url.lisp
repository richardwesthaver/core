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
(definline url-encode-params (params &key (encoding *default-external-format*)
                                     space-to-plus
                                     (percent-encode t))
  (declare (optimize (speed 3)))
  (check-type params list)
  (flet ((maybe-encode (string)
           (if percent-encode
               (url-encode string
                           :encoding encoding
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

(defun url-encode (data &key (encoding *default-external-format*)
                             (start 0)
                             end
                             space-to-plus)
  (declare ((or string octet-vector) data)
           (integer start)
           (optimize (speed 3) (safety 2)))
  (let* ((octets (if (stringp data)
                     (string-to-octets data :external-format encoding :start start :end end)
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
                (replace res (integer-to-hexdigit byte) :start1 i)
                (incf i 2))))
    (setf (fill-pointer res) i)
    res))

;;; Decode
(definline url-decode-params (data &key (delimiter #\&)
                                   (encoding *default-external-format*)
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
      (labels ((maybe-decode (string encoding start end)
                 (if percent-decode
                     (url-decode string
                                 :encoding encoding
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
                          (cons (maybe-decode data encoding start-mark =-mark)
                                (maybe-decode data encoding (1+ =-mark) p))))
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
                          (cons (maybe-decode data encoding start-mark p)
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

(defun url-decode (data &key (encoding *default-external-format*)
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
    (octets-to-string buffer :end i :external-format encoding)))

