(in-package :net/codec/osc)

;;; params
(defparameter *default-osc-buffer-size* 1024)

;; utility functions for osc-string/padding slonking

(defun cat (&rest catatac)
  (apply #'concatenate '(vector (unsigned-byte 8)) catatac))

(defun padding-length (s)
  "returns the length of padding required for a given length of string"
  (declare (type fixnum s))
  (- 4 (mod s 4)))

(defun padded-length (s)
  "returns the length of an osc-string made from a given length of string"
  (declare (type fixnum s))
  (+ s (- 4 (mod s 4))))

(defun string-padding (string)
  "returns the padding required for a given osc string"
  (declare (type simple-string string))
  (pad (padding-length (length string))))

(defun pad (n)
  "make a sequence of the required number of #\Nul characters"
  (declare (type fixnum n))
  (make-array n :initial-element 0 :fill-pointer n))

(defclass osc-data () ())

(defclass message (osc-data)
  ((command
    :reader command
    :initarg :command)
   (args
    :reader args
    :initarg :args
    :initform nil)))

(defclass bundle (osc-data)
  ((timetag
    :reader timetag
    :initarg :timetag
    :initform :now)
   (elements
    :reader elements
    :initarg :elements
    :initform nil)))

;; Constructors

(defun make-message (command args)
  (unless (listp args)
    (setf args (list args)))
  (make-instance 'message
                 :command command
                 :args args))

(defun message (command &rest args)
  (make-message command args))

(defun make-bundle (timetag elements)
  (unless (listp elements)
    (setf elements (list elements)))
  (make-instance 'bundle
                 :timetag timetag
                 :elements elements))

(defun bundle (timetag &rest elements)
  (make-bundle timetag elements))

(defgeneric format-osc-data (data &key stream width))

(defmethod format-osc-data ((message message) &key (stream t)
                                                (width 80))
  (let ((args-string (format nil "~{~a~^ ~}" (args message))))
    (when (> (length args-string) width)
      (setf args-string
            (concatenate 'string
                         (subseq args-string 0 width)
                         "...")))
    (format stream "~a ~a~%"
            (command message)
            args-string)))

(defmethod format-osc-data ((bundle bundle) &key (stream t) (width 80))
  (format stream "~&[ ~a~%" (timetag bundle))
  (dolist (element (elements bundle))
    (format-osc-data element :stream stream :width width))
  (format stream "~&]~%"))

;;; Time

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))
(defconstant +2^32+ (expt 2 32))
(defconstant +2^32/million+ (/ +2^32+ (expt 10 6)))
(defconstant +usecs+ (expt 10 6))

(deftype timetag () '(unsigned-byte 64))

(defun timetagp (object)
  (typep object 'timetag))

(defun unix-secs+usecs->timetag (secs usecs)
  (let ((sec-offset (+ secs +unix-epoch+))) ; Seconds from 1900.
    (setf sec-offset (ash sec-offset 32))   ; Make seconds the top 32
                                            ; bits.
    (let ((usec-offset
           (round (* usecs +2^32/MILLION+)))) ; Fractional part.
      (the timetag (+ sec-offset usec-offset)))))

(defun get-current-timetag ()
  "Returns a fixed-point 64 bit NTP-style timetag, where the top 32
bits represent seconds since midnight 19000101, and the bottom 32 bits
represent the fractional parts of a second."
  #+sbcl (multiple-value-bind (secs usecs)
             (sb-ext:get-time-of-day)
           (the timetag (unix-secs+usecs->timetag secs usecs)))
  #-sbcl (error "Can't encode timetags using this implementation."))

(defun timetag+ (original seconds-offset)
  (declare (type timetag original))
  (let ((offset (round (* seconds-offset +2^32+))))
    (the timetag (+ original offset))))


;;;=====================================================================
;;; Functions for using double-float unix timestamps.
;;;=====================================================================

(defun get-unix-time ()
  "Returns a a double-float representing real-time now in seconds,
with microsecond precision, relative to 19700101."
  #+sbcl (multiple-value-bind (secs usecs)
             (sb-ext:get-time-of-day)
           (the double-float (+ secs (microseconds->subsecs usecs))))
  #-sbcl (error "Can't encode timetags using this implementation."))

(defun unix-time->timetag (unix-time)
  (multiple-value-bind (secs subsecs)
      (floor unix-time)
    (the timetag
         (unix-secs+usecs->timetag secs
                                   (subsecs->microseconds subsecs)))))

(defun timetag->unix-time (timetag)
  (if (= timetag 1)
      1                                 ; immediate timetag
      (let* ((secs (ash timetag -32))
             (subsec-int32 (- timetag (ash secs 32))))
        (the double-float (+ (- secs +unix-epoch+)
                             (int32->subsecs subsec-int32))))))

(defun microseconds->subsecs (usecs)
  (declare (type (integer 0 1000000) usecs))
  (coerce (/ usecs  +usecs+) 'double-float))

(defun subsecs->microseconds (subsecs)
  (declare (type (float 0.0 1.0) subsecs))
  (round (* subsecs +usecs+)))

(defun int32->subsecs (int32)
  "This maps a 32 bit integer, representing subsecond time, to a
double float in the range 0-1."
  (declare (type (unsigned-byte 32) int32))
  (coerce (/ int32 +2^32+) 'double-float))

(defun print-as-double (time)
  (format t "~%~F" (coerce time 'double-float))
  time)

(defgeneric encode-osc-data (data))

(defmethod encode-osc-data ((data message))
  "Encode an osc message with the given address and args."
  (with-slots (command args) data
    (concatenate '(vector (unsigned-byte 8))
                 (encode-address command)
                 (encode-typetags args)
                 (encode-args args))))

(defmethod encode-osc-data ((data bundle))
  "Encode an osc bundle. A bundle contains a timetag (symbol or 64bit
  int) and a list of message or nested bundle elements."
  (with-slots (timetag elements) data
    (cat '(35 98 117 110 100 108 101 0)    ; #bundle
         (if timetag
             (encode-timetag timetag)
             (encode-timetag :now))
         (apply #'cat (mapcar #'encode-bundle-elt elements)))))

(defgeneric encode-bundle-elt (data))

(defmethod encode-bundle-elt ((data message))
  (let ((bytes (encode-osc-data data)))
    (cat (encode-int32 (length bytes)) bytes)))

(defmethod encode-bundle-elt ((data bundle))
  (let ((bytes (encode-osc-data data)))
    (cat (encode-int32 (length bytes)) bytes)))

;; Auxilary functions

(defun encode-address (address)
  (cat (map 'vector #'char-code address)
       (string-padding address)))

(defun encode-typetags (data)
  "creates a typetag string suitable for the given data.
  valid typetags according to the osc spec are ,i ,f ,s and ,b
  non-std extensions include ,{h|t|d|S|c|r|m|T|F|N|I|[|]}
                             see the spec for more details. ..

  NOTE: currently handles the following tags
   i => #(105) => int32
   f => #(102) => float
   s => #(115) => string
   b => #(98)  => blob
   h => #(104) => int64
  and considers non int/float/string data to be a blob."

  (let ((lump (make-array 0 :adjustable t
                          :fill-pointer t)))
    (macrolet ((write-to-vector (char)
                 `(vector-push-extend
                   (char-code ,char) lump)))
      (write-to-vector #\,)
      (dolist (x data)
        (typecase x
          (integer (if (>= x 4294967296) (write-to-vector #\h) (write-to-vector #\i)))
          (float (write-to-vector #\f))
          (simple-string (write-to-vector #\s))
          (keyword (write-to-vector #\s))
          (t (write-to-vector #\b)))))
    (cat lump
         (pad (padding-length (length lump))))))

(defun encode-args (args)
  "encodes args in a format suitable for an OSC message"
  (let ((lump (make-array 0 :adjustable t :fill-pointer t)))
    (macrolet ((enc (f)
                 `(setf lump (cat lump (,f x)))))
      (dolist (x args)
        (typecase x
          (integer (if (>= x 4294967296) (enc encode-int64) (enc encode-int32)))
          (float (enc encode-float32))
          (simple-string (enc encode-string))
          (t (enc encode-blob))))
      lump)))


;;;;;; ;    ;;    ;     ; ;     ; ; ;         ;
;;
;;    decoding OSC messages
;;
;;; ;;    ;;     ; ;     ;      ;      ; ;

(defun bundle-p (buffer &optional (start 0))
  "A bundle begins with '#bundle' (8 bytes). The start argument should
index the beginning of a bundle in the buffer."
  (= 35 (elt buffer start)))

(defun get-timetag (buffer &optional (start 0))
  "Bytes 8-15 are the bundle timestamp. The start argument should
index the beginning of a bundle in the buffer."
  (decode-timetag (subseq buffer
                          (+ 8 start)
                          (+ 16 start))))

(defun get-bundle-element-length (buffer &optional (start 16))
  "Bytes 16-19 are the size of the bundle element. The start argument
should index the beginning of the bundle element (length, content)
pair in the buffer."
  (decode-int32 (subseq buffer start (+ 4 start))))

(defun get-bundle-element (buffer &optional (start 16))
  "Bytes 20 upto to the length of the content (defined by the
preceding 4 bytes) are the content of the bundle. The start argument
should index the beginning of the bundle element (length, content)
pair in the buffer."
  (let ((length (get-bundle-element-length buffer start)))
    (subseq buffer
            (+ 4 start)
            (+ (+ 4 start)
               (+ length)))))

(defun split-sequence-by-n (sequence n)
  (loop :with length := (length sequence)
        :for start :from 0 :by n :below length
        :collecting (coerce
                     (subseq sequence start (min length (+ start n)))
                     'list)))

(defun print-buffer (buffer &optional (n 8))
  (format t "~%~{~{ ~5d~}~%~}Total: ~a bytes~2%"
          (split-sequence-by-n buffer n)
          (length buffer)))

(defun decode-bundle (buffer &key (start 0) end)
  "Decodes an osc bundle/message into a bundle/message object. Bundles
  comprise an osc-timetag and a list of elements, which may be
  messages or bundles recursively. An optional end argument can be
  supplied (i.e. the length value returned by socket-receive, or the
  element length in the case of nested bundles), otherwise the entire
  buffer is decoded - in which case, if you are reusing buffers, you
  are responsible for ensuring that the buffer does not contain stale
  data."
  (unless end
    (setf end (- (length buffer) start)))
  (when *log-level*
    (format t "~%Buffer start: ~a end: ~a~%" start end)
    (print-buffer (subseq buffer start end)))
  (if (bundle-p buffer start)
      ;; Bundle
      (let ((timetag (get-timetag buffer start)))
        (incf start (+ 8 8))            ; #bundle, timetag bytes
        (loop while (< start end)
              for element-length = (get-bundle-element-length
                                    buffer start)
              do (incf start 4)            ; length bytes
              when *log-level*
              do (format t "~&Bundle element length: ~a~%" element-length)
              collect (decode-bundle buffer
                                     :start start
                                     :end (+ start element-length))
              into elements
              do (incf start (+ element-length))
              finally (return
                        (values (make-bundle timetag elements)
                                timetag))))
      ;; Message
      (let ((message
             (decode-message
              (subseq buffer start (+ start end)))))
        (make-message (car message) (cdr message)))))

(defun decode-message (message)
  "reduces an osc message to an (address . data) pair. .."
  (declare (type (vector *) message))
  (let ((x (position (char-code #\,) message)))
    (if (eq x nil)
        (format t "message contains no data.. ")
        (cons (decode-address (subseq message 0 x))
              (decode-taged-data (subseq message x))))))

(defun decode-address (address)
  (coerce (map 'vector #'code-char
               (delete 0 address))
          'string))

(defun decode-taged-data (data)
  "decodes data encoded with typetags...
  NOTE: currently handles the following tags
   i => #(105) => int32
   f => #(102) => float
   s => #(115) => string
   b => #(98)  => blob
   h => #(104) => int64"

  (let ((div (position 0 data)))
    (let ((tags (subseq data 1 div))
          (acc (subseq data (padded-length div)))
          (result '()))
      (map 'vector
           #'(lambda (x)
               (cond
                 ((eq x (char-code #\i))
                  (push (decode-int32 (subseq acc 0 4))
                        result)
                  (setf acc (subseq acc 4)))
                 ((eq x (char-code #\h))
                  (push (decode-uint64 (subseq acc 0 8))
                        result)
                  (setf acc (subseq acc 8)))
                 ((eq x (char-code #\f))
                  (push (decode-float32 (subseq acc 0 4))
                        result)
                  (setf acc (subseq acc 4)))
                 ((eq x (char-code #\s))
                  (let ((pointer (padded-length (position 0 acc))))
                    (push (decode-string
                           (subseq acc 0 pointer))
                          result)
                    (setf acc (subseq acc pointer))))
                 ((eq x (char-code #\b))
                  (let* ((size (decode-int32 (subseq acc 0 4)))
                         (bl (+ 4 size))
                         (end (+ bl (mod (- 4 bl) 4))))
                    ;; NOTE: cannot use (padded-length bl), as it is not the same algorithm.
                    ;; Blobs of 4, 8, 12 etc bytes should not be padded!
                    (push (decode-blob (subseq acc 0 end))
                          result)
                    (setf acc (subseq acc end))))
                 (t (error "unrecognised typetag ~a" x))))
           tags)
      (nreverse result))))


;;;;;; ;; ;; ; ; ;  ;  ; ;;     ;
;;
;; timetags
;;
;; - timetags can be encoded using a value, or the :now and :time
;;   keywords. the keywords enable either a tag indicating 'immediate'
;;   execution, or a tag containing the current time (which will most
;;   likely be in the past of any receiver) to be created.
;;
;; - see this c.l.l thread to sync universal-time and internal-time
;;   http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/c207fef63a78d720/adc7442d2e4de5a0?lnk=gst&q=internal-real-time-sync&rnum=1#adc7442d2e4de5a0

;; - In SBCL, using sb-ext:get-time-of-day to get accurate seconds and
;;   microseconds from OS.
;;
;;;; ;; ; ;

(defun encode-timetag (timetag)
  "From the spec: `Time tags are represented by a 64 bit fixed point
number. The first 32 bits specify the number of seconds since midnight
on January 1, 1900, and the last 32 bits specify fractional parts of a
second to a precision of about 200 picoseconds. This is the
representation used by Internet NTP timestamps'. For an
'instantaneous' timetag use (encode-timetag :now), and for a timetag
with the current time use (encode-timetag :time)."
  (cond
    ((equalp timetag :now)
     ;; a 1 bit timetag will be interpreted as 'immediately'
     #(0 0 0 0 0 0 0 1))
    ((equalp timetag :time)
     ;; encode timetag with current real time
     (encode-int64 (get-current-timetag)))
    ((timetagp timetag)
     ;; encode osc timetag
     (encode-int64 timetag))
    (t (error "Argument given is not one of :now, :time, or timetagp."))))

(defun decode-timetag (timetag)
  "Return a 64 bit timetag from a vector of 8 bytes in network byte
  order."
  (if (equalp timetag #(0 0 0 0 0 0 0 1))
      1 ; A timetag of 1 is defined as immediately.
      (decode-uint64 timetag)))

;;;;; ; ; ;;    ;; ; ;
;;
;; dataformat en- de- cetera.
;;
;;; ;; ;   ;  ;

;; floats are encoded using implementation specific 'internals' which is not
;; particulaly portable, but 'works for now'.

(defun encode-float32 (f)
  "encode an ieee754 float as a 4 byte vector. currently sbcl/cmucl specific"
  (encode-int32 (sb-kernel:single-float-bits f)))

(defun decode-float32 (s)
  "ieee754 float from a vector of 4 bytes in network byte order"
  (sb-kernel:make-single-float (decode-int32 s)))

(defmacro defint-decoder (num-of-octets &optional docstring)
  (let ((decoder-name (intern (format nil "~:@(decode-uint~)~D" (* 8 num-of-octets))))
        (seq (gensym))
        (int (gensym)))
    `(defun ,decoder-name (,seq)
       ,@(when docstring
           (list docstring))
       (let* ((,int 0)
              ,@(loop
                  for n below num-of-octets
                  collect `(,int (dpb (aref ,seq ,n) (byte 8 (* 8 (- (1- ,num-of-octets) ,n)))
                                      ,int))))
         ,int))))

(defint-decoder 8)

(defun decode-uint32 (s)
  "4 byte -> 32 bit unsigned int"
  (let ((i (+ (ash (elt s 0) 24)
              (ash (elt s 1) 16)
              (ash (elt s 2) 8)
              (elt s 3))))
    i))

(defmacro defint-encoder (num-of-octets &optional docstring)
  (let ((enc-name (intern (format nil "~:@(encode-int~)~D" (* 8 num-of-octets))))
        (buf (gensym))
        (int (gensym)))
    `(defun ,enc-name (,int)
       ,@(when docstring
           (list docstring))
       (let ((,buf (make-array ,num-of-octets :element-type '(unsigned-byte 8))))
         ,@(loop
                 for n below num-of-octets
                 collect `(setf (aref ,buf ,n)
                                (ldb (byte 8 (* 8 (- (1- ,num-of-octets) ,n)))
                                     ,int)))
         ,buf))))

(defint-encoder 4 "Convert an integer into a sequence of 4 bytes in network byte order (32 bit).")
(defint-encoder 8 "Convert an integer into a sequence of 8 bytes in network byte order (64 bit).")

(defun decode-int32 (s)
  "4 byte -> 32 bit int -> two's complement (in network byte order)"
  (let ((i (decode-uint32 s)))
    (if (>= i #.(1- (expt 2 31)))
        (- (- #.(expt 2 32) i))
        i)))

(defun decode-int64 (s)
  "8 byte -> 64 bit int -> two's complement (in network byte order)"
  (let ((i (decode-uint64 s)))
    (if (>= i #.(1- (expt 2 63)))
        (- (- #.(expt 2 64) i))
        i)))

;; osc-strings are unsigned bytes, padded to a 4 byte boundary

(defun encode-string (string)
  "encodes a string as a vector of character-codes, padded to 4 byte boundary"
  (cat (map 'vector #'char-code string)
       (string-padding string)))

(defun decode-string (data)
  "converts a binary vector to a string and removes trailing #\nul characters"
  (string-trim '(#\nul) (coerce (map 'vector #'code-char data) 'string)))


;; blobs are binary data, consisting of a length (int32) and bytes which are
;; osc-padded to a 4 byte boundary.

(defun encode-blob (blob)
  "encodes a blob from a given vector"
  (let ((bl (length blob)))
    (cat (encode-int32 bl) blob
         (pad (padding-length bl)))))

(defun decode-blob (blob)
  "decode a blob as a vector of unsigned bytes."
  (let ((size (decode-int32
               (subseq blob 0 4))))
    (subseq blob 4 (+ 4 size))))

(defun make-osc-tree ()
  (make-hash-table :test 'equalp))


;;; ;; ;;;;;;  ;        ;  ;  ;
;;
;; register/delete and dispatch. ..
;;
;;;;  ; ; ;   ;;

(defun dp-register (tree address function)
  "Registers a function to respond to incoming osc messages. Since
   only one function should be associated with an address, any
   previous registration will be overwritten."
  (setf (gethash address tree)
        function))

(defun dp-remove (tree address)
  "Removes the function associated with the given address."
  (remhash address tree))

(defun dp-match (tree pattern)
  "Returns a list of functions which are registered for dispatch for a
given address pattern."
  (list (gethash pattern tree)))

(defgeneric dispatch (tree data device address port &optional timetag
                                                      parent-bundle))

(defmethod dispatch (tree (data message) device address port &optional
                                                               timetag
                                                               parent-bundle)
  "Calls the function(s) matching the address(pattern) in the osc
message passing the message object, the recieving device, and
optionally in the case where a message is part of a bundle, the
timetag of the bundle and the enclosing bundle."
  (let ((pattern (command data)))
    (dolist (x (dp-match tree pattern))
      (unless (eq x NIL)
        (funcall x (command data) (args data) device address port
                 timetag parent-bundle)))))

(defmethod dispatch (tree (data bundle) device address port &optional
                                                              timetag
                                                              parent-bundle)
  "Dispatches each bundle element in sequence."
  (declare (ignore timetag parent-bundle))
  (dolist (element (elements data))
    (dispatch tree element device address port (timetag data) data)))
