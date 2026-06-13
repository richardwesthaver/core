;;; obj/uri/mask.lisp --- string character masks for parsing

;;

;;; Code:
(in-package :obj/uri)
;; To match sets of characters, the parser uses bit vectors constructed from
;; lists of characters.

;; The size of bit vectors are defined to check for characters in the range 0
;; to 126 (~). We use location 0 and 1, which are never set by any generated
;; character list, as boolean.
(eval-always
(defparameter +uri-bit-vector-size+ 127)
;; The is the index at which we store the boolean: does this bitvector allow
;; `ucschar' (from the grammar)?
(defparameter +bitvector-index-ucschar+  0)
;; The is the index at which we store the boolean: does this bitvector allow
;; `iprivate' (from the grammar)?
(defparameter +bitvector-index-iprivate+ 1)

(defun generate-character-list (char-start char-end)
    ;; Generate a list of characters between char-start and char-end,
    ;; inclusive of the start and end characters.
    (when (>= (char-code char-start) (char-code char-end))
      (error "char-start (~s) must come before char-end (~s)."
             char-start char-end))
    ;; Make sure it doesn't index off the end of the array:
    (when (>= (char-code char-end) +uri-bit-vector-size+)
      (error "Illegal char-code (>= ~d)." +uri-bit-vector-size+))
    (do* ((stop-code (1- (char-code char-start)))
          (c (char-code char-end) (1- c))
          (res '()))
         ((= c stop-code) res)
      (push (code-char c) res))))

(defmacro char-included-p (bit-vector char-code)
  `(= 1 (sbit ,bit-vector ,char-code)))

(defmacro safe-char-included-p (bit-vector char-code)
  (let ((g-bv (gensym))
        (g-cc (gensym)))
    `(let* ((,g-bv ,bit-vector)
            (,g-cc ,char-code))
       (or (null ,g-bv)
           (and (< ,g-cc +uri-bit-vector-size+)
                (char-included-p ,g-bv ,g-cc))))))

(defun make-char-bitvector (chars &key except iri)
  ;; Return a bitvector which has a 1 for each character represented in
  ;; CHARS, where the index is the char-code of the character.  If EXCEPT
  ;; is non-nil, it should be a list of characters to exclude.
  ;;
  ;; If IRI is non-nil, it should be either :ucschar or :iprivate.
  ;; Since the first two bits of the bitvector returned by this function
  ;; are unused (those characters are invalid for URIs and IRIs), we use
  ;; those bits for IRI validation.  During IRI character validation,
  ;; characters outside the ASCII range are validated with either ucscharp
  ;; or iprivatep.  IRI mode is indicated by .iri-mode. having a non-nil
  ;; value.
  (do* ((a (make-array +uri-bit-vector-size+
                       :element-type 'bit :initial-element 0))
        (chars chars (cdr chars))
        (c (car chars) (car chars)))
       ((null chars)
        (when iri
          ;; set the booleans for this bitvector, used in .looking-at
          (ecase iri
            (:ucschar (setf (sbit a #.+bitvector-index-ucschar+) 1))
            (:iprivate (setf (sbit a #.+bitvector-index-iprivate+) 1))))
        a)
    (if* (and except (member c except :test #'eq))
       thenret
       else (setf (sbit a (char-code c)) 1))))

;; Lists of characters used to make the bit vectors.  These lists are
;; pretty much straight out of the grammars.
(defparameter *alpha-chars*
  '#.(append (generate-character-list #\A #\Z)
             (generate-character-list #\a #\z)))

(defparameter *digit-chars* '#.(generate-character-list #\0 #\9))

(defparameter *hexdig-chars*
  (append *digit-chars*
          '#.(generate-character-list #\A #\F)
          '#.(generate-character-list #\a #\f)))

(defparameter *alphanum-chars*  (append *alpha-chars* *digit-chars*))
(defparameter *alphanum+-chars* (append *alphanum-chars* '(#\-)))

(defparameter *sub-delims-chars* '(#\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\=))

(defparameter *unreserved-chars*
  (append *alpha-chars* *digit-chars* '(#\- #\. #\_ #\~)))

(defparameter *pchar-chars*
  (append *unreserved-chars* *sub-delims-chars* '(#\: #\@)))

;; used in pathname to URI conversion:
(defparameter *pchar/-chars*  (append *pchar-chars* '(#\/)))

(defparameter *urn-nss-chars* (append *pchar-chars* '(#\/)))

(defparameter *segment-nz-nc-chars* ;; pchar w/o #\:
  (append *unreserved-chars* *sub-delims-chars* '(#\@)))

(defparameter *query-strict-chars*    (append *pchar-chars* '(#\/ #\?)))
(defparameter *urn-query-chars*       (append *pchar-chars* '(#\/)))
(defparameter *fragment-strict-chars* (append *pchar-chars* '(#\/ #\?)))

(defparameter *ipvfuture-chars*
  (append *unreserved-chars* *sub-delims-chars* '(#\:)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *alpha-bitvector*      (make-char-bitvector *alpha-chars*))
(defparameter *digit-bitvector*      (make-char-bitvector *digit-chars*))
(defparameter *alphanum-bitvector*   (make-char-bitvector *alphanum-chars*))
(defparameter *alphanum+-bitvector*  (make-char-bitvector *alphanum+-chars*))
(defparameter *hexdig-bitvector*     (make-char-bitvector *hexdig-chars*))
(defparameter *pchar-bitvector*      (make-char-bitvector *pchar-chars*
                                                          :iri :ucschar))
(defparameter *urn-nss-bitvector*    (make-char-bitvector *urn-nss-chars*
                                                          :iri :ucschar))
(defparameter *unreserved-bitvector* (make-char-bitvector *unreserved-chars*
                                                          :iri :ucschar))

;; used in pathname to URI conversion:
(defparameter *pchar/-bitvector*     (make-char-bitvector *pchar/-chars*
                                                          :iri :ucschar))

(defparameter *userinfo-bitvector*
  (make-char-bitvector
   (append *unreserved-chars* *sub-delims-chars* '(#\:))
   :iri :ucschar))

(defparameter *reg-name-bitvector*
  (make-char-bitvector (append *unreserved-chars* *sub-delims-chars*)
                       :iri :ucschar))

(defparameter *scheme-bitvector*
  (make-char-bitvector (append *alpha-chars* *digit-chars* '(#\+ #\- #\.))))

(defparameter *query-bitvector-strict*
  (make-char-bitvector *query-strict-chars*
                       :iri :iprivate))

(defparameter *query-bitvector-non-strict*
  (make-char-bitvector (append *query-strict-chars*
                               '(#\| #\^
                                 ;; Too many websites/tools use this in URLs
                                 #\space))
                       :iri :iprivate))

;;;;;;;;; HACK
;; See discussion in rfe15844.  Decoding the query should not touch percent
;; encodings of #\+, #\= and #\&, because those are interpreted by
;; another specification (HTTP).

(defparameter *decode-query-strict-chars*
  (append *unreserved-chars*
          ;; Instead of *sub-delims-chars*, this (which is just like
          ;; *sub-delims-chars*, except for the commented out characters):
          '(#\! #\$ #\' #\( #\) #\* #\, #\;
            ;;#\& #\+ #\=
            )
          '(#\: #\@)))

(defparameter *decode-query-bitvector-strict*
  (make-char-bitvector *decode-query-strict-chars* :iri :iprivate))

(defparameter *decode-query-bitvector-non-strict*
  (make-char-bitvector
   (append *decode-query-strict-chars*
           '(#\| #\^
             ;; Too many websites/tools use this in URLs
             #\space))
   :iri :iprivate))
;;;;;;;;; ...HACK

(defparameter *fragment-bitvector-strict*
  (make-char-bitvector *fragment-strict-chars* :iri :ucschar))

(defparameter *fragment-bitvector-non-strict*
  (make-char-bitvector
   (append *fragment-strict-chars*
           '(#\#
             ;; Too many websites/tools use these in URLs
             #\space #\|))
   :iri :ucschar))

(defparameter *segment-nz-nc-bitvector*
  (make-char-bitvector *segment-nz-nc-chars* :iri :ucschar))

(defparameter *urn-query-bitvector*
  ;; Not sure which to use, :ucschar or :iprivate.  The universe will
  ;; probably end before anyone figures it out.
  (make-char-bitvector *urn-query-chars* :iri :iprivate))

(defparameter *ipvfuture-bitvector*
  (make-char-bitvector *ipvfuture-chars* :iri :ucschar))

;; The part of a URI that can have percent encoding:
;; - userinfo
;; - host
;; - path
;; - query
;; - fragment

(defun percent-decode-string (string allowed-bitvector)
  ;; Return a new string based on STRING which has all percent encoded
  ;; pairs (%xx) turned into real characters.  If ALLOWED-BITVECTOR is
  ;; non-nil, only characters that `match' this bitvector are converted.
  ;; (declare (type string string))
  (do* ((i 0 (1+ i))
        (max (length string))
        (new-string (make-string max))
        (new-i 0 (1+ new-i))
        ch ch2 chc chc2)
       ((= i max)
        ;; (nyi! "was formerly a call to EXCL package") - shrinks new-string vector to fit size?
        (remove #\Nul new-string))
    (declare (fixnum i max new-i))
    (if* (char= #\% (setq ch (schar string i)))
       then (when (> (+ i 3) max)
              (error "Unsyntactic percent encoding at ~d in ~s." i string))
            (setq ch (schar string (incf i)))
            (setq ch2 (schar string (incf i)))
            (when (not (and (setq chc (digit-char-p ch 16))
                            (setq chc2 (digit-char-p ch2 16))))
              (error
               "Non-hexidecimal digits after % at ~d in ~s."
               (- i 2) string))
            (let ((ci (the fixnum
                           (+ (the fixnum (* 16 (the fixnum chc)))
                              (the fixnum chc2)))))
              (declare (fixnum ci))
              (if* (safe-char-included-p allowed-bitvector ci)
                 then ;; OK to convert
                      (setf (schar new-string new-i)
                            (code-char ci))
                 else ;; leave percent encoded
                      (setf (schar new-string new-i) #\%)
                      (setf (schar new-string (incf new-i)) ch)
                      (setf (schar new-string (incf new-i)) ch2)))
       else (setf (schar new-string new-i) ch))))

;; This is experimental work in progress.
#+ignore
(defun percent-decode-utf8-string (string allowed-bitvector)
  ;; like percent-decode-string, but handle UTF-8 encoded sequences
;;;; chars 0..127 use allowed-bitvector
;;;; chars  > 127 use RFC 3629 grammar
  (do* ((i 0 (1+ i))
        (max (length string))
        (new-string (make-string max))
        (new-i 0 (1+ new-i))
        ch ch2 chc chc2
        (state :start)
        (vec (make-array 4 :element-type '(unsigned-byte 8)))
        (temps (make-string 1 :element-type 'character))
        (veci 0))
       ((= i max)
        (excl::.primcall 'sys::shrink-svector new-string new-i)
        new-string)
    (declare (fixnum i max new-i veci)
             (type (simple-array (unsigned-byte 8) (4)) vec)
             (dynamic-extent vec))
    (cond
      ((char= #\% (setq ch (schar string i)))
       (when (> (+ i 3) max)
         (excl::.parse-error
          "Unsyntactic percent encoding at ~d in ~s." i string))
       (setq ch (schar string (incf i)))
       (setq ch2 (schar string (incf i)))
       (when (not (and (setq chc (digit-char-p ch 16))
                       (setq chc2 (digit-char-p ch2 16))))
         (excl::.parse-error
          "Non-hexidecimal digits after % at ~d in ~s."
          (- i 2) string))
       (let ((cc (the fixnum
                      (+ (the fixnum (* 16 (the fixnum chc)))
                         (the fixnum chc2)))))
         (declare (fixnum cc))
         (cond
           ((<= cc #.+uri-bit-vector-size+)
            (if* (char-included-p allowed-bitvector cc)
               then ;; OK to convert
                    (setf (schar new-string new-i)
                          (code-char cc))
               else ;; leave percent encoded
                    (setf (schar new-string new-i) #\%)
                    (setf (schar new-string (incf new-i)) ch)
                    (setf (schar new-string (incf new-i)) ch2)))
           (t
            ;; check for valid UTF-8 encoding (from RFC 2234):
;;;; UTF8-octets = *( UTF8-char )
;;;; UTF8-char   = UTF8-1 / UTF8-2 / UTF8-3 / UTF8-4
;;;; UTF8-1      = %x00-7F
;;;; UTF8-2      = %xC2-DF UTF8-tail
;;;; UTF8-3      = %xE0 %xA0-BF UTF8-tail / %xE1-EC 2( UTF8-tail ) /
;;;;               %xED %x80-9F UTF8-tail / %xEE-EF 2( UTF8-tail )
;;;; UTF8-4      = %xF0 %x90-BF 2( UTF8-tail ) / %xF1-F3 3( UTF8-tail ) /
;;;;               %xF4 %x80-8F 2( UTF8-tail )
;;;; UTF8-tail   = %x80-BF
            ;; We have a little FSM here.  `state' can be one of:
            ;;  :start      :: looking for markers for UTF8-{2,3,4}
            ;;  :utf8-3a    :: have UTF8-3, read %E0, look for %xA0-BF 
            ;;  :utf8-3b    :: have UTF8-3, read %ED, look for %x80-9F
            ;;  :utf8-4a    :: have UTF8-4, read %F0, look for %x90-BF
            ;;  :utf8-4b    :: have UTF8-4, read %F4, look for %x80-8F
            ;;  :utf8-tail3 :: look for 3( UTF8-tail )
            ;;  :utf8-tail2 :: look for 2( UTF8-tail )
            ;;  :utf8-tail1 :: look for 1( UTF8-tail )
            (case state
              (:start
;;;; UTF8-2
               (if* (<= #xC2 cc #xDF)
                  then (setf (aref vec 0) cc)
                       (setq veci 1)
                       (setq state :utf8-tail1)
;;;; UTF8-3
                elseif (= #xE0 cc)
                  then (setf (aref vec 0) cc)
                       (setq veci 1)
                       (setq state :utf8-3a)
                elseif (or (<= #xE1 cc #xEC)
                           (<= #xEE cc #xEF))
                  then (setf (aref vec 0) cc)
                       (setq veci 1)
                       (setq state :utf8-tail2)
                elseif (= #xED cc)
                  then (setf (aref vec 0) cc)
                       (setq veci 1)
                       (setq state :utf8-3b)
;;;; UTF8-4
                elseif (= #xF0 cc)
                  then (setf (aref vec 0) cc)
                       (setq veci 1)
                       (setq state :utf8-4a)
                elseif (<= #xF1 cc #xF3)
                  then (setf (aref vec 0) cc)
                       (setq veci 1)
                       (setq state :utf8-tail3)
                elseif (= #xF4 cc)
                  then (setf (aref vec 0) cc)
                       (setq veci 1)
                       (setq state :utf8-4b)
                  else (excl::.parse-error
;;;;TODO:
                        "invalid UTF-8 encoding...")))
              (:utf8-3a
               (if* (<= #xA0 cc #xBF)
                  then (setf (aref vec veci) cc)
                       (incf veci)
                       (setq state :utf8-tail1)
                  else (error "invalid UTF8-3 2nd byte: ~x" cc)))
              (:utf8-3b
               (if* (<= #x80 cc #x9F)
                  then (setf (aref vec veci) cc)
                       (incf veci)
                       (setq state :utf8-tail3)
                  else (error "invalid UTF8-3 2nd byte: ~x" cc)))
              (:utf8-4a
               (if* (<= #x90 cc #xBF)
                  then (setf (aref vec veci) cc)
                       (incf veci)
                       (setq state :utf8-tail2)
                  else (error "invalid UTF8-4 2nd byte: ~x" cc)))
              (:utf8-4b
               (if* (<= #x80 cc #x8F)
                  then (setf (aref vec veci) cc)
                       (incf veci)
                       (setq state :utf8-tail2)
                  else (error "invalid UTF8-4 2nd byte: ~x" cc)))
              (:utf8-tail3
               (if* (<= #x80 cc #xBF)
                  then (setf (aref vec veci) cc)
                       (incf veci)
                       (setq state :utf8-tail2)))
              (:utf8-tail2
               (if* (<= #x80 cc #xBF)
                  then (setf (aref vec veci) cc)
                       (incf veci)
                       (setq state :utf8-tail1)))
              (:utf8-tail1
               (if* (<= #x80 cc #xBF)
                  then (setf (aref vec veci) cc)
                       (setq state :done)))
              (:done
               (octets-to-string vec :external-format :utf-8
                                     :end veci :string temps)
               (setf (schar new-string new-i) (char temps 0)))
              (t (error "internal error: bad state: ~s" state)))))))
      (t
       (setq state :start)
       (setf (schar new-string new-i) ch)))))

(defun percent-encode-string (string allowed-bitvector)
  ;; Return a new string based on STRING which has all characters which do
  ;; not match ALLOWED-BITVECTOR converted into percent encoded pairs (%xx).
  ;; Percent-encoded pairs in the string are skipped over, as it is assumed
  ;; they were required to be encoded.
  ;;
  ;; Make a string as big as it possibly needs to be (3 times the original
  ;; size), and truncate it at the end.
  ;; (declare (type string string))
  (declare (optimize (safety 1))) 
  ;;(declare (:explain :calls :types))
  (do* ((hexchars ;; RFC 3986 section 2.1 says use upper case:
         "0123456789ABCDEF")
        (pct (char-code #\%))
        (max (length string))
        (new-max (* 3 max)) ;; worst case new size
        (new-string (make-string new-max))
        (i 0 (1+ i))
        (new-i -1)
        (ci ;; so the fixnum decl is true:
         0)
        c)
       ((= i max)
        ;; is it safe to delete all 0 chars here?
        ;; (nyi! "was previously a call to EXCL")
        (remove #\Nul new-string))
    (declare (fixnum pct max new-max i new-i ci))
    (setq ci (char-code (setq c (schar string i))))
    (if* (or (= ci pct) ;; skip %'s
             (safe-char-included-p allowed-bitvector ci))
       then ;; ok as is
            (incf new-i)
            (setf (schar new-string new-i) c)
       else ;; need to escape it
            (let ((d1 (ash ci -4))
                  (d2 (logand ci #xf)))
              (declare (fixnum d1 d2))
              (setf (schar new-string (incf new-i)) #\%)
              (setf (schar new-string (incf new-i)) (schar hexchars d1))
              (setf (schar new-string (incf new-i)) (schar hexchars d2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For efficiency, we do as few subseq's as possible.  To achieve this, we
;; return, from various parser functions, the start/end pair encoded into a
;; fixnum.   This means the limit for a URI string is limited to 16384 on a
;; 32-bit Lisp.  It appears from searches that this is well above the
;; accepted maximum for URI strings.

(eval-always
  ;; The max array index is 1/2 of the available fixnum range.
  (defparameter +uri-max-string-length+
    #.(expt 2 (truncate (integer-length most-positive-fixnum) 2)))
  (defparameter +uri-pack-shift+
    #.(truncate (integer-length most-positive-fixnum) 2))
  (defparameter +uri-unpack-shift+
    #.(- (truncate (integer-length most-positive-fixnum) 2)))
  (defparameter +uri-unpack-mask+
    #.(1- (ash 1 (truncate (integer-length most-positive-fixnum) 2))))

  ;; This is used as a marker for the null string.  It must be a fixnum
  ;; that can't be returned as an index into a string.
  (defparameter *uri-null-marker* -1)
  )

(defun check-xri-string (string)
  ;; Make sure that:
  ;; 1. STRING is a simple string, and
  ;; 2. Two indices into STRING can packed into a single fixnum.
  ;;    This is what xsubseq/val do.
  (or (stringp string)
      (error "string must be a simple string."))
  (or (< (length string) #.+uri-max-string-length+)
      (error "string is larger than ~d characters."
             #.+uri-max-string-length+)))

(defun xsubseq (start end)
  ;; Encode START and END into a fixnum.
  (declare (fixnum start end) (optimize (safety 1)))
  (the fixnum
       (+ start (the fixnum
                     (ash end +uri-pack-shift+)))))

(defun xval (string i)
  ;; Return the subsequence of STRING given by I, which was encoded with
  ;; XSUBSEQ.
  (declare (type (or fixnum null) i) (optimize (safety 1)))
  (when i
    (cond
      ((= i *uri-null-marker*) "")
      (t (let ((start (the fixnum (logand i +uri-unpack-mask+)))
               (end (the fixnum
                         (ash i +uri-unpack-shift+))))
           (declare (fixnum start end))
           (if* (simple-string-p string)
              then ;; This is a good bit faster than calling subseq
                   (do* ((len (the fixnum (- end start)))
                         (res (make-string len))
                         (src-index start (the fixnum (1+ src-index)))
                         (dst-index 0 (the fixnum (1+ dst-index))))
                        ((= src-index end) res)
                     (declare (fixnum len src-index dst-index))
                     (setf (schar res dst-index) (schar string src-index)))
              else (subseq string start end)))))))

(defun at-end-p (i end)
  ;; return T if index I is beyond the END of the string
  (>= i end))

;; This macro is very specialized and not hygenic.  It is built for pure
;; speed.
(defmacro .looking-at (simple thing string index end char-equal)
    ;; INDEX and END are declared FIXNUM by our caller.
    ;; SIMPLE-STRING-P and SCHAR are much faster than STRINGP and CHAR.
    ;; For the details of what this function returns, see looking-at below.
  (let ((stringp (if simple 'simple-string-p 'stringp))
        (schar (if simple 'schar 'char))
        ;; TODO
          (length (if simple 'sequence:length 'length))
          (len (gensym))
          (i (gensym))
          (j (gensym))
          (x (gensym))
          (c (gensym)))
      `(let ((,len 0))
         (declare (fixnum ,len))
         (if* (at-end-p ,index ,end)
            then nil
          elseif (characterp ,thing)
            then ;; In this case, we ignore CHAR-EQUAL and always do the
                 ;; character comparison with CHAR= (case sensitively).
                 (when (char= ,thing (,schar ,string ,index))
                   (the fixnum (1+ ,index)))
          elseif (,stringp ,thing)
            then (when (not (at-end-p
                             (+ ,index
                                (setq ,len (the fixnum (,length ,thing))))
                             ,end))
                   (do* ((,i ,index (the fixnum (1+ ,i)))
                         (,j 0 (the fixnum (1+ ,j)))
                         (,x ,len (the fixnum (1- ,x))))
                        ((= 0 ,x) (+ ,index ,len))
                     (declare (fixnum ,i ,j ,x))
                     (if* ,char-equal
                        then (when (not (char-equal (,schar ,string ,i)
                                                    (,schar ,thing  ,j)))
                               (return nil))
                        else (when (not (char= (,schar ,string ,i)
                                               (,schar ,thing  ,j)))
                               (return nil)))))
          elseif (simple-bit-vector-p ,thing) ;; a LOT faster than bit-vector-p
            then (let ((,c (char-code (,schar ,string ,index))))
                   (if* (< ,c +uri-bit-vector-size+)
                      then (when (char-included-p ,thing ,c)
                             (the fixnum (1+ ,index)))
                    elseif (and %iri-mode
                                (or
                                   ;; If the ucschar or iprivate booleans are set,
                                   ;; then check for characters in those ranges.
                                 (and (= 1 (sbit ,thing #.+bitvector-index-ucschar+))
                                      (ucscharp ,c))
                                 (and (= 1 (sbit ,thing #.+bitvector-index-iprivate+))
                                      (iprivatep ,c))))
                      then (the fixnum (1+ ,index))))
            else (error "bad object: ~s." ,thing)))))

(defun ucscharp (code)
  (declare (type fixnum code) (optimize (safety 1)))
  ;; This is straight from the grammer in RFC 3987, for ucschar.
  (or (<= #x000A0 code #x0D7FF)
      (<= #x0F900 code #x0FDCF)
      (<= #x0FDF0 code #x0FFEF)
      (<= #x10000 code #x1FFFD)
      (<= #x20000 code #x2FFFD)
      (<= #x30000 code #x3FFFD)
      (<= #x40000 code #x4FFFD)
      (<= #x50000 code #x5FFFD)
      (<= #x60000 code #x6FFFD)
      (<= #x70000 code #x7FFFD)
      (<= #x80000 code #x8FFFD)
      (<= #x90000 code #x9FFFD)
      (<= #xA0000 code #xAFFFD)
      (<= #xB0000 code #xBFFFD)
      (<= #xC0000 code #xCFFFD)
      (<= #xD0000 code #xDFFFD)
      (<= #xE1000 code #xEFFFD)))

(defun iprivatep (code)
  (declare (fixnum code) (optimize (safety 1)))
  ;; This is straight from the grammer in RFC 3987, for iprivate.
  (or (<= #x00E000 code #x00F8FF)
      (<= #x0F0000 code #x0FFFFD)
      (<= #x100000 code #x10FFFD)))

;; Future optimization from rfr:
;;   If THING is going to be a string very often,
;;   then you might get a useful speed improvement by splitting this
;;   again based on char-equal true/false. As it is, you're generating
;;   code in .looking-at that checks the char-equal argument on every
;;   character.
(defun looking-at (thing string index end
                   ;; optional because it is rarely given
                   &optional char-equal)
  ;; Return a new index into the parse buffer (STRING), if
  ;; an object equivalent to THING exists at index INDEX.
  ;; THING can be a:
  ;;  - bit vector: if a bit vector, then check that at character
  ;;    code index for it, there is a `1'
  ;;  - string: check that the string is in STRING starting at INDEX
  ;;  - character: check that the character is in STRING starting at
  ;;    INDEX
  ;; If CHAR-EQUAL is non-nil, then do character comparisons
  ;; case insensitively with CHAR-EQUAL.
  (declare (type fixnum index end) (optimize (safety 1)))
  ;; The simple-string version is much faster, so this is worth the
  ;; complexity.
  ;;
  ;; NOTE: .looking-at takes ONLY symbols. The macro is not hygenic.
  (if* (simple-string-p string)
     then (.looking-at t   thing string index end char-equal)
     else (.looking-at nil thing string index end char-equal)))

(defun scan-forward (string start end bitvector
                     &optional func)
  ;; Scan STRING using BITVECTOR for matching, starting from position
  ;; START, and going no farther than END.
  ;; Return the index of the first non-matching character, or nil if no
  ;; characters matched.
  ;;
  ;; If BITVECTOR does not match, then call FUNC with three arguments
  ;; (STRING, <index>, and END).  If the FUNC returns nil, then scanning
  ;; terminates and this function returns <index>, if it is > START.
  (declare (type fixnum start end)
           (type (or function null) func)
           (optimize (safety 1)))
  (do ((i start)
       (new-i nil))
      ((= end i)
       (if* (= i start)
          then nil
          else i))
    (declare (fixnum i))
    (cond
      ((looking-at bitvector string i end)
       ;; Advance
       (incf i))
      (func
       ;; BITVECTOR failed.
       (if* (setq new-i (funcall func string i end))
          then ;; FUNC return non-nil, advance I and keep going...
               (setq i new-i)
          else ;; FUNC return NIL, we're done
               (if* (= i start)
                  then ;; Nothing matched => NIL:
                       (return nil)
                  else ;; Something matched => first index that didn't:
                       (return i))))
      (t
       ;; BITVECTOR didn't match.  We're done.
       (if* (= i start)
          then ;; Nothing matched:
               (return nil)
          else ;; Something matched, first index that didn't:
               (return i))))))
