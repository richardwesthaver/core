;;; dat/qrcode.lisp --- QR Code formats

;; see https://github.com/jnjcc/cl-qrencode

;; Copyright (c) 2011-2014 jnjcc, Yste.org. All rights reserved.

;;; Code:
(in-package :dat/qrcode)

(defun read-file-content (fpath)
  (with-open-file (fp fpath)
    (let ((content (make-string (file-length fp))))
      (read-sequence content fp)
      content)))

;;;; Galois Field with primitive element 2, as used by Reed-Solomon code
(defclass galois ()
  ((power :initform nil :initarg :power :reader gf-power
          :documentation "Galois Field GF(2^POWER)")
   (prime-poly :initform nil :initarg :ppoly :reader prime-poly
               :documentation "prime polynomial")
   (order :initform nil :reader gf-order)
   (exp-table :initform nil)
   (log-table :initform nil)))

(defmethod initialize-instance :after ((gf galois) &rest args)
  (declare (ignore args))
  (setf (slot-value gf 'order) (ash 1 (slot-value gf 'power)))
  (let* ((order (gf-order gf))
         (ppoly (prime-poly gf))
         ;; 2^0 = 1 && (log 0) = -1
         (exptab (make-array order :initial-element 1))
         (logtab (make-array order :initial-element -1)))
    (do ((i 1 (1+ i)))
        ((>= i order))
      (setf (aref exptab i) (* (aref exptab (- i 1)) 2))
      (when (>= (aref exptab i) order)
        (setf (aref exptab i)
              (boole boole-and (- order 1)
                     (boole boole-xor (aref exptab i) ppoly))))
      (setf (aref logtab (aref exptab i)) i))
    (setf (aref logtab 1) 0)
    (setf (slot-value gf 'exp-table) exptab)
    (setf (slot-value gf 'log-table) logtab)))

;;; value accessor
(defgeneric gf-exp (gf pow)
  (:documentation "2^POW under Galois Field GF"))
(defgeneric gf-log (gf value)
  (:documentation "VALUE should be within range [0, 2^POW - 1]"))

(defmethod gf-exp ((gf galois) pow)
  (let* ((sz (- (gf-order gf) 1))
         (idx (mod pow sz)))
    (aref (slot-value gf 'exp-table) idx)))

(defmethod gf-log ((gf galois) value)
  (let* ((sz (gf-order gf))
         (idx (mod value sz)))
    (aref (slot-value gf 'log-table) idx)))

;;; Galois Field arithmetic
(defgeneric gf-add (gf a b))
(defgeneric gf-subtract (gf a b))
(defgeneric gf-multiply (gf a b))
(defgeneric gf-divide (gf a b))

(defmethod gf-add ((gf galois) a b)
  (boole boole-xor a b))

(defmethod gf-subtract ((gf galois) a b)
  (boole boole-xor a b))

(defmethod gf-multiply ((gf galois) a b)
  (let ((sum (+ (gf-log gf a) (gf-log gf b))))
    (gf-exp gf sum)))

(defmethod gf-divide ((gf galois) a b)
  (when (= b 0)
    (error "divide by zero"))
  (if (= a 0)
      0
      (let ((sub (- (gf-log gf a) (gf-log gf b))))
        (gf-exp gf sub))))

;;; open-paren at beg of line confuses `slime-compile-defun` which uses
;;; elisp function `beginning-of-defun`, which in turn involves
;;; backward-searching open-paren at beg of line
;;;   there seems to be no easy way to fix this problem
;; with an extra leading '\', docstring is kind of ulgy now, though
(defmacro with-gf-accessors (accessors gf &body body)
  "shortcuts for gf-exp & gf-log, usage:
\(with-gf-accessors ((gfexp gf-exp)) *gf-instance* ...)"
  `(labels ,(mapcar (lambda (acc-entry)
                      (let ((acc-name (car acc-entry))
                            (method-name (cadr acc-entry)))
                        `(,acc-name (a)
                                    (,method-name ,gf a))))
                    accessors)
     ,@body))

(defmacro with-gf-arithmetics (ariths gf &body body)
  "shortcuts for gf-add, gf-subtract, gf-multiply & gf-divide, usage:
\(with-gf-arithmetics ((gf+ gf-add)) *gf-instance* ...)"
  `(labels ,(mapcar (lambda (arith-entry)
                      (let ((arith-name (car arith-entry))
                            (method-name (cadr arith-entry)))
                        `(,arith-name (a b)
                                      (,method-name ,gf a b))))
                    ariths)
     ,@body))

(defmacro with-gf-shortcuts (accessors ariths gf &body body)
  "combined with-gf-accessors & with-gf-arithmetics, usage:
\(with-gf-shortcuts ((gflog gf-log)) ((gf* gf-multiply)) *gf-instance* ...)"
  `(labels ,(append
             (mapcar (lambda (acc-entry)
                       (let ((acc-name (car acc-entry))
                             (method-name (cadr acc-entry)))
                         `(,acc-name (a)
                                     (,method-name ,gf a))))
                     accessors)
             (mapcar (lambda (arith-entry)
                       (let ((arith-name (car arith-entry))
                             (method-name (cadr arith-entry)))
                         `(,arith-name (a b)
                                       (,method-name ,gf a b))))
                     ariths))
     ,@body))

;;;; Bose-Chaudhuri-Hocquenghem (BCH) error correction code

;;; Polynomial (using list) arithmetics
;;; by polynomial list (3 2 1), we mean 3*x^2 + 2*x + 1
(defun poly-ash (poly s)
  "shift left POLY by S"
  (declare (type list poly))
  (append poly (make-list s :initial-element 0)))
(defun poly-multiply (poly b &optional (op #'*))
  "multiply B on every element of POLY using OP"
  (labels ((mult (elem)
             (funcall op elem b)))
    (mapcar #'mult poly)))
(defun poly-substract (lhs rhs &optional (op #'-))
  (labels ((sub (elem1 elem2)
             (funcall op elem1 elem2)))
    (mapcar #'sub lhs rhs)))
(defun poly-mod (msg gen rem &optional (sub #'poly-substract) (mul #'poly-multiply))
  "MSG % GEN, with REM remainders"
  (labels ((cdrnzero (msg rem)
             (do ((head msg (cdr head)))
                 ((or (null head) (<= (length head) rem) (/= (car head) 0)) head)
               head)))
    (do ((m (poly-ash msg rem) (cdrnzero m rem)))
        ((<= (length m) rem) m)
      (let* ((glen (length gen))
             (sft (- (length m) glen))
             ;; LEAD coffiecient of message polynomial
             (lead (car m)))
        (setf m (funcall sub m (poly-ash (funcall mul gen lead) sft)))))))

(defclass bch-ecc ()
  ((k :initform nil :initarg :k
      :documentation "# of data codewords")
   (ec :initform nil :initarg :ec
       :documentation "# of error correction codewords")))

(defun bch* (poly b)
  (poly-multiply poly b))
(defun bch- (lhs rhs)
  (labels ((xor (a b)
             (boole boole-xor a b)))
    (poly-substract lhs rhs #'xor)))
(defun bch-xor (lhs rhs)
  (labels ((xor (a b)
             (boole boole-xor a b)))
    (mapcar #'xor lhs rhs)))
(defun bch% (msg gen rem)
  (poly-mod msg gen rem #'bch- #'bch*))

(defgeneric bch-ecc (bch msgpoly genpoly)
  (:documentation "do bch error correction under BCH(K+EC, K)"))

(defmethod bch-ecc ((bch bch-ecc) msg gen)
  (with-slots (k ec) bch
    (unless (= (length msg) k)
      (error "wrong msg length, expect: ~A; got: ~A~%" k (length msg)))
    (bch% msg gen ec)))

;;; As used by format information ecc & version information ecc respectively
;;; BCH(15, 5) & BCH(18, 6)
(let ((fi-ecc (make-instance 'bch-ecc :k 5 :ec 10))
      ;; format information generator polynomial
      ;; x^10 + x^8 + x^5 + x^4 + x^2 + x + 1
      (fi-gpoly '(1 0 1 0 0 1 1 0 1 1 1))
      (fi-xor '(1 0 1 0 1 0 0 0 0 0 1 0 0 1 0)))
  (defun format-ecc (level mask-ind)
    (let ((seq (append (level-indicator level)
                       (mask-pattern-ref mask-ind))))
      (bch-xor (append seq (bch-ecc fi-ecc seq fi-gpoly))
               fi-xor))))

(let ((vi-ecc (make-instance 'bch-ecc :k 6 :ec 12))
      ;; version information generator polynomial
      ;; x^12 + x^11 + x^10 + x^9 + x^8 + x^5 + x^2 + 1
      (vi-gpoly '(1 1 1 1 1 0 0 1 0 0 1 0 1)))
  (defun version-ecc (version)
    (let ((seq (decimal->bstream version 6)))
      (append seq (bch-ecc vi-ecc seq vi-gpoly)))))

(defclass rs-ecc ()
  ((k :initform nil :initarg :k
      :documentation "# of data codewords")
   (ec :initform nil :initarg :ec
       :documentation "# of error correction codewords")
   (gpoly :initform nil :reader gpoly
          :documentation "with EC, we calculate generator poly immediately")))

;;; Reed-Solomon code uses GF(2^8) with prime polynomial 285,
;;; or 1,0001,1101, or (x^8 + x^4 + x^3 + x^2 + 1)
(let ((gf256 (make-instance 'galois :power 8 :ppoly 285)))
  ;; Polynomial arithmetics under GF(2^8), as used by Reed-Solomon ecc
  (defun rs* (poly b)
    "multiply B on every element of POLY under GF(2^8)"
    (with-gf-arithmetics ((gf* gf-multiply)) gf256
      (poly-multiply poly b #'gf*)))
  (defun rs- (lhs rhs)
    (with-gf-arithmetics ((gf- gf-subtract)) gf256
      (poly-substract lhs rhs #'gf-)))
  (defun rs% (msg gen rem)
    (poly-mod msg gen rem #'rs- #'rs*))

  (defmethod initialize-instance :after ((rs rs-ecc) &rest args)
    (declare (ignore args))
    (setf (slot-value rs 'gpoly) (gen-poly rs)))

  (defgeneric gen-poly (rs))
  (defmethod gen-poly ((rs rs-ecc))
    "Generator Polynomial: (x-a^0) * (x-a^1) * ... * (x-a^(ec-1))"
    (with-slots (ec) rs
      (let* ((size (+ ec 1))
             (poly (make-list size :initial-element nil)))
        (with-gf-shortcuts ((gfexp gf-exp)) ((gf+ gf-add) (gf* gf-multiply)) gf256
          (setf (nth 0 poly) 1
                (nth 1 poly) 1)
          (do ((i 2 (1+ i)))
              ((> i ec) poly)
            (setf (nth i poly) 1)
            (do ((j (- i 1) (1- j)))
                ((<= j 0))
              (if (not (= (nth j poly) 0))
                  (setf (nth j poly)
                        (gf+ (nth (- j 1) poly)
                             (gf* (nth j poly) (gfexp (- i 1)))))
                  (setf (nth j poly) (nth (- j 1) poly))))
            (setf (nth 0 poly) (gf* (nth 0 poly) (gfexp (- i 1))))))
        (reverse poly))))

  (defgeneric gen-poly-gflog (rs))
  (defgeneric ecc-poly (rs msg))

  (defmethod gen-poly-gflog ((rs rs-ecc))
    (with-gf-accessors ((gflog gf-log)) gf256
      ;; GPOLY already calculated when making new instance
      (mapcar #'gflog (gpoly rs))))

  (defmethod ecc-poly ((rs rs-ecc) msg-poly)
    "Error Correction codewords Polynomial for MSG-POLY"
    (with-slots (k ec gpoly) rs
      (unless (= (length msg-poly) k)
        (error "wrong msg-poly length, expect: ~A~%" k))
      (rs% msg-poly gpoly ec))))

(deftype qr-mode ()
  '(member :unknown
    :numeric :alnum :byte :kanji
    ;; Extended Channel Interpretation, Structured Append, FNC1
    :eci :structured :fnc1))

(defun mode-indicator (mode)
  (declare (type qr-mode mode))
  (case mode
    (:numeric '(0 0 0 1)) ; "0001"
    (:alnum '(0 0 1 0))   ; "0010"
    (:byte '(0 1 0 0))    ; "0100"
    (:kanji '(1 0 0 0))   ; "1000"
    (:eci '(0 1 1 1))     ; "0111"
    (:structured '(0 0 1 1)) ; "0011"
    (:fnc1 '(0 1 0 1))))  ; FIX: "0101" & "1001"

(defun terminator (bstream version level)
  "End of message"
  (let* ((nbits (length bstream))
         (diff (- (* (data-words-capacity version level) 8)
                  nbits)))
    (cond
      ((< diff 0) (error "you serious about this?!"))
      ((<= diff 4) (make-list diff :initial-element 0))
      (t (make-list 4 :initial-element 0)))))

(defun byte-value (mode byte)
  "BYTE value under MODE"
  (declare (type qr-mode mode))
  (case mode
    (:numeric
     (and (<= #x30 byte #x39)
          (- byte #x30)))
    (:alnum
     (cond
       ((<= #x30 byte #x39) (- byte #x30)) ; 0-9
       ((<= #x41 byte #x5A) (+ (- byte #x41) 10)) ; A-Z
       ((= byte #x20) 36) ; SP
       ((= byte #x24) 37) ; $
       ((= byte #x25) 38) ; %
       ((= byte #x2A) 39) ; *
       ((= byte #x2B) 40) ; +
       ((= byte #x2D) 41) ; -
       ((= byte #x2E) 42) ; .
       ((= byte #x2F) 43) ; /
       ((= byte #x3A) 44) ; :
       (t nil)))
    ((:byte :kanji) byte)))

(defun kanji-word-p (word)
  "(kanji-p, kanji-range: {0, 1})"
  (cond
    ((<= #x8140 word #x9ffc) (values t 0))
    ((<= #xe040 word #xebbf) (values t 1))
    (t (values nil nil))))

(defun starts-kanji-p (bytes)
  "(BYTES starts with kanji-p, kanji word value, kanji-range: {0, 1})"
  (declare (type list bytes))
  (let* ((first (car bytes))
         (second (cadr bytes))
         (word (and second (+ (ash first 8) second))))
    (if (and first second)
        (multiple-value-bind (kanji-p range)
            (kanji-word-p word)
          (values kanji-p word range))
        (values nil nil nil))))

(defun xor-subset-of (bytes)
  "exclusive subset of first unit of BYTES.
as for unit, one byte for :numeric, :alnum; two bytes for :kanji"
  (declare (type list bytes))
  (let* ((first (car bytes)))
    (cond
      ((null first) :unknown)
      ((byte-value :numeric first) :numeric)
      ((byte-value :alnum first) :alnum)
      ;; excluding reserved values 80-9F & E0-FF
      ((and (not (<= #x80 first #x9F))
            (not (<= #xE0 first #xFF)))
       :byte)
      ((starts-kanji-p bytes)
       :kanji))))

(defclass qr-input ()
  ((bytes
    :initform nil :initarg :bytes :reader bytes :type list
    :documentation "list of bytes to be encoded")
   (version
    :initform 1 :initarg :version :reader version
    :documentation "version of qr symbol, adapted according to BYTES")
   (ec-level ; cannot be NIL
    :initform :level-m :initarg :ec-level :reader level :type ecc-level)
   (mode
    :initform nil :initarg :mode :reader mode :type (or null qr-mode)
    :documentation "if supplied, we force all BYTES to be under MODE,
therefore, unless you know exactly what you are doing, leave this NIL")
   (cur-byte
    :initform 0 :accessor cur-byte
    :documentation "index of BYTES during data analysis")
   (segments
    :initform nil :accessor segments :type list
    :documentation
    "list of list, of the form ((:mode1 byte ...) (:mode2 byte ...) ...)")
   (bstream
    :initform nil :reader bstream :type list
    :documentation "list of 0-1 values after encoding SEGMENTS")
   (blocks
    :initform nil :reader blocks :type list
    :documentation "list of list, of the form ((codeword ...) (codeword ...) ...)
after converting BSTREAM to codewords")
   (ecc-blocks ; error correction blocks
    :initform nil :reader ecc-blocks :type list
    :documentation "list of list, ec codewords corresponding to BLOCKS")
   (msg-codewords
    :initform nil :reader message :type list
    :documentation "list of codewords from BLOCKS & ECC-BLOCKS,
interleaving if neccessary")
   (matrix
    :initform nil :accessor matrix
    :documentation "raw QR code symbol (without masking) as matrix")))

(defmethod initialize-instance :after ((input qr-input) &rest args)
  (declare (ignore args))
  (validate-and-analysis input))

;;; 0) Data analysis
(defgeneric validate-and-analysis (input)
  (:documentation "adapt VERSION according to BYTES, and fill SEGMENTS slot"))
;;; 1) Data encoding
(defgeneric data-encoding (input)
  (:documentation "encode SEGMENTS into BSTREAM slot"))
;;; 2) Error correction coding
(defgeneric ec-coding (input)
  (:documentation "split BSTREAM into BLOCKS, do rs-ecc, and fill ECC-BLOCKS"))
;;; 3) Structure final message
(defgeneric structure-message (input)
  (:documentation "interleaving BLOCKS and ECC-BLOCKS into MSG-CODEWORDS"))
;;; 4) Codeword placement in matrix, a.k.a, raw QR code symbol
(defgeneric module-placement (input)
  (:documentation "write MSG-CODEWORDS into the raw (without masking) MATRIX"))
;;; 5) Data masking & Format information
(defgeneric data-masking (input)
  (:documentation "mask MATRIX with best pattern, generate the final symbol"))

(defgeneric data-analysis (input)
  (:documentation "BYTES -> SEGMETS, switch bewteen modes as necessary to
achieve the most efficient conversion of data"))
(defgeneric redo-data-analysis (input)
  (:documentation "VERSION changed, reset CUR-BYTE and redo data analysis"))
(defgeneric analyse-byte-mode (input &optional seg))
(defgeneric analyse-alnum-mode (input &optional seg))
(defgeneric analyse-numeric-mode (input &optional seg))
(defgeneric analyse-kanji-mode (input &optional seg))
(defgeneric append-cur-byte (input &optional seg)
  (:documentation "append CUR-BYTE of BYTES into SEGMENTS"))
(defun mode-analyse-func (mode)
  "put CUR-BYTE into MODE, and then look at following BYTES for new segment"
  (case mode
    (:byte #'analyse-byte-mode)
    (:alnum #'analyse-alnum-mode)
    (:numeric #'analyse-numeric-mode)
    (:kanji #'analyse-kanji-mode)))

(defmethod data-analysis ((input qr-input))
  (with-slots (mode cur-byte segments) input
    (when mode ; MODE supplied
      (let ((seg (append (list mode) (bytes input))))
        (setf cur-byte (length (bytes input)))
        (setf segments (append segments (list seg))))
      (return-from data-analysis)))
  (with-slots (bytes version segments) input
    (let ((init-mode (select-init-mode bytes version)))
      (funcall (mode-analyse-func init-mode) input))))

(defmethod redo-data-analysis ((input qr-input))
  (with-slots (cur-byte segments) input
    (setf cur-byte 0)
    (setf segments nil)
    (data-analysis input)))

(defun select-init-mode (bytes version)
  "optimization of bitstream length: select initial mode"
  (declare (type list bytes))
  (let ((init-xor (xor-subset-of bytes)))
    (case init-xor
      (:byte :byte)
      (:kanji
       (case (xor-subset-of (nthcdr 2 bytes))
         ((:numeric :alnum) :kanji)
         (:byte
          (let ((nunits (ecase (version-range version)
                          ((0 1) 5)
                          (2 6))))
            (if (every-unit-matches (nthcdr 3 bytes) 2 nunits :kanji)
                :byte
                :kanji)))
         (otherwise :kanji)))
      (:alnum
       (let ((nunits (ecase (version-range version)
                       (0 6) (1 7) (2 8))))
         ;; number of units (characters) match :alnum, followed by a :byte unit
         (multiple-value-bind (n last-mode) (nunits-matches (cdr bytes) :alnum)
           (if (and (< n nunits) (eq last-mode :byte))
               :byte
               :alnum))))
      (:numeric
       (let ((nbunits (ecase (version-range version)
                        ((0 1) 4) (2 5)))
             (naunits (ecase (version-range version)
                        (0 7) (1 8) (2 9))))
         (multiple-value-bind (n last-mode) (nunits-matches (cdr bytes) :numeric)
           (if (and (< n nbunits) (eq last-mode :byte))
               :byte
               (if (and (< n naunits) (eq last-mode :alnum))
                   :alnum
                   :numeric))))))))

;;; UNIT: character under a certain mode,
;;;   a byte under :numeric :alnum & :byte, or a byte-pair under :kanji
(defun every-unit-matches (bytes usize nunits mode)
  "if every unit of USZIE bytes (at most NUNITS unit) within BYTES matches MODE"
  (declare (type list bytes) (type qr-mode mode))
  (when (>= (length bytes) (* usize nunits))
    (dotimes (i nunits)
      (let ((b (nthcdr (* usize i) bytes)))
        (unless (eq (xor-subset-of b) mode)
          (return-from every-unit-matches nil))))
    (return-from every-unit-matches t)))

(defun nunits-matches (bytes mode)
  "(number of units that matches MODE, and mode for the first unmatched unit)"
  (declare (type list bytes) (type qr-mode mode))
  (let ((usize (ecase mode
                 ((:byte :alnum :numeric) 1)
                 ;; as for :kanji, 2 bytes forms a single unit
                 (:kanji 2)))
        (nunits 0))
    (do ((b bytes (nthcdr usize b)))
        ((or (null b)
             (not (eq (xor-subset-of b) mode)))
         (values nunits (xor-subset-of b)))
      (incf nunits))))

(defmethod analyse-byte-mode ((input qr-input) &optional (seg '(:byte)))
  (declare (type list seg))
  (setf seg (append-cur-byte input seg))
  (unless seg
    (return-from analyse-byte-mode))
  (with-slots (bytes cur-byte version segments) input
    (let* ((range (version-range version))
           (nkunits (ecase range ; number of :kanji units before more :byte
                      (0 9) (1 12) (2 13)))
           (nanuits (ecase range ; number of :alnum units before more :byte
                      (0 11) (1 15) (2 16)))
           (nmunits1 (ecase range ; number of :numeric units before more :byte
                       (0 6) (1 8) (2 9)))
           (nmunits2 (ecase range ; number of :numeric units before more :alnum
                       (0 6) (1 7) (2 8)))
           (switch-mode nil))
      (multiple-value-bind (nmatches last-mode)
          (nunits-matches (nthcdr cur-byte bytes) :kanji)
        (and (>= nmatches nkunits) (eq last-mode :byte)
             (setf switch-mode :kanji)))
      (unless switch-mode
        (multiple-value-bind (nmatches last-mode)
            (nunits-matches (nthcdr cur-byte bytes) :alnum)
          (and (>= nmatches nanuits) (eq last-mode :byte)
               (setf switch-mode :alnum))))
      (unless switch-mode
        (multiple-value-bind (nmatches last-mode)
            (nunits-matches (nthcdr cur-byte bytes) :numeric)
          (case last-mode
            (:byte (and (>= nmatches nmunits1)
                        (setf switch-mode :numeric)))
            (:alnum (and (>= nmatches nmunits2)
                         (setf switch-mode :numeric))))))
      (if switch-mode
          (progn
            ;; current segment finished, add a new SWITCH-MODE segment
            (setf segments (append segments (list seg)))
            (setf seg (list switch-mode)))
          (setf switch-mode :byte))
      (funcall (mode-analyse-func switch-mode) input seg))))

(defmethod analyse-alnum-mode ((input qr-input) &optional (seg '(:alnum)))
  (declare (type list seg))
  (setf seg (append-cur-byte input seg))
  (unless seg
    (return-from analyse-alnum-mode))
  (with-slots (bytes cur-byte version segments) input
    (let ((nmunits (ecase (version-range version)
                     (0 13) (1 15) (2 17)))
          (switch-mode nil))
      (when (>= (nunits-matches (nthcdr cur-byte bytes) :kanji) 1)
        (setf switch-mode :kanji))
      (unless switch-mode
        (when (>= (nunits-matches (nthcdr cur-byte bytes) :byte) 1)
          (setf switch-mode :byte)))
      (unless switch-mode
        (multiple-value-bind (nmatches last-mode)
            (nunits-matches (nthcdr cur-byte bytes) :numeric)
          (and (>= nmatches nmunits) (eq last-mode :alnum)
               (setf switch-mode :numeric))))
      (if switch-mode
          (progn
            (setf segments (append segments (list seg)))
            (setf seg (list switch-mode)))
          (setf switch-mode :alnum))
      (funcall (mode-analyse-func switch-mode) input seg))))

(defmethod analyse-numeric-mode ((input qr-input) &optional (seg '(:numeric)))
  (declare (type list seg))
  (setf seg (append-cur-byte input seg))
  (unless seg
    (return-from analyse-numeric-mode))
  (with-slots (bytes cur-byte version segments) input
    (let ((switch-mode nil))
      (when (>= (nunits-matches (nthcdr cur-byte bytes) :kanji) 1)
        (setf switch-mode :kanji))
      (unless switch-mode
        (when (>= (nunits-matches (nthcdr cur-byte bytes) :byte) 1)
          (setf switch-mode :byte)))
      (unless switch-mode
        (when (>= (nunits-matches (nthcdr cur-byte bytes) :alnum) 1)
          (setf switch-mode :alnum)))
      (if switch-mode
          (progn
            (setf segments (append segments (list seg)))
            (setf seg (list switch-mode)))
          (setf switch-mode :numeric))
      (funcall (mode-analyse-func switch-mode) input seg))))

(defmethod append-cur-byte ((input qr-input) &optional seg)
  "if CUR-BYTE is the last byte, return nil"
  (declare (type list seg))
  (with-slots (bytes cur-byte segments) input
    (setf seg (append seg (list (nth cur-byte bytes))))
    (incf cur-byte)
    (when (>= cur-byte (length bytes))
      (setf segments (append segments (list seg)))
      (setf seg nil))
    (return-from append-cur-byte seg)))

(defmethod analyse-kanji-mode ((input qr-input) &optional (seg '(:kanji)))
  (declare (type list seg))
  (with-slots (bytes cur-byte segments) input
    (setf seg (append seg (nthcdr cur-byte bytes)))
    (setf cur-byte (length bytes))
    (setf segments (append segments (list seg)))))

(defmethod validate-and-analysis ((input qr-input))
  (with-slots ((level ec-level) segments) input
    (unless (<= 1 (version input) 40)
      (error "version ~A out of bounds" (version input)))
    (do ((prev -1))
        ((<= (version input) prev))
      (setf prev (version input))
      (redo-data-analysis input)
      (labels ((seg-bstream-len (seg)
                 (segment-bstream-length seg (version input))))
        (let* ((blen (reduce #'+ (mapcar #'seg-bstream-len segments)
                             :initial-value 0))
               (min-v (minimum-version prev (ceiling blen 8) level)))
          (if min-v
              (setf (slot-value input 'version) min-v)
              (error "no version to hold ~A bytes" (ceiling blen 8))))))))

(defmethod data-encoding ((input qr-input))
  (with-slots (version (level ec-level) segments) input
    (labels ((seg->bstream (seg)
               (segment->bstream seg version)))
      (let* ((bs (reduce #'append (mapcar #'seg->bstream segments)
                         :initial-value nil))
             (tt (terminator bs version level))
             ;; connect bit streams in all segment, with terminator appended
             (bstream (append bs tt)))
        ;; add padding bits
        (setf bstream (append bstream (padding-bits bstream)))
        ;; add pad codewords, finishes data encoding
        (setf (slot-value input 'bstream)
              (append bstream
                      (pad-codewords bstream version level)))))))

(defmethod ec-coding ((input qr-input))
  (with-slots (version (level ec-level) bstream) input
    (let ((codewords (bstream->codewords bstream))
          (blocks nil)
          (ecc-blocks nil)
          ;; RS error correction obj for blk1 & blk2
          (rs1 nil)
          (rs2 nil))
      (multiple-value-bind (ecc-num blk1 data1 blk2 data2)
          (ecc-block-nums version level)
        (when (> blk1 0)
          (setf rs1 (make-instance 'rs-ecc :k data1 :ec ecc-num)))
        (when (> blk2 0)
          (setf rs2 (make-instance 'rs-ecc :k data2 :ec ecc-num)))
        (dotimes (i blk1)
          (setf blocks
                (append blocks (list (subseq codewords 0 data1))))
          (setf codewords (nthcdr data1 codewords)))
        (dotimes (i blk2)
          (setf blocks
                (append blocks (list (subseq codewords 0 data2))))
          (setf codewords (nthcdr data2 codewords)))
        (dotimes (i blk1)
          (setf ecc-blocks
                (append ecc-blocks (list (ecc-poly rs1 (nth i blocks))))))
        (dotimes (i blk2)
          (setf ecc-blocks
                (append ecc-blocks (list (ecc-poly rs2 (nth (+ i blk1) blocks))))))
        (setf (slot-value input 'blocks) blocks)
        (setf (slot-value input 'ecc-blocks) ecc-blocks)))))

(defmethod structure-message ((input qr-input))
  (with-slots (version (level ec-level) blocks ecc-blocks) input
    (let ((final nil))
      (multiple-value-bind (ecc-num blk1 data1 blk2 data2)
          (ecc-block-nums version level)
        (declare (ignore ecc-num))
        (setf (slot-value input 'msg-codewords)
              (append final
                      ;; interleave data blocks, data blocks may differ in length
                      (take-data-in-turn blocks blk1 data1 blk2 data2)
                      ;; we know error correction blocks are of the same length
                      (take-in-turn ecc-blocks)))))))

(defmethod module-placement ((input qr-input))
  (setf (matrix input) (make-matrix (version input)))
  (with-slots (version msg-codewords matrix) input
    ;; Function pattern placement
    (function-patterns matrix version)
    ;; Symbol character placement
    (let ((rbits (remainder-bits version))
          (bstream nil))
      (labels ((dec->byte (codeword)
                 (decimal->bstream codeword 8)))
        (setf bstream (append (reduce #'append (mapcar #'dec->byte msg-codewords))
                              ;; data capacity of _symbol_ does not divide by 8
                              (make-list rbits :initial-element 0))))
      (symbol-character bstream matrix version))))

(defmethod data-masking ((input qr-input))
  "(masked matrix, mask pattern reference)"
  (with-slots (version (level ec-level) matrix) input
    (let ((modules (matrix-modules version)))
      (multiple-value-bind (masked indicator)
          (choose-masking matrix modules level)
        (values masked (mask-pattern-ref indicator))))))

(defun decimal->bstream (dec nbits)
  "using NBITS bits to encode decimal DEC"
  (let ((bstream nil))
    (dotimes (i nbits)
      (if (logbitp i dec)
          (push 1 bstream)
          (push 0 bstream)))
    bstream))
(defun bstream->decimal (bstream nbits)
  (declare (type list bstream))
  (let ((nbits (min nbits (length bstream)))
        (dec 0))
    (dotimes (i nbits)
      (setf dec (+ (* dec 2) (nth i bstream))))
    dec))

;;; :numeric mode
(defun group->decimal (values ndigits)
  "digit groups of length NDIGITS (1, 2 or 3) to decimal"
  (declare (type list values))
  (case ndigits
    (1 (nth 0 values))
    (2 (+ (* (nth 0 values) 10) (nth 1 values)))
    (3 (+ (* (nth 0 values) 100) (* (nth 1 values) 10) (nth 2 values)))))
(defun final-digit-bits (n)
  "the final one or two digits are converted to 4 or 7 bits respectively"
  (case n
    (0 0) (1 4) (2 7)))
(defun numeric->bstream (bytes)
  (declare (type list bytes))
  (labels ((num-value (byte)
             (byte-value :numeric byte)))
    (let ((values (mapcar #'num-value bytes))
          (bstream nil))
      (do ((v values (nthcdr 3 v)))
          ((null v) bstream)
        (case (length v)
          (1 ; only 1 digits left
           (setf bstream
                 (append bstream (decimal->bstream (group->decimal v 1)
                                                   (final-digit-bits 1)))))
          (2 ; only 2 digits left
           (setf bstream
                 (append bstream (decimal->bstream (group->decimal v 2)
                                                   (final-digit-bits 2)))))
          (otherwise ; at least 3 digits left
           (setf bstream
                 (append bstream
                         (decimal->bstream (group->decimal v 3) 10)))))))))

;;; :alnum mode
(defun pair->decimal (values num)
  "alnum pairs of length NUM (1 or 2) to decimal"
  (declare (type list values))
  (case num
    (1 (nth 0 values))
    (2 (+ (* (nth 0 values) 45) (nth 1 values)))))
(defun alnum->bstream (bytes)
  (declare (type list bytes))
  (labels ((alnum-value (byte)
             (byte-value :alnum byte)))
    (let ((values (mapcar #'alnum-value bytes))
          (bstream nil))
      (do ((v values (nthcdr 2 v)))
          ((null v) bstream)
        (case (length v)
          (1 ; only 1 alnum left
           (setf bstream
                 (append bstream
                         (decimal->bstream (pair->decimal v 1) 6))))
          (otherwise ; at least 2 alnum left
           (setf bstream
                 (append bstream
                         (decimal->bstream (pair->decimal v 2) 11)))))))))

;;; :byte mode
(defun byte->bstream (bytes)
  (declare (type list bytes))
  (labels ((join (prev cur)
             (append prev (decimal->bstream (byte-value :byte cur) 8))))
    (reduce #'join bytes :initial-value nil)))

;;; :kanji mode
(defun kanji->decimal (word range)
  (let ((subtractor (ecase range
                      (0 #x8140)
                      (1 #xc140))))
    (decf word subtractor)
    (setf word (+ (* (ash word -8) #xc0)
                  (boole boole-and word #xff)))))
(defun kanji->bstream (bytes)
  (declare (type list bytes))
  (labels ((kanji-value (byte)
             (byte-value :kanji byte)))
    (let ((values (mapcar #'kanji-value bytes))
          (delta 1)
          (bstream nil))
      (do ((v values (nthcdr delta v)))
          ((null v) bstream)
        (case (length v)
          (1 ; only 1 byte left
           (setf bstream
                 (append bstream (decimal->bstream (car v) 13)))
           (setf delta 1))
          (otherwise ; at least 2 bytes left
           (multiple-value-bind (kanji-p word range) (starts-kanji-p v)
             (if kanji-p
                 (progn
                   (setf bstream
                         (append bstream
                                 (decimal->bstream (kanji->decimal word range)
                                                   13)))
                   (setf delta 2))
                 (progn
                   (setf bstream
                         (append bstream (decimal->bstream (car v) 13)))
                   (setf delta 1))))))))))

;;; :eci mode
(defun eci->bstream (bytes)
  "TODO"
  (declare (ignore bytes))
  (error "eci->bstream: TODO..."))

(defun bstream-trans-func (mode)
  (case mode
    (:numeric #'numeric->bstream)
    (:alnum #'alnum->bstream)
    (:byte #'byte->bstream)
    (:kanji #'kanji->bstream)))

(defun kanji-bytes-length (bytes)
  (declare (type list bytes))
  (let ((step 1)
        (len 0))
    (do ((b bytes (nthcdr step b)))
        ((null b) len)
      (if (starts-kanji-p b)
          (setf step 2)
          (setf step 1))
      (incf len))))

(defun bytes-length (bytes mode)
  "number of data characters under MODE"
  (declare (type list bytes) (type qr-mode mode))
  (case mode
    ((:numeric :alnum :byte) (length bytes))
    (:kanji (kanji-bytes-length bytes))))

(defun segment-bstream-length (segment version)
  "bit stream length of SEGMENT (:mode b0 b1 ...) under VERSION"
  (declare (type list segment))
  (let* ((mode (car segment))
         (bytes (cdr segment))
         (m 4)
         (c (char-count-bits version mode))
         (d (bytes-length bytes mode))
         (r 0))
    ;; M = number of bits in mode indicator
    ;; C = number of bits in character count indicator
    ;; D = number of input data characters
    (case mode
      (:numeric
       (setf r (final-digit-bits (mod d 3)))
       ;; B = M + C + 10 * (D / 3) + R
       (+ m c (* 10 (floor d 3)) r))
      (:alnum
       (setf r (mod d 2))
       ;; B = M + C + 11 * (D / 2) + 6 * (D % 2)
       (+ m c (* 11 (floor d 2)) (* 6 r)))
      (:byte
       ;; B = M + C + 8 * D
       (+ m c (* 8 d)))
      (:kanji
       ;; B = M + C + 13 * D
       (+ m c (* 13 d))))))

(defun segment->bstream (segment version)
  "SEGMENT (:mode b0 b1 ...) to bit stream under VERSION"
  (declare (type list segment))
  (let* ((mode (car segment))
         (bytes (cdr segment))
         (len (bytes-length bytes mode))
         (n (char-count-bits version mode))
         (bstream nil))
    (append bstream (mode-indicator mode)
            (decimal->bstream len n) ; character count indicator
            (funcall (bstream-trans-func mode) bytes))))

(defun padding-bits (bstream)
  "add padding bits so that BSTREAM ends at a codeword boundary"
  (multiple-value-bind (quot rem) (ceiling (length bstream) 8)
    (declare (ignore quot))
    (make-list (- rem) :initial-element 0)))

(defun pad-codewords (bstream version level)
  "add pad codewords (after adding padding-bits) to fill data codeword capacity"
  (let ((pad-words '((1 1 1 0 1 1 0 0)
                     (0 0 0 1 0 0 0 1)))
        (pad-len (- (data-words-capacity version level)
                    (/ (length bstream) 8)))
        (ret nil))
    (dotimes (i pad-len)
      (setf ret (append ret (nth (mod i 2) pad-words))))
    ret))

(defun bstream->codewords (bstream)
  "convert bstream into codewords, as coefficients of the terms of a polynomial"
  (do ((b bstream (nthcdr 8 b))
       (codewords nil))
      ((null b) codewords)
    (setf codewords (append codewords (list (bstream->decimal b 8))))))

(defun take-in-turn (blks)
  "taking codewords from each block (bound by minimum length) in turn"
  (reduce #'append (apply #'mapcar #'list blks)))

(defun take-data-in-turn (blocks blk1 data1 blk2 data2)
  "taking data words from each block (might have different length) in turn"
  (let ((data-final nil)
        (left-blks nil))
    (setf data-final (take-in-turn blocks))
    (cond
      ((or (= blk1 0) (= blk2 0))
       ;; only one kind of block exists
       (setf left-blks nil))
      ((> data1 data2)
       ;; block 1 has more elements left
       (setf left-blks (mapcar #'(lambda (blk)
                                   (nthcdr data2 blk))
                               (subseq blocks 0 blk1))))
      ((> data2 data1)
       ;; block 2 has more elements left
       (setf left-blks (mapcar #'(lambda (blk)
                                   (nthcdr data1 blk))
                               (subseq blocks blk1 (+ blk1 blk2))))))
    (if left-blks
        (append data-final (take-in-turn left-blks))
        data-final)))

(deftype module-color ()
  ":RAW, nothing has been done to this module; :RESERVE, format info reserve module
:FLIGHT/:FDARK, function pattern light/dark module; :LIGHT/:DARK, data modules"
  '(member :raw :flight :fdark :reserve :light :dark))

(defun same-color-p (color1 color2)
  "during QR symbol evaluation, :fdark & :dark are considered to be same"
  (case color1
    ((:flight :light) (or (eq color2 :flight) (eq color2 :light)))
    ((:fdark :dark) (or (eq color2 :fdark) (eq color2 :fdark)))
    (otherwise (eq color1 color2))))

(defun raw-module-p (matrix i j)
  "nothing has been done to MATRIX[I, J]"
  (eq (aref matrix i j) :raw))

(defun make-modules-matrix (modules &optional (init :raw))
  "make a raw matrix with MODULES * MODULES elements"
  (make-array `(,modules ,modules) :initial-element init))

(defun make-matrix (version &optional (init :raw))
  "make a raw matrix according to VERSION"
  (let ((n (matrix-modules version)))
    (make-modules-matrix n init)))

(defun paint-square (matrix x y n &optional (color :fdark))
  "Paint a square of size N*N starting from upleft (X, Y) in MATRIX to COLOR"
  (let ((maxx (+ x n -1))
        (maxy (+ y n -1)))
    (loop for i from x to maxx do
         (loop for j from y to maxy do
              (setf (aref matrix i j) color))))
  matrix)

;;; Function Patterns
(defun function-patterns (matrix version)
  (let ((modules (matrix-modules version)))
    (finder-patterns matrix modules)
    (separator matrix modules)
    (timing-patterns matrix modules)
    (alignment-patterns matrix version))
  matrix)
;; a) Finder Patterns: fixed position in matrix
(defun one-finder-pattern (matrix x y)
  "Paint one finder pattern starting from upleft (X, Y)"
  (paint-square matrix x y 7 :fdark)
  (paint-square matrix (+ x 1) (+ y 1) 5 :flight)
  (paint-square matrix (+ x 2) (+ y 2) 3 :fdark))
(defun finder-patterns (matrix modules)
  ;; top-left finder pattern
  (one-finder-pattern matrix 0 0)
  ;; top-right finder pattern
  (one-finder-pattern matrix (- modules 7) 0)
  ;; bottom-left finder pattern
  (one-finder-pattern matrix 0 (- modules 7)))

;; b) Separator: fixed position in matrix
(defun separator (matrix modules)
  (dotimes (j 8)
    ;; top-left horizontal separator
    (setf (aref matrix 7 j) :flight)
    ;; top-right horizontal separator
    (setf (aref matrix 7 (- modules j 1)) :flight)
    ;; bottom-left horizontal separator
    (setf (aref matrix (- modules 8) j) :flight))
  (dotimes (i 8)
    ;; top-left vertical separator
    (setf (aref matrix i 7) :flight)
    ;; bottom-left vertical separator
    (setf (aref matrix (- modules i 1) 7) :flight)
    ;; top-right vertical separator
    (setf (aref matrix i (- modules 8)) :flight))
  matrix)

;; c) Timing patterns
(defun timing-patterns (matrix modules)
  (let ((color :fdark))
    (loop for idx from 8 to (- modules 9) do
         (if (evenp idx)
             (setf color :fdark)
             (setf color :flight))
         ;; Horizontal
         (setf (aref matrix 6 idx) color)
         ;; Vertical
         (setf (aref matrix idx 6) color)))
  matrix)

;; d) Alignment Patterns: varies between versions
;; may overlap timing patterns, modules coincide with that of timing patterns
(defun one-align-pattern (matrix x y)
  "Paint one alignment pattern centered at (X, Y)"
  (paint-square matrix (- x 2) (- y 2) 5 :fdark)
  (paint-square matrix (- x 1) (- y 1) 3 :flight)
  (paint-square matrix x y 1 :fdark))
(defun alignment-patterns (matrix version)
  (dolist (center (align-centers version) matrix)
    (one-align-pattern matrix (first center) (second center))))

;;; Encoding Region
(defun symbol-character (bstream matrix version)
  (let ((modules (matrix-modules version)))
    (reserve-information matrix version)
    (bstream-placement bstream matrix modules))
  matrix)
;; reserve format information & version information
(defun reserve-information (matrix version)
  (let ((modules (matrix-modules version)))
    ;; format information...
    ;; top-left & top-right horizontal
    (dotimes (j 8)
      (when (raw-module-p matrix 8 j)
        (setf (aref matrix 8 j) :reserve))
      (setf (aref matrix 8 (- modules j 1)) :reserve))
    (setf (aref matrix 8 8) :reserve)
    ;; top-left & bottom-left vertical
    (dotimes (i 8)
      (when (raw-module-p matrix i 8)
        (setf (aref matrix i 8) :reserve))
      (setf (aref matrix (- modules i 1) 8) :reserve))
    ;; dark module...
    (setf (aref matrix (- modules 8) 8) :fdark)

    ;; version information for version 7-40
    (when (>= version 7)
      (version-information matrix modules version))))

(defun paint-fcolor-bit (matrix i j bit)
  "Paint function pattern color for MATRIX[I, J] according to BIT of {0, 1}"
  (setf (aref matrix i j) (case bit
                            (0 :flight) (1 :fdark))))
(defun version-information (matrix modules version)
  "version information placement on two blocks of modules:
bottom-left 3*6 block: [modules-11, modules-9] * [0, 5]
top-right 6*3 block:   [0, 5] * [modules-11, modules-9]"
  (assert (>= version 7))
  (let ((vib (version-ecc version))
        (i (- modules 9))
        (start (- modules 9))
        (bound (- modules 11))
        (j 5))
    (dolist (bit vib matrix)
      (paint-fcolor-bit matrix i j bit)
      (paint-fcolor-bit matrix j i bit)
      (if (>= (- i 1) bound)
          (decf i)
          (progn
            (decf j)
            (setf i start))))))

;; Symbol character placement
(defun paint-color-bit (matrix i j bit)
  "Paint data color for MATRIX[I, J] according to BIT of {0, 1}"
  (setf (aref matrix i j) (case bit
                            (0 :light) (1 :dark))))
(defun bstream-placement (bstream matrix modules)
  "2X4 module block for a regular symbol character. Regard the interleaved
codeword sequence as a single bit stream, which is placed in the two module
wide columns, alternately in the right and left modules, moving upwards or
downwards according to DIRECTION, skipping function patterns, changing DIRECTION
at the top or bottom of the symbol. The only exception is that no block should
ever overlap the vertical timing pattern."
  (let ((i (- modules 1))
        (j (- modules 1))
        ;; -1: upwards, +1: downwards
        (direction -1)
        (len (length bstream)))
    (do ((idx 0))
        ((>= idx len) matrix)
      (when (raw-module-p matrix i j)
        (paint-color-bit matrix i j (nth idx bstream))
        (incf idx))
      (when (and (>= (- j 1) 0)
                 (raw-module-p matrix i (- j 1)))
        ;; try left module
        (paint-color-bit matrix i (- j 1) (nth idx bstream))
        (incf idx))
      (if (< -1 (+ i direction) modules)
          (incf i direction)
          (progn
            ;; reverse direction
            (setf direction (- direction))
            (if (= j 8)
                ;; vertical timing pattern reached, the next block starts
                ;; to the left of it
                (decf j 3)
                (decf j 2)))))))

;;; format information, during and after masking
(defun format-information (matrix modules level mask-ind)
  ;; format information bistream
  (let ((fib (format-ecc level mask-ind))
        (darks 0)
        (idx 0)
        (idx2 0))
    (setf darks (count-if #'(lambda (elem) (= elem 1)) fib))
    ;; horizontal 14 ~ 8
    (loop for j from 0 to 7 do
         (when (eq (aref matrix 8 j) :reserve)
           (paint-fcolor-bit matrix 8 j (nth idx fib))
           (incf idx)))
    ;; vertical 14 ~ 8
    (loop for i from (- modules 1) downto (- modules 7) do
         (paint-fcolor-bit matrix i 8 (nth idx2 fib))
         (incf idx2))
    ;; horizontal 7 - 0
    (loop for j from (- modules 8) to (- modules 1) do
         (paint-fcolor-bit matrix 8 j (nth idx fib))
         (incf idx))
    ;; vertical 7 - 0
    (loop for i from 8 downto 0 do
         (when (eq (aref matrix i 8) :reserve)
           (paint-fcolor-bit matrix i 8 (nth idx2 fib))
           (incf idx2)))
    (values matrix darks)))

;;; only encoding region modules (excluding format information) are masked
(defun encoding-module-p (matrix i j)
  "modules belong to encoding region, excluding format & version information"
  (or (eq (aref matrix i j) :light)
      (eq (aref matrix i j) :dark)))
(defun non-mask-module-p (matrix i j)
  (not (encoding-module-p matrix i j)))
(defun reverse-module-color (matrix i j)
  (case (aref matrix i j)
    (:dark :light) (:light :dark)))

;;; all modules are evaluated:
;;;  there should be only :dark :light :fdark :flight modules left by now
(defun dark-module-p (matrix i j)
  (or (eq (aref matrix i j) :fdark)
      (eq (aref matrix i j) :dark)))

(defun copy-and-mask (matrix modules level mask-ind)
  "make a new matrix and mask using MASK-IND for later evaluation"
  (let ((ret (make-modules-matrix modules))
        (mask-p (mask-condition mask-ind))
        (darks 0))
    (dotimes (i modules)
      (dotimes (j modules)
        (cond
          ((non-mask-module-p matrix i j)
           (setf (aref ret i j) (aref matrix i j)))
          ((funcall mask-p i j) ; need mask
           (setf (aref ret i j) (reverse-module-color matrix i j)))
          (t
           (setf (aref ret i j) (aref matrix i j))))
        (when (dark-module-p ret i j)
          (incf darks))))
    (multiple-value-bind (dummy fi-darks)
        (format-information ret modules level mask-ind)
      (declare (ignore dummy))
      ;; add format information dark modules
      (values ret (+ darks fi-darks)))))

(defun mask-matrix (matrix modules level mask-ind)
  "do not evaluate, just go ahead and mask MATRIX using MASK-IND mask pattern"
  (let ((mask-p (mask-condition mask-ind)))
    (dotimes (i modules)
      (dotimes (j modules)
        (and (encoding-module-p matrix i j)
             (funcall mask-p i j)
             (setf (aref matrix i j) (reverse-module-color matrix i j)))))
    ;; paint format information
    (format-information matrix modules level mask-ind)
    matrix))

(defvar *mask-pattern-num* 8)

(defun choose-masking (matrix modules level)
  "mask and evaluate using each mask pattern, choose the best mask result"
  (let ((n4 10)
        (best-matrix nil)
        (mask-indicator nil)
        (min-penalty nil)
        (square (* modules modules))
        (cur-penalty 0))
    (dotimes (i *mask-pattern-num*)
      (multiple-value-bind (cur-matrix darks)
          (copy-and-mask matrix modules level i)
        ;; feature 4: proportion of dark modules in entire symbol
        (let ((bratio (/ (+ (* darks 200) square) square 2)))
          (setf cur-penalty (* (/ (abs (- bratio 50)) 5) n4)))
        (incf cur-penalty (evaluate-feature-123 cur-matrix modules))
        (when (or (null min-penalty)
                  (< cur-penalty min-penalty))
          (setf min-penalty cur-penalty
                mask-indicator i
                best-matrix cur-matrix))))
    (values best-matrix mask-indicator)))

;;; feature 1 & 2 & 3
(defun evaluate-feature-123 (matrix modules)
  (let ((penalty 0))
    (incf penalty (evaluate-feature-2 matrix modules))
    (dotimes (col modules)
      (let ((rlength (calc-run-length matrix modules col)))
        (incf penalty (evaluate-feature-1 rlength))
        (incf penalty (evaluate-feature-3 rlength))))
    (dotimes (row modules)
      (let ((rlength (calc-run-length matrix modules row :col)))
        (incf penalty (evaluate-feature-1 rlength))
        (incf penalty (evaluate-feature-3 rlength))))
    penalty))

(defun calc-run-length (matrix modules num &optional (direction :row))
  "list of number of adjacent modules in same color"
  (let ((rlength nil)
        (ridx 0))
    (labels ((get-elem (idx)
               (case direction
                 (:row (aref matrix num idx))
                 (:col (aref matrix idx num))))
             (add-to-list (list elem)
               (append list (list elem))))
      ;; we make sure (NTH 1 rlength) is for dark module
      (when (same-color-p (get-elem 0) :dark)
        (setf rlength (add-to-list rlength -1)
              ridx 1))
      (setf rlength (add-to-list rlength 1))

      (loop for i from 1 to (- modules 1) do
           (if (same-color-p (get-elem i) (get-elem (- i 1)))
               (incf (nth ridx rlength))
               (progn
                 (incf ridx)
                 (setf rlength (add-to-list rlength 1)))))
      rlength)))

(defun evaluate-feature-1 (rlength)
  "(5 + i) adjacent modules in row/column in same color. (N1 + i) points, N1 = 3"
  (let ((n1 3)
        (penalty 0))
    (dolist (sz rlength penalty)
      (when (> sz 5)
        (incf penalty (+ n1 sz -5))))))

(defun evaluate-feature-3 (rlength)
  "1:1:3:1:1 ration (dark:light:dark:light:dark) pattern in row/column,
preceded or followed by light area 4 modules wide. N3 points, N3 = 40"
  (let ((n3 40)
        (len (length rlength))
        (penalty 0))
    (do ((i 3 (+ i 2)))
        ((>= i (- len 2)) penalty)
      (when (and (= (mod i 2) 1) ; for dark module
                 (= (mod (nth i rlength) 3) 0)
        (let ((fact (floor (nth i rlength) 3)))
          ;; 1:1:3:1:1
          (when (= fact
                   (nth (- i 2) rlength)
                   (nth (- i 1) rlength)
                   (nth (+ i 1) rlength)
                   (nth (+ i 2) rlength))
            (cond
              ((<= (- i 3) 0) (incf penalty n3))
              ((>= (+ i 4) len) (incf penalty n3))
              ((>= (nth (- i 3) rlength) (* 4 fact)) (incf penalty n3))
              ((>= (nth (+ i 3) rlength) (* 4 fact)) (incf penalty n3))))))))))

(defun evaluate-feature-2 (matrix modules)
  "block m * n of modules in same color. N2 * (m-1) * (n-1) points, N2=3"
  (let ((n2 3)
        (penalty 0)
        (bcount 0))
    (dotimes (i (- modules 1) penalty)
      (dotimes (j (- modules 1))
        (when (dark-module-p matrix i j)
          (incf bcount))
        (when (dark-module-p matrix (+ i 1) j)
          (incf bcount))
        (when (dark-module-p matrix i (+ j 1))
          (incf bcount))
        (when (dark-module-p matrix (+ i 1) (+ j 1))
          (incf bcount))
        (when (or (= bcount 0) (= bcount 4))
          (incf penalty n2))))))

(defclass qr-symbol ()
  ((matrix :initform nil :initarg :matrix :reader matrix
           :documentation "qr code symbol as matrix")
   (modules :initform nil :initarg :modules :reader modules
            :documentation "qr code symbol modules")))

(defmethod print-object ((symbol qr-symbol) stream)
  (fresh-line stream)
  (with-slots (matrix modules) symbol
    (format stream "qr symbol ~A x ~A:~%" modules modules)
    (dotimes (i modules)
      (dotimes (j modules)
        (if (dark-module-p matrix i j)
            (format stream "1 ")
            (format stream "0 ")))
      (format stream "~%"))))

;;; FIX: other encodings???
(defun ascii->bytes (text)
  (map 'list #'char-code text))

(defun bytes->input (bytes version level mode)
  (setf version (min (max version 1) 40))
  (let ((input (make-instance 'qr-input :bytes bytes :version version
                              :ec-level level :mode mode)))
    (data-encoding input)
    (ec-coding input)
    (structure-message input)
    (module-placement input)
    input))

(defun input->symbol (input)
  "encode qr symbol from a qr-input"
  (multiple-value-bind (matrix mask-ref)
      (data-masking input)
    (declare (ignore mask-ref))
    (let ((modules (matrix-modules (version input))))
      (make-instance 'qr-symbol :matrix matrix :modules modules))))

(defun encode-symbol-bytes (bytes &key (version 1) (level :level-m) (mode nil))
  "encode final qr symbol from BYTES list"
  (let ((input (bytes->input bytes version level mode)))
    (log:debug! (format nil "version: ~A; segments: ~A~%" (version input)
                        (segments input)))
    (input->symbol input)))

;;;-----------------------------------------------------------------------------
;;; One Ring to Rule Them All, One Ring to Find Them,
;;; One Ring to Bring Them All and In the Darkness Blind Them:
;;;   This function wraps all we need.
;;;-----------------------------------------------------------------------------
;; (sdebug :dbg-input)
(defun encode-symbol (text &key (version 1) (level :level-m) (mode nil))
  "encode final qr symbol, unless you know what you are doing, leave MODE NIL"
  (let ((bytes (ascii->bytes text)))
    (encode-symbol-bytes bytes :version version :level level :mode mode)))

;;; Table 1 - Codeword capacity of all versions of QR Code 2005
;;; excluding Micro QR Code, varies between version
(defvar *codeword-capacity-table*
  #2A((-1  -1   -1 -1    -1   -1) ; 0, no such version
      (21  202  31 208   26   0) (25  235  31 359   44   7)
      (29  243  31 567   70   7) (33  251  31 807   100  7)
      (37  259  31 1079  134  7) (41  267  31 1383  172  7)
      (45  390  67 1568  196  0) (49  398  67 1936  242  0)
      (53  406  67 2336  292  0) (57  414  67 2768  346  0) ; Version 10
      (61  422  67 3232  404  0) (65  430  67 3728  466  0)
      (69  438  67 4256  532  0) (73  611  67 4651  581  3)
      (77  619  67 5243  655  3) (81  627  67 5867  733  3)
      (85  635  67 6523  815  3) (89  643  67 7211  901  3)
      (93  651  67 7931  991  3) (97  659  67 8683  1085 3) ; Version 20
      (101 882  67 9252  1156 4) (105 890  67 10068 1258 4)
      (109 898  67 10916 1364 4) (113 906  67 11796 1474 4)
      (117 914  67 12708 1588 4) (121 922  67 13652 1706 4)
      (125 930  67 14628 1828 4) (129 1203 67 15371 1921 3)
      (133 1211 67 16411 2051 3) (137 1219 67 17483 2185 3) ; Version 30
      (141 1227 67 18587 2323 3) (145 1235 67 19723 2465 3)
      (149 1243 67 20891 2611 3) (153 1251 67 22091 2761 3)
      (157 1574 67 23008 2876 0) (161 1582 67 24272 3034 0)
      (165 1590 67 25568 3196 0) (169 1598 67 26896 3362 0)
      (173 1606 67 28256 3532 0) (177 1614 67 29648 3706 0)) ; Version 40
  "Number of modules (as version increases, 4 modules added) A | Function pattern
modules B | Format and Version information modules C | Data modules (A^2-B-C) |
Data capacity codewords (bytes, including ecc codewords) | Remainder bits.")
(defun codeword-capacity (version)
  "codeword: data word + ecc word"
  (aref *codeword-capacity-table* version 4))
(defun matrix-modules (version)
  (aref *codeword-capacity-table* version 0))
(defun remainder-bits (version)
  (aref *codeword-capacity-table* version 5))

(defun mode->index (mode)
  (case mode
    (:numeric 0)
    (:alnum 1)
    (:byte 2)
    (:kanji 3)))

(deftype ecc-level ()
  '(member :level-l :level-m :level-q :level-h))
(defun level->index (level)
  (case level
    (:level-l 0)
    (:level-m 1)
    (:level-q 2)
    (:level-h 3)))

;;; (Part I of) Table 9 - Number of Error Correction Codewords (bytes)
;;; varies between version and level
(defvar *ecc-codewords-table*
  ;; (:level-l :level-m :level-q :level-h)
  #2A((-1  -1   -1   -1) ;; 0, no such version
      (7   10   13   17)   (10  16   22   28)   (15  26   36   44)
      (20  36   52   64)   (26  48   72   88)   (36  64   96   112)
      (40  72   108  130)  (48  88   132  156)  (60  110  160  192)
      (72  130  192  224)  (80  150  224  264)  (96  176  260  308)
      (104 198  288  352)  (120 216  320  384)  (132 240  360  432)
      (144 280  408  480)  (168 308  448  532)  (180 338  504  588)
      (196 364  546  650)  (224 416  600  700)  (224 442  644  750)
      (252 476  690  816)  (270 504  750  900)  (300 560  810  960)
      (312 588  870  1050) (336 644  952  1110) (360 700  1020 1200)
      (390 728  1050 1260) (420 784  1140 1350) (450 812  1200 1440)
      (480 868  1290 1530) (510 924  1350 1620) (540 980  1440 1710)
      (570 1036 1530 1800) (570 1064 1590 1890) (600 1120 1680 1980)
      (630 1204 1770 2100) (660 1260 1860 2220) (720 1316 1950 2310)
      (750 1372 2040 2430))) ;; version 1 ~ 40
(defun ecc-words-capacity (version level)
  (aref *ecc-codewords-table* version (level->index level)))
(defun data-words-capacity (version level)
  (- (codeword-capacity version) (ecc-words-capacity version level)))

;;; (Part II of) Table 9 - Error Correction blocks
;;; varies between version and level
(defvar *ecc-blocks*
  ;; (version, level) =>
  ;;   (# of ec codewords for each blk, # of blk 1, # of data words for blk 1,
  ;;                                    # of blk 2, # of data words for blk 2)
  ;; :level-l :level-m :level-q :level-h
  #3A(((0  0 0  0 0)     (0  0 0  0 0)    (0  0 0  0 0)    (0  0 0 0 0))     ; no such version
      ((7  1 19 0 0)     (10 1 16 0 0)    (13 1 13 0 0)    (17 1 9 0 0))     ; Version 1
      ((10 1 34 0 0)     (16 1 28 0 0)    (22 1 22 0 0)    (28 1 16 0 0))
      ((15 1 55 0 0)     (26 1 44 0 0)    (18 2 17 0 0)    (22 2 13 0 0))
      ((20 1 80 0 0)     (18 2 32 0 0)    (26 2 24 0 0)    (16 4 9 0 0))
      ((26 1 108 0 0)    (24 2 43 0 0)    (18 2 15 2 16)   (22 2 11 2 12))   ; Version 5
      ((18 2 68 0 0)     (16 4 27 0 0)    (24 4 19 0 0)    (28 4 15 0 0))
      ((20 2 78 0 0)     (18 4 31 0 0)    (18 2 14 4 15)   (26 4 13 1 14))
      ((24 2 97 0 0)     (22 2 38 2 39)   (22 4 18 2 19)   (26 4 14 2 15))
      ((30 2 116 0 0)    (22 3 36 2 37)   (20 4 16 4 17)   (24 4 12 4 13))
      ((18 2 68 2 69)    (26 4 43 1 44)   (24 6 19 2 20)   (28 6 15 2 16))   ; Version 10
      ((20 4 81 0 0)     (30 1 50 4 51)   (28 4 22 4 23)   (24 3 12 8 13))
      ((24 2 92 2 93)    (22 6 36 2 37)   (26 4 20 6 21)   (28 7 14 4 15))
      ((26 4 107 0 0)    (22 8 37 1 38)   (24 8 20 4 21)   (22 12 11 4 12))
      ((30 3 115 1 116)  (24 4 40 5 41)   (20 11 16 5 17)  (24 11 12 5 13))
      ((22 5 87 1 88)    (24 5 41 5 42)   (30 5 24 7 25)   (24 11 12 7 13))  ; Version 15
      ((24 5 98 1 99)    (28 7 45 3 46)   (24 15 19 2 20)  (30 3 15 13 16))
      ((28 1 107 5 108)  (28 10 46 1 47)  (28 1 22 15 23)  (28 2 14 17 15))
      ((30 5 120 1 121)  (26 9 43 4 44)   (28 17 22 1 23)  (28 2 14 19 15))
      ((28 3 113 4 114)  (26 3 44 11 45)  (26 17 21 4 22)  (26 9 13 16 14))
      ((28 3 107 5 108)  (26 3 41 13 42)  (30 15 24 5 25)  (28 15 15 10 16)) ; Version 20
      ((28 4 116 4 117)  (26 17 42 0 0)   (28 17 22 6 23)  (30 19 16 6 17))
      ((28 2 111 7 112)  (28 17 46 0 0)   (30 7 24 16 25)  (24 34 13 0 0))
      ((30 4 121 5 122)  (28 4 47 14 48)  (30 11 24 14 25) (30 16 15 14 16))
      ((30 6 117 4 118)  (28 6 45 14 46)  (30 11 24 16 25) (30 30 16 2 17))
      ((26 8 106 4 107)  (28 8 47 13 48)  (30 7 24 22 25)  (30 22 15 13 16)) ; Version 25
      ((28 10 114 2 115) (28 19 46 4 47)  (28 28 22 6 23)  (30 33 16 4 17))
      ((30 8 122 4 123)  (28 22 45 3 46)  (30 8 23 26 24)  (30 12 15 28 16))
      ((30 3 117 10 118) (28 3 45 23 46)  (30 4 24 31 25)  (30 11 15 31 16))
      ((30 7 116 7 117)  (28 21 45 7 46)  (30 1 23 37 24)  (30 19 15 26 16))
      ((30 5 115 10 116) (28 19 47 10 48) (30 15 24 25 25) (30 23 15 25 16)) ; Version 30
      ((30 13 115 3 116) (28 2 46 29 47)  (30 42 24 1 25)  (30 23 15 28 16))
      ((30 17 115 0 0)   (28 10 46 23 47) (30 10 24 35 25) (30 19 15 35 16))
      ((30 17 115 1 116) (28 14 46 21 47) (30 29 24 19 25) (30 11 15 46 16))
      ((30 13 115 6 116) (28 14 46 23 47) (30 44 24 7 25)  (30 59 16 1 17))
      ((30 12 121 7 122) (28 12 47 26 48) (30 39 24 14 25) (30 22 15 41 16)) ; Version 35
      ((30 6 121 14 122) (28 6 47 34 48)  (30 46 24 10 25) (30 2 15 64 16))
      ((30 17 122 4 123) (28 29 46 14 47) (30 49 24 10 25) (30 24 15 46 16))
      ((30 4 122 18 123) (28 13 46 32 47) (30 48 24 14 25) (30 42 15 32 16))
      ((30 20 117 4 118) (28 40 47 7 48)  (30 43 24 22 25) (30 10 15 67 16))
      ((30 19 118 6 119) (28 18 47 31 48) (30 34 24 34 25) (30 20 15 61 16)) ; Version 40
      ))
(defun ecc-block-nums (version level)
  "# of ec codewords for each blk, # of blk 1, # of data words for blk 1, ..."
  (let ((lidx (level->index level)))
    (values (aref *ecc-blocks* version lidx 0)
            (aref *ecc-blocks* version lidx 1)
            (aref *ecc-blocks* version lidx 2)
            (aref *ecc-blocks* version lidx 3)
            (aref *ecc-blocks* version lidx 4))))

(defun minimum-version (init-version nbytes level)
  "minimum version that can hold NBYTES data words, or INIT-VERSION if bigger"
  (do ((v init-version (1+ v)))
      ((> v 40) nil)
    (when (>= (data-words-capacity v level) nbytes)
      (return-from minimum-version v))))

(defun version-range (version)
  (cond
    ((<= 1 version 9) 0)
    ((<= 10 version 26) 1)
    ((<= 27 version 40) 2)))

;;; Table 3 - Number of bits in character count indicator for QR Code 2005
(defvar *char-count-indicator*
  ;; :numeric :alnum :byte :kanji
  #2A((10 9  8  8)    ; version-range 0
      (12 11 16 10)   ; version-range 1
      (14 13 16 12))) ; version-range 2
(defun char-count-bits (version mode)
  (let ((i (version-range version))
        (j (mode->index mode)))
    (aref *char-count-indicator* i j)))

;;; Table E.1 - Row/column coordinates of center modules of alignment patterns
;;; varies between versions
(defvar *align-coord-table*
  #2A((0  ()) ; 0, no such version
      (0  ())                       (1  (6 18))                   (1  (6 22))
      (1  (6 26))                   (1  (6 30))                   (1  (6 34))
      (6  (6 22 38))                (6  (6 24 42))                (6  (6 26 46))
      (6  (6 28 50))                (6  (6 30 54))                (6  (6 32 58))
      (6  (6 34 62))                (13 (6 26 46 66))             (13 (6 26 48 70))
      (13 (6 26 50 74))             (13 (6 30 54 78))             (13 (6 30 56 82))
      (13 (6 30 58 86))             (13 (6 34 62 90))             (22 (6 28 50 72 94))
      (22 (6 26 50 74 98))          (22 (6 30 54 78 102))         (22 (6 28 54 80 106))
      (22 (6 32 58 84 110))         (22 (6 30 58 86 114))         (22 (6 34 62 90 118))
      (33 (6 26 50 74 98 122))      (33 (6 30 54 78 102 126))     (33 (6 26 52 78 104 130))
      (33 (6 30 56 82 108 134))     (33 (6 34 60 86 112 138))     (33 (6 30 58 86 114 142))
      (33 (6 34 62 90 118 146))     (46 (6 30 54 78 102 126 150)) (46 (6 24 50 76 102 128 154))
      (46 (6 28 54 80 106 132 158)) (46 (6 32 58 84 110 136 162)) (46 (6 26 54 82 110 138 166))
      (46 (6 30 58 86 114 142 170)))
  "# of Alignment Patterns, row/column coordinates of center modules.")
(defun valid-center-p (x y modules)
  "The alignment center module is not in Finder Patterns."
  (not (or (and (<= 0 x 8) (<= 0 y 8)) ; upleft finder pattern
           (and (<= 0 x 8)
                (<= (- modules 8) y (- modules 1))) ; upright finder pattern
           (and (<= (- modules 8) x (- modules 1))
                (<= 0 y 8)))))
(defun align-centers (version)
  "list of all valid alignment pattern center modules under VERSION"
  (let* ((modules (matrix-modules version))
         (coords (aref *align-coord-table* version 1))
         (len (length coords))
         (centers nil))
    (dotimes (i len)
      (loop for j from i to (- len 1) do
           (let ((x (nth i coords))
                 (y (nth j coords)))
             (when (valid-center-p x y modules)
               (push (list x y) centers))
             (unless (= x y)
               (when (valid-center-p y x modules)
                 (push (list y x) centers))))))
    centers))

(defun mask-condition (indicator)
  (lambda (i j)
    (case indicator
      ;; (i + j) mod 2 == 0
      (0 (= (mod (+ i j) 2) 0))
      ;; i mod 2 == 0
      (1 (= (mod i 2) 0))
      ;; j mod 3 == 0
      (2 (= (mod j 3) 0))
      ;; (i + j) mod 3 == 0
      (3 (= (mod (+ i j) 3) 0))
      ;; ((i/2) + (j/3)) mod 2 == 0
      (4 (= (mod (+ (floor i 2) (floor j 3)) 2) 0))
      ;; (i*j) mod 2 + (i*j) mod 3 == 0
      (5 (= (+ (mod (* i j) 2) (mod (* i j) 3)) 0))
      ;; ((i*j) mod 2 + (i*j) mod 3)) mod 2 == 0
      (6 (= (mod (+ (mod (* i j) 2) (mod (* i j) 3)) 2) 0))
      ;; ((i+j) mod 2 + (i*j) mod 3)) mod 2 == 0
      (7 (= (mod (+ (mod (+ i j) 2) (mod (* i j) 3)) 2) 0)))))

(defvar *ecc-level-indicator* #((0 1) (0 0) (1 1) (1 0))
  ":level-l :level-m :level-q :level-h")
(defun level-indicator (level)
  (aref *ecc-level-indicator* (level->index level)))
(defvar *mask-pattern-reference*
  #((0 0 0) (0 0 1) (0 1 0) (0 1 1)
    (1 0 0) (1 0 1) (1 1 0) (1 1 1)))
(defun mask-pattern-ref (ind)
  (aref *mask-pattern-reference* ind))

;;; png backend for QR code symbol

(defun set-color (pngarray x y color)
  (setf (aref pngarray x y 0) color)
  (setf (aref pngarray x y 1) color)
  (setf (aref pngarray x y 2) color))

(defun qr-symbol-to-png (symbol pixsize margin)
  "return the qr symbol written into a PNG object with PIXSIZE
pixels for each module, and MARGIN pixels on all four sides"
  (with-slots (matrix modules) symbol
    (let* ((size (+ (* modules pixsize) (* margin 2)))
           (qrpng (make-instance 'png :width size :height size))
           (qrarray (dat/png::data-array qrpng)))
      (dotimes (x size)
        (dotimes (y size)
          (if (and (<= margin x (- size margin 1))
                   (<= margin y (- size margin 1)))
              (let ((i (floor (- x margin) pixsize))
                    (j (floor (- y margin) pixsize)))
                (if (dark-module-p matrix i j)
                    (set-color qrarray x y 0)
                    (set-color qrarray x y 255)))
              ;; quiet zone
              (set-color qrarray x y 255))))
      qrpng)))

(defun qr-encode-png (text &key (path "qrcode.png") (version 1) (level :level-m)
                   (mode nil) (pixsize 9) (margin 8))
  (let ((symbol (encode-symbol text :version version :level level :mode mode)))
    (write-png (qr-symbol-to-png symbol pixsize margin) path)))

(defmethod serialize ((self string) (format (eql :qrcode)) &key path (version 1) (level :level-m))
  (declare (ignore format))
  (qr-encode-png self :path path :version version :level level))

(defun qr-encode-png-stream (text stream &key (version 1) (level :level-m)
                          (mode nil) (pixsize 9) (margin 8))
  (let ((symbol (encode-symbol text :version version :level level :mode mode)))
    (write-png-stream (qr-symbol-to-png symbol pixsize margin) stream)))

(defun qr-encode-png-bytes (bytes &key (fpath "kanji.png") (version 1)
                         (level :level-m) (mode nil) (pixsize 9) (margin 8))
  (let ((symbol (encode-symbol-bytes bytes :version version :level level
                                     :mode mode)))
    (write-png (qr-symbol-to-png symbol pixsize margin) fpath)))

(defun qr-encode-png-bytes-stream (bytes stream &key (version 1) (level :level-m)
                                (mode nil) (pixsize 9) (margin 8))
  (let ((symbol (encode-symbol-bytes bytes :version version :level level
                                     :mode mode)))
    (write-png-stream (qr-symbol-to-png symbol pixsize margin) stream)))
