;;; ttf.lisp --- TrueType Fonts

;; Access TrueType font metrics and outlines from Common Lisp

;; Written by Zach Beane <xach@xach.com>

;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials
;;     provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:
(in-package :dat/ttf)
;;; Utils
(defun read-uint32 (stream)
  (loop repeat 4
        for value = (read-byte stream)
        then (logior (ash value 8) (read-byte stream))
        finally (return value)))

(defun read-uint16 (stream)
  (loop repeat 2
        for value = (read-byte stream)
          then (logior (ash value 8) (read-byte stream))
        finally (return value)))

(defun read-uint8 (stream)
  (read-byte stream))

(defun read-int8 (stream)
  (let ((result (read-byte stream)))
    (if (logbitp 7 result)
        (1- (- (logandc2 #xFF result)))
        result)))

(defun read-int16 (stream)
  (let ((result (read-uint16 stream)))
    (if (logbitp 15 result)
        (1- (- (logandc2 #xFFFF result)))
        result)))

(defun read-fixed (stream)
  (read-uint32 stream))

(defun read-fword (stream)
  (read-int16 stream))

(defun read-ufword (stream)
  (read-uint16 stream))

(defun read-fixed2.14 (stream)
  (let ((value (read-uint16 stream)))
    (let ((integer (ash value -14))
          (fraction (logand #x3FFF value)))
      (when (logbitp 1 integer)
        (setf integer (1- (- (logandc2 #b11 integer)))))
      (+ integer (float (/ fraction #x4000))))))

(defun read-pstring (stream)
  "Read a Pascal-style length-prefixed string."
  (let* ((length (read-uint8 stream))
         (buf (make-array length :element-type '(unsigned-byte 8)))
         (string (make-string length)))
    (read-sequence buf stream)
    ;; The following could be (map 'string #'code-char buf), but that
    ;; form benchmarked poorly
    (dotimes (i length string)
      (setf (schar string i) (code-char (aref buf i))))))

(defun advance-file-position (stream n)
  "Move the file position of STREAM ahead by N bytes."
  (let ((pos (file-position stream)))
    (file-position stream (+ pos n))))

(defun bounded-aref (vector index)
  "Some TrueType data vectors are truncated, and any references beyond
the end of the vector should be treated as a reference to the last
element in the vector."
  (aref vector (min (1- (length vector)) index)))

(defun (setf bounded-aref) (new-value vector index)
  (setf (aref vector (min (1- (length vector)) index)) new-value))

;;; Conditions
(define-condition regrettable-value (error)
  ((actual-value
    :initarg :actual-value
    :accessor actual-value)
   (expected-values
    :initarg :expected-values
    :accessor expected-values)
   (description
    :initarg :description
    :initform nil
    :accessor description)
   (location
    :initarg :location
    :initform nil
    :accessor location))
  (:report
   (lambda (c s)
     (format s "~:[Regrettable~;~:*~A~] value~:[~;~:* in ~A~]: ~
                ~A (expected ~{~A~^ or ~})"
             (description c)
             (location c)
             (actual-value c)
             (expected-values c)))))

(define-condition regrettable-hex-value (regrettable-value)
  ((size
    :initarg :size
    :initform 8
    :accessor size)
   (actual-value
    :reader %actual-value)
   (expected-values
    :reader %expected-values)))

(defmethod actual-value ((c regrettable-hex-value))
  (format nil "#x~v,'0X" (size c) (%actual-value c)))

(defmethod expected-values ((c regrettable-hex-value))
  (mapcar (lambda (v)
            (format nil "#x~v,'0X" (size c) v))
          (%expected-values c)))

(define-condition bad-magic (regrettable-hex-value)
  ((description :initform "Bad magic")))

(define-condition unsupported-version (regrettable-hex-value)
  ((description :initform "Unsupported version")))

(define-condition unsupported-format (regrettable-hex-value)
  ((description :initform "Unsupported format")))

(define-condition unsupported-value (regrettable-value)
  ((description :initform "Unsupported")))

(defun check-version (location actual &rest expected)
  (or (member actual expected :test #'=)
      (error 'unsupported-version
             :location location
             :actual-value actual
             :expected-values expected)))

;;; Bounding Box
(defgeneric bounding-box (object))

(macrolet ((bbox-accessor (name index)
             `(progn
                (defgeneric ,name (object)
                  (:method (object)
                    (aref (bounding-box object) ,index)))
                (defgeneric (setf ,name) (new-value object)
                  (:method (new-value object)
                    (setf (aref (bounding-box object) ,index) new-value))))))
  (bbox-accessor bbox-xmin 0)
  (bbox-accessor bbox-ymin 1)
  (bbox-accessor bbox-xmax 2)
  (bbox-accessor bbox-ymax 3))

(defmethod bounding-box ((object array))
  object)

;;; Font Loader
(defclass font-loader ()
  ((tables :initform (make-hash-table) :reader tables)
   (input-stream :initarg :input-stream :accessor input-stream
          :documentation "The stream from which things are loaded.")
   (table-count :initarg :table-count :reader table-count)
   ;; from the 'head' table
   (units/em :accessor units/em)
   (bounding-box :accessor bounding-box)
   (loca-offset-format :accessor loca-offset-format)
   ;; from the 'loca' table
   (glyph-locations :accessor glyph-locations)
   ;; from the 'cmap' table
   (character-map :accessor character-map)
   (inverse-character-map :accessor inverse-character-map)
   ;; from the 'maxp' table
   (glyph-count :accessor glyph-count)
   ;; from the 'hhea' table
   (ascender :accessor ascender)
   (descender :accessor descender)
   (line-gap :accessor line-gap)
   (max-width :accessor max-width)
   ;; from the 'hmtx' table
   (advance-widths :accessor advance-widths)
   (left-side-bearings :accessor left-side-bearings)
   ;; from the 'vhea' table
   (vhea-missing-p :initform nil :accessor vhea-missing-p)
   (vascender :accessor vascender)
   (vdescender :accessor vdescender)
   ;; from 'vhea' and 'vmtx' tables
   (vmtx-missing-p :initform nil :accessor vmtx-missing-p)
   (advance-heights :accessor advance-heights)
   (top-side-bearings :accessor top-side-bearings)
   ;; from the 'kern' table
   (kerning-table :initform (make-hash-table) :accessor kerning-table)
   ;; from the 'name' table
   (name-entries :initform nil :accessor name-entries)
   ;; from the 'post' table
   (italic-angle :accessor italic-angle :initform 0)
   (fixed-pitch-p :accessor fixed-pitch-p :initform nil)
   (underline-position :accessor underline-position :initform 0)
   (underline-thickness :accessor underline-thickness :initform 0)
   (postscript-glyph-names :accessor postscript-glyph-names)
   ;; misc
   (glyph-cache :accessor glyph-cache)
   ;; # of fonts in collection, if loaded from a ttc file
   (collection-font-count :reader collection-font-count :initform nil
                          :initarg :collection-font-cont)
   ;; index of font in collection, if loaded from a ttc file
   (collection-font-index :reader collection-font-index :initform nil
                          :initarg :collection-font-index)))

(defclass table-info ()
  ((name :initarg :name :reader name)
   (offset :initarg :offset :reader io::offset)
   (size :initarg :size :reader io::size)))

(defmethod print-object ((object table-info) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "\"~A\"" (name object))))

;;; tag integers to strings and back
(defun number->tag (number)
  "Convert the 32-bit NUMBER to a string of four characters based on
the CODE-CHAR of each octet in the number."
  (let ((tag (make-string 4)))
    (loop for i below 4
          for offset from 24 downto 0 by 8
          do (setf (schar tag i)
                   (code-char (ldb (byte 8 offset) number))))
    tag))

(defun tag->number (tag)
  "Convert the four-character string TAG to a 32-bit number based on
the CHAR-CODE of each character."
  (declare (simple-string tag))
  (loop for char across tag
        for offset from 24 downto 0 by 8
        summing (ash (char-code char) offset)))

;;; Getting table info out of the loader
(defmethod table-info ((tag string) (font-loader font-loader))
  (gethash (tag->number tag) (tables font-loader)))

(defmethod table-exists-p (tag font-loader)
  (nth-value 1 (table-info tag font-loader)))

(defmethod table-position ((tag string) (font-loader font-loader))
  "Return the byte position in the font-loader's stream for the table
named by TAG."
  (let ((table-info (table-info tag font-loader)))
    (if table-info
        (offset table-info)
        (error "No such table -- ~A" tag))))

(defmethod table-size ((tag string) (font-loader font-loader))
  (let ((table-info (table-info tag font-loader)))
    (if table-info
        (size table-info)
        (error "No such table -- ~A" tag))))

(defmethod seek-to-table ((tag string) (font-loader font-loader))
  "Move FONT-LOADER's input stream to the start of the table named by TAG."
  (let ((table-info (table-info tag font-loader)))
    (if table-info
        (seek-to-table table-info font-loader)
        (error "No such table -- ~A" tag))))

(defmethod seek-to-table ((table table-info) (font-loader font-loader))
  "Move FONT-LOADER's input stream to the start of TABLE."
  (file-position (input-stream font-loader) (offset table)))

;;; maxp
;; Loading data from the "maxp" table.

;; ref: https://docs.microsoft.com/en-us/typography/opentype/spec/maxp
;; ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6maxp.html

(defmethod load-maxp-info ((font-loader font-loader))
  (seek-to-table "maxp" font-loader)
  (with-slots (input-stream glyph-count) font-loader
    (let ((version (read-uint32 input-stream)))
      (check-version "\"maxp\" table" version #x00010000)
      (setf glyph-count (read-uint16 input-stream)))))

;;; head
;; Loading data from the "head" table.

;; ref: https://docs.microsoft.com/en-us/typography/opentype/spec/head
;; ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6head.html

(defmethod load-head-info ((font-loader font-loader))
  (seek-to-table "head" font-loader)
  (with-slots (input-stream units/em bounding-box loca-offset-format)
      font-loader
    (flet ((skip-bytes (count)
             (file-position input-stream (+ count
                                            (file-position input-stream)))))
      (let ((version (read-uint32 input-stream)))
        (check-version "\"head\" table" version #x00010000))
      ;; skip fontRevsion and checkSumAdjustment (both uint32)
      (skip-bytes 8)
      ;; check the magicNumber
      (let ((magic-number (read-uint32 input-stream)))
        (when (/= magic-number #x5F0F3CF5)
          (error 'bad-magic
                 :location "\"head\" table"
                 :expected-values (list #x5F0F3CF5)
                 :actual-value magic-number)))
      ;; skip flags
      (skip-bytes 2)
      (setf units/em (read-uint16 input-stream))
      ;; skip created and modified dates
      (skip-bytes 16)
      (setf bounding-box (vector (read-int16 input-stream)
                                 (read-int16 input-stream)
                                 (read-int16 input-stream)
                                 (read-int16 input-stream)))
      ;; skip macStyle, lowestRecPPEM, fontDirectionHint
      (skip-bytes 6)
      ;; set the loca-offset-format
      (if (zerop (read-int16 input-stream))
          (setf loca-offset-format :short)
          (setf loca-offset-format :long)))))

;;; kern
;; "kern" table functions

;; ref: https://docs.microsoft.com/en-us/typography/opentype/spec/kern
;; ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6kern.html
(defun load-kerning-format-0 (table stream)
  "Return a hash table keyed on a UINT32 key that represents the glyph
index in the left and right halves with a value of the kerning
distance between the pair."
  (let ((pair-count (read-uint16 stream))
        (search-range (read-uint16 stream))
        (entry-selector (read-uint16 stream))
        (range-shift (read-uint16 stream))
        (bytes-read 8))
    (declare (ignore search-range entry-selector range-shift))
    (dotimes (i pair-count)
      (let ((key (read-uint32 stream))
            (value (read-int16 stream)))
        ;; apple specifies a terminating entry, ignore it
        (unless (and (= key #xffffffff) (= value 0))
          (setf (gethash key table) value))
        (incf bytes-read 6)))
    bytes-read))

(defun parse-offset-table (buffer start)
  (let ((first-glyph (aref buffer start))
        (glyph-count (aref buffer (1+ start)))
        (offsets (make-hash-table)))
    (loop for i from (+ start 2)
          for g from first-glyph
          repeat glyph-count
          collect (setf (gethash g offsets) (aref buffer i)))
    offsets))

(defun load-kerning-format-2 (table stream size)
  "Return a hash table keyed on a UINT32 key that represents the glyph
index in the left and right halves with a value of the kerning
distance between the pair."
  (let* ((buffer (coerce (loop repeat (/ size 2)
                               collect (read-uint16 stream))
                         '(simple-array (unsigned-byte) 1)))
         (row-width (aref buffer 0))
         (left-offset-table (aref buffer 1))
         (right-offset-table (aref buffer 2))
         (array-offset (aref buffer 3))
         (left (parse-offset-table buffer (- (/ left-offset-table 2) 4)))
         (right (parse-offset-table buffer (- (/ right-offset-table 2) 4))))
    (declare (ignorable row-width array-offset))
    (flet ((s16 (x)
             (if (logbitp 15 x)
                 (1- (- (logandc2 #xFFFF x)))
                 x)))
      (maphash (lambda (lk lv)
                 (maphash (lambda (rk rv)
                            (let ((key (logior (ash lk 16) rk))
                                  (value (s16 (aref buffer
                                                    (- (/ (+ lv rv) 2) 4)))))
                              (unless (zerop value)
                                (setf (gethash key table) value))))
                          right))
               left))
    size))

(defmethod load-kerning-subtable ((font-loader font-loader) format size)
  (when (/= format 0 1 2)
    (error 'unsupported-format
           :description "kerning subtable"
           :size 1
           :expected-values (list 0 1 2)
           :actual-value format))
  (case format
    (0
     (load-kerning-format-0 (kerning-table font-loader)
                            (input-stream font-loader)))
    (1
     ;; state table for contextual kerning, ignored for now
     (advance-file-position (input-stream font-loader) (- size 8))
     (- size 8))
    (2
     (load-kerning-format-2 (kerning-table font-loader)
                            (input-stream font-loader)
                            size))))

(defmethod load-kern-info ((font-loader font-loader))
  (when (table-exists-p "kern" font-loader)
    (seek-to-table "kern" font-loader)
    (let* ((stream (input-stream font-loader))
           (maybe-version (read-uint16 stream))
           (maybe-table-count (read-uint16 stream))
           (version 0)
           (table-count 0)
           (apple-p nil))
      ;; These shenanegins are because Apple documents one style of
      ;; kern table and Microsoft documents another. This code
      ;; tries to support both.
      ;; See:
      ;;  https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html
      ;;  https://learn.microsoft.com/en-us/typography/opentype/spec/kern
      (if (zerop maybe-version)
          (setf version maybe-version
                table-count maybe-table-count)
          (setf version (logand (ash maybe-version 16) maybe-table-count)
                table-count (read-uint32 stream)
                apple-p t))
      (check-version "\"kern\" table" version 0)
      (dotimes (i table-count)
        (let ((version (read-uint16 stream))
              (length (read-uint16 stream))
              (coverage-flags (read-uint8 stream))
              (format (read-uint8 stream)))
          (declare (ignorable version))
          (case coverage-flags
            ;; only read horizontal kerning, since storing others in
            ;; same array would be confusing and vertical layouts
            ;; don't seem to be supported currently
            (0
             (when apple-p
               (read-uint16 stream))    ; read and discard tuple-index

             (let ((bytes-read (+ (load-kerning-subtable font-loader format
                                                         length)
                                  (if apple-p 8 6))))
               (advance-file-position stream (- length bytes-read))))
            ;; ignore other known types of kerning
            ((#x8000  ;; vertical
              #x4000  ;; cross stream
              #x2000) ;; variation
             (advance-file-position stream (- length 6)))
            ;; otherwise error
            (otherwise
             (error 'unsupported-format
                    :description "kerning subtable coverage"
                    :size 2
                    :expected-values (list 0 #x2000 #x4000 #x8000)
                    :actual-value coverage-flags))))))))

(defmethod all-kerning-pairs ((font-loader font-loader))
  (let ((pairs nil))
    (maphash (lambda (k v)
               (let* ((left-index (ldb (byte 16 16) k))
                      (right-index (ldb (byte 16 0) k))
                      (left (index-glyph left-index font-loader))
                      (right (index-glyph right-index font-loader)))
                 (push (list left right v) pairs)))
             (kerning-table font-loader))
    pairs))

;;; loca
;; Loading data from the "loca" table.

;; ref: https://docs.microsoft.com/en-us/typography/opentype/spec/loca
;; ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6loca.html
(defmethod load-loca-info ((font-loader font-loader))
  (seek-to-table "loca" font-loader)
  (with-slots (input-stream glyph-locations glyph-count loca-offset-format)
      font-loader
    (setf glyph-locations (make-array (1+ glyph-count)))
    (dotimes (i (1+ glyph-count))
      (setf (svref glyph-locations i)
            (if (eql loca-offset-format :short)
                (* (read-uint16 input-stream) 2)
                (read-uint32 input-stream))))))

(defmethod glyph-location (index (font-loader font-loader))
  (aref (glyph-locations font-loader) index))

(defmethod glyph-length (index (font-loader font-loader))
  (with-slots (glyph-locations)
      font-loader
    (- (aref glyph-locations (1+ index))
       (aref glyph-locations index))))

;;; name
;; Loading data from the TrueType "name" table.

;; ref: https://docs.microsoft.com/en-us/typography/opentype/spec/name
;; ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6name.html
(defvar *name-identifiers*
  #(:copyright-notice
    :font-family
    :font-subfamily
    :unique-subfamily
    :full-name
    :name-table-version
    :postscript-name
    :trademark-notice
    :manufacturer-name
    :designer
    :description
    :vendor-url
    :designer-url
    :license-description
    :licence-info-url
    :reserved
    :preferred-family
    :preferred-subfamily
    :compatible-full
    :sample-text))

(defvar *platform-identifiers*
  #(:unicode
    :macintosh
    :iso
    :microsoft
    :custom))

(defvar *unicode-encoding-ids*
  #(:unicode-1.0
    :unicode-1.1
    :iso-10646\:1993
    :unicode>=2.0-bmp-only
    :unicode>=2.0-full-repertoire))

(defvar *microsoft-encoding-ids*
  #(:symbol
    :unicode
    :shiftjis
    :prc
    :big5
    :wansung
    :johab
    :7-reserved
    :8-reserved
    :9-reserved
    :ucs-4))

(defvar *macintosh-encoding-ids*
  #(:roman
    :japanese
    :chinese-traditional
    :korean
    :arabic
    :hebrew
    :greek
    :russian
    :RSymbol
    :devanagari
    :gurmukhi
    :gujarati
    :oriya
    :bengali
    :tamil
    :telugu
    :kennada
    :malayam
    :sinhalese
    :burmese
    :khmer
    :thai
    :laotian
    :georgian
    :armenian
    :chinese-simplified
    :tibetan
    :mongolian
    :geez
    :slavic
    :vietnamese
    :sindhi
    :uninterpreted))

(defvar *iso-encoding-ids*
  #(:7-bit-ascii
    :iso-10646
    :iso-8859-1))

(defparameter *encoding-tables*
  (vector *unicode-encoding-ids*
          *macintosh-encoding-ids*
          *iso-encoding-ids*
          *microsoft-encoding-ids*
          nil))

(defun encoding-id-name (platform-id encoding-id)
  (if (and (array-in-bounds-p *encoding-tables* platform-id)
           (aref *encoding-tables* platform-id)
           (array-in-bounds-p (aref *encoding-tables* platform-id) encoding-id))
      (aref (aref *encoding-tables* platform-id) encoding-id)
      encoding-id))

(defun platform-id-name (platform-id)
  (if (array-in-bounds-p *platform-identifiers* platform-id)
      (aref *platform-identifiers* platform-id)
      platform-id))

(defparameter *macroman-translation-table*
  #(#x00 #x00
    #x01 #x01
    #x02 #x02
    #x03 #x03
    #x04 #x04
    #x05 #x05
    #x06 #x06
    #x07 #x07
    #x08 #x08
    #x09 #x09
    #x0A #x0A
    #x0B #x0B
    #x0C #x0C
    #x0D #x0D
    #x0E #x0E
    #x0F #x0F
    #x10 #x10
    #x11 #x11
    #x12 #x12
    #x13 #x13
    #x14 #x14
    #x15 #x15
    #x16 #x16
    #x17 #x17
    #x18 #x18
    #x19 #x19
    #x1A #x1A
    #x1B #x1B
    #x1C #x1C
    #x1D #x1D
    #x1E #x1E
    #x1F #x1F
    #x20 #x20
    #x21 #x21
    #x22 #x22
    #x23 #x23
    #x24 #x24
    #x25 #x25
    #x26 #x26
    #x27 #x27
    #x28 #x28
    #x29 #x29
    #x2A #x2A
    #x2B #x2B
    #x2C #x2C
    #x2D #x2D
    #x2E #x2E
    #x2F #x2F
    #x30 #x30
    #x31 #x31
    #x32 #x32
    #x33 #x33
    #x34 #x34
    #x35 #x35
    #x36 #x36
    #x37 #x37
    #x38 #x38
    #x39 #x39
    #x3A #x3A
    #x3B #x3B
    #x3C #x3C
    #x3D #x3D
    #x3E #x3E
    #x3F #x3F
    #x40 #x40
    #x41 #x41
    #x42 #x42
    #x43 #x43
    #x44 #x44
    #x45 #x45
    #x46 #x46
    #x47 #x47
    #x48 #x48
    #x49 #x49
    #x4A #x4A
    #x4B #x4B
    #x4C #x4C
    #x4D #x4D
    #x4E #x4E
    #x4F #x4F
    #x50 #x50
    #x51 #x51
    #x52 #x52
    #x53 #x53
    #x54 #x54
    #x55 #x55
    #x56 #x56
    #x57 #x57
    #x58 #x58
    #x59 #x59
    #x5A #x5A
    #x5B #x5B
    #x5C #x5C
    #x5D #x5D
    #x5E #x5E
    #x5F #x5F
    #x60 #x60
    #x61 #x61
    #x62 #x62
    #x63 #x63
    #x64 #x64
    #x65 #x65
    #x66 #x66
    #x67 #x67
    #x68 #x68
    #x69 #x69
    #x6A #x6A
    #x6B #x6B
    #x6C #x6C
    #x6D #x6D
    #x6E #x6E
    #x6F #x6F
    #x70 #x70
    #x71 #x71
    #x72 #x72
    #x73 #x73
    #x74 #x74
    #x75 #x75
    #x76 #x76
    #x77 #x77
    #x78 #x78
    #x79 #x79
    #x7A #x7A
    #x7B #x7B
    #x7C #x7C
    #x7D #x7D
    #x7E #x7E
    #x7F #x7F
    #x80 #x00C4
    #x81 #x00C5
    #x82 #x00C7
    #x83 #x00C9
    #x84 #x00D1
    #x85 #x00D6
    #x86 #x00DC
    #x87 #x00E1
    #x88 #x00E0
    #x89 #x00E2
    #x8A #x00E4
    #x8B #x00E3
    #x8C #x00E5
    #x8D #x00E7
    #x8E #x00E9
    #x8F #x00E8
    #x90 #x00EA
    #x91 #x00EB
    #x92 #x00ED
    #x93 #x00EC
    #x94 #x00EE
    #x95 #x00EF
    #x96 #x00F1
    #x97 #x00F3
    #x98 #x00F2
    #x99 #x00F4
    #x9A #x00F6
    #x9B #x00F5
    #x9C #x00FA
    #x9D #x00F9
    #x9E #x00FB
    #x9F #x00FC
    #xA0 #x2020
    #xA1 #x00B0
    #xA2 #x00A2
    #xA3 #x00A3
    #xA4 #x00A7
    #xA5 #x2022
    #xA6 #x00B6
    #xA7 #x00DF
    #xA8 #x00AE
    #xA9 #x00A9
    #xAA #x2122
    #xAB #x00B4
    #xAC #x00A8
    #xAD #x2260
    #xAE #x00C6
    #xAF #x00D8
    #xB0 #x221E
    #xB1 #x00B1
    #xB2 #x2264
    #xB3 #x2265
    #xB4 #x00A5
    #xB5 #x00B5
    #xB6 #x2202
    #xB7 #x2211
    #xB8 #x220F
    #xB9 #x03C0
    #xBA #x222B
    #xBB #x00AA
    #xBC #x00BA
    #xBD #x03A9
    #xBE #x00E6
    #xBF #x00F8
    #xC0 #x00BF
    #xC1 #x00A1
    #xC2 #x00AC
    #xC3 #x221A
    #xC4 #x0192
    #xC5 #x2248
    #xC6 #x2206
    #xC7 #x00AB
    #xC8 #x00BB
    #xC9 #x2026
    #xCA #x00A0
    #xCB #x00C0
    #xCC #x00C3
    #xCD #x00D5
    #xCE #x0152
    #xCF #x0153
    #xD0 #x2103
    #xD1 #x2014
    #xD2 #x201C
    #xD3 #x201D
    #xD4 #x2018
    #xD5 #x2019
    #xD6 #x00F7
    #xD7 #x25CA
    #xD8 #x00FF
    #xD9 #x0178
    #xDA #x2044
    #xDB #x20AC
    #xDC #x2039
    #xDD #x203A
    #xDE #xFB01
    #xDF #xFB02
    #xE0 #x2021
    #xE1 #x00B7
    #xE2 #x201A
    #xE3 #x201E
    #xE4 #x2030
    #xE5 #x00C2
    #xE6 #x00CA
    #xE7 #x00C1
    #xE8 #x00CB
    #xE9 #x00C8
    #xEA #x00CD
    #xEB #x00CE
    #xEC #x00CF
    #xED #x00CC
    #xEE #x00D3
    #xEF #x00D4
    #xF0 #xF8FF
    #xF1 #x00D2
    #xF2 #x00DA
    #xF3 #x00DB
    #xF4 #x00D9
    #xF5 #x0131
    #xF6 #x02C6
    #xF7 #x02DC
    #xF8 #x00AF
    #xF9 #x02D8
    #xFA #x02D9
    #xFB #x02DA
    #xFC #x00B8
    #xFD #x02DD
    #xFE #x02DB
    #xFF #x02C7))

(defconstant +unicode-platform-id+   0)
(defconstant +macintosh-platform-id+ 1)
(defconstant +iso-platform-id+       2)
(defconstant +microsoft-platform-id+ 3)
(defconstant +custom-platform-id+    4)

(defconstant +unicode-2.0-encoding-id+           3)
(defconstant +unicode-2.0-full-encoding-id+      4)
(defconstant +microsoft-unicode-bmp-encoding-id+ 1)
(defconstant +microsoft-unicode-ucs4-encoding-id+ 10)
(defconstant +microsoft-symbol-encoding-id+      0)
(defconstant +macintosh-roman-encoding-id+       1)

;; Full list of microsoft language IDs is here:
;;  http://www.microsoft.com/globaldev/reference/lcid-all.mspx

(defconstant +microsoft-us-english-language-id+ #x0409)
(defconstant +macintosh-english-language-id+    1)
(defconstant +unicode-language-id+              0)

(defclass name-entry ()
  ((font-loader
    :initarg :font-loader
    :accessor font-loader)
   (platform-id
    :initarg :platform-id
    :accessor platform-id)
   (encoding-id
    :initarg :encoding-id
    :accessor encoding-id)
   (language-id
    :initarg :language-id
    :accessor language-id)
   (name-id
    :initarg :name-id
    :accessor name-id)
   (offset
    :initarg :offset
    :accessor offset
    :documentation "The octet offset within the TrueType file stream
of the entry's data. *Not* the same as the offset in the NameRecord
structure, which is relative to the start of the string data for the
table.")
   (entry-length
    :initarg :entry-length
    :accessor entry-length)
   (value
    :reader %value
    :writer (setf value))
   (octets
    :reader %octets
    :writer (setf data))))

(defmethod print-object ((name-entry name-entry) stream)
  (print-unreadable-object (name-entry stream :type t)
    (format stream "~A (~A/~A/~D)"
            (aref *name-identifiers* (name-id name-entry))
            (platform-id-name (platform-id name-entry))
            (encoding-id-name (platform-id name-entry)
                              (encoding-id name-entry))
            (language-id name-entry))))

(defun unicode-octets-to-string (octets)
  (let ((string (make-string (/ (length octets) 2))))
    (flet ((ref16 (i)
             (+ (ash (aref octets i) 16)
                (aref octets (1+ i)))))
      (loop for i from 0 below (length octets) by 2
            for j from 0
            do (setf (char string j) (code-char (ref16 i))))
      string)))

(defun macintosh-octets-to-string (octets)
  (flet ((macroman->unicode (point)
           (code-char (aref *macroman-translation-table* (1+ (ash point 1))))))
    (let ((string (make-string (length octets))))
      (dotimes (i (length octets) string)
        (setf (schar string i) (macroman->unicode (aref octets i)))))))

(defmethod data ((self name-entry))
  (unless (slot-boundp self 'octets)
    (initialize-name-entry self))
  (%octets self))

(defgeneric initialize-name-entry (name-entry)
  (:method (name-entry)
    (let ((stream (input-stream (font-loader name-entry)))
          (octets (make-array (entry-length name-entry)
                              :element-type '(unsigned-byte 8)))
          (value nil)
          (platform-id (platform-id name-entry)))
      (file-position stream (offset name-entry))
      (read-sequence octets stream)
      (cond ((or (= platform-id +unicode-platform-id+)
                 (= platform-id +microsoft-platform-id+))
             (setf value (unicode-octets-to-string octets)))
            ((= platform-id +macintosh-platform-id+)
             (setf value (macintosh-octets-to-string octets)))
            (t
             (error 'unsupported-value
                    :location "\"name\" table platform ID"
                    :actual-value platform-id
                    :expected-values (list +unicode-platform-id+
                                           +microsoft-platform-id+
                                           +macintosh-platform-id+))))
      (setf (value name-entry) value
            (data name-entry) octets))))

(defmethod value ((name-entry name-entry))
  (unless (slot-boundp name-entry 'value)
    (initialize-name-entry name-entry))
  (%value name-entry))

(defun load-name-info (loader)
  (seek-to-table "name" loader)
  (let* ((stream (input-stream loader))
         (table-offset (file-position stream))
         (format (read-uint16 stream)))
    (unless (= format 0)
      (error 'unsupported-format
             :location "\"name\" table"
             :actual-value format
             :expected-values (list 0)))
    (let* ((count (read-uint16 stream))
           (values-offset (read-uint16 stream))
           (entries (make-array count)))
      (setf (name-entries loader) entries)
      (dotimes (i count)
        (let ((platform-id (read-uint16 stream))
              (encoding-id (read-uint16 stream))
              (language-id (read-uint16 stream))
              (name-id (read-uint16 stream))
              (length (read-uint16 stream))
              (offset (read-uint16 stream)))
          (setf (aref entries i)
                (make-instance 'name-entry
                               :font-loader loader
                               :platform-id platform-id
                               :encoding-id encoding-id
                               :language-id language-id
                               :name-id name-id
                               :entry-length length
                               :offset (+ table-offset values-offset offset))))))))

;;;
;;; Fetching info out of the name-entry vector
;;;

(defun name-identifier-id (symbol)
  (let ((id (position symbol *name-identifiers*)))
    (if id
        id
        (error "Unknown NAME identifier: ~S" symbol))))


(defmethod find-name-entry (platform-id encoding-id language-id name-id
                            (font-loader font-loader))
  ;; FIXME: this vector is sorted by platform ID, encoding ID,
  ;; language ID, and name ID, in that order. Could bisect if it
  ;; mattered.
  (loop for name-entry across (name-entries font-loader)
        when (and (or (null platform-id)
                      (= (platform-id name-entry) platform-id))
                  (or (null encoding-id)
                      (= (encoding-id name-entry) encoding-id))
                  (or (null language-id)
                      (= (language-id name-entry) language-id))
                  (or (null name-id)
                      (= (name-id name-entry) name-id)))
        return name-entry))

(defmethod name-entry-value (name-designator (font-loader font-loader))
  (let* ((name-id (etypecase name-designator
                    (keyword (name-identifier-id name-designator))
                    (integer name-designator)))
         (entry (or (find-name-entry +unicode-platform-id+
                                     +unicode-2.0-encoding-id+
                                     +unicode-language-id+
                                     name-id
                                     font-loader)
                    (find-name-entry +microsoft-platform-id+
                                     nil
                                     +microsoft-us-english-language-id+
                                     name-id
                                     font-loader)
                    (find-name-entry +macintosh-platform-id+
                                     +macintosh-roman-encoding-id+
                                     +macintosh-english-language-id+
                                     name-id
                                     font-loader))))
    (when entry
      (value entry))))


(defmethod postscript-name ((font-loader font-loader))
  (name-entry-value :postscript-name font-loader))

(defmethod family-name ((font-loader font-loader))
  (name-entry-value :font-family font-loader))

(defmethod subfamily-name ((font-loader font-loader))
  (name-entry-value :font-subfamily font-loader))

(defmethod full-name ((font-loader font-loader))
  (name-entry-value :full-name font-loader))

;;; cmap
;; Loading data from the "cmap" table.

;; ref: https://docs.microsoft.com/en-us/typography/opentype/spec/cmap
;; ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6cmap.html
(deftype cmap-value-table ()
  `(array (unsigned-byte 16) (*)))

;;; FIXME: "unicode-cmap" is actually a format 4 character map that
;;; happens to currently be loaded from a Unicode-compatible
;;; subtable. However, other character maps (like Microsoft's Symbol
;;; encoding) also use format 4 and could be loaded with these
;;; "unicode" objects and functions.

(defclass unicode-cmap ()
  ((segment-count :initarg :segment-count :reader segment-count)
   (end-codes :initarg :end-codes :reader end-codes)
   (start-codes :initarg :start-codes :reader start-codes)
   (id-deltas :initarg :id-deltas :reader id-deltas)
   (id-range-offsets :initarg :id-range-offsets :reader id-range-offsets)
   (glyph-indexes :initarg :glyph-indexes :accessor glyph-indexes)))

(defclass format-12-cmap ()
  ((group-count :initarg :group-count :reader group-count)
   (start-codes :initarg :start-codes :reader start-codes)
   (end-codes :initarg :end-codes :reader end-codes)
   (glyph-starts :initarg :glyph-starts :accessor glyph-starts)))

(defun load-unicode-cmap-format12 (stream)
  "Load a Unicode character map of type 12 from STREAM starting at the
current offset. Assumes format is already read and checked."
  (let* ((reserved (read-uint16 stream))
         (subtable-length (read-uint32 stream))
         (language-code (read-uint32 stream))
         (group-count (read-uint32 stream))
         (start-codes (make-array group-count
                                  :element-type '(unsigned-byte 32)
                                  :initial-element 0))
         (end-codes (make-array group-count
                                :element-type '(unsigned-byte 32)
                                :initial-element 0))
         (glyph-starts (make-array group-count
                                   :element-type '(unsigned-byte 32)
                                   :initial-element 0)))
    (declare (ignore reserved language-code subtable-length))
    (loop for i below group-count
          do (setf (aref start-codes i) (read-uint32 stream)
                   (aref end-codes i) (read-uint32 stream)
                   (aref glyph-starts i) (read-uint32 stream)))
    (make-instance 'format-12-cmap
                   :group-count group-count
                   :start-codes start-codes
                   :end-codes end-codes
                   :glyph-starts glyph-starts)))

(defun load-unicode-cmap (stream)
  "Load a Unicode character map of type 4 or 12 from STREAM starting at
the current offset."
  (let ((format (read-uint16 stream)))
    (when (= format 12)
      (return-from load-unicode-cmap (load-unicode-cmap-format12 stream)))
    (when (/= format 4)
      (error 'unsupported-format
             :location "\"cmap\" subtable"
             :actual-value format
             :expected-values (list 4))))
  (let ((table-start (- (file-position stream) 2))
        (subtable-length (read-uint16 stream))
        (language-code (read-uint16 stream))
        (segment-count (/ (read-uint16 stream) 2))
        (search-range (read-uint16 stream))
        (entry-selector (read-uint16 stream))
        (range-shift (read-uint16 stream)))
    (declare (ignore language-code search-range entry-selector range-shift))
    (flet ((make-and-load-array (&optional (size segment-count))
             (loop with array = (make-array size
                                            :element-type '(unsigned-byte 16)
                                            :initial-element 0)
                   for i below size
                   do (setf (aref array i) (read-uint16 stream))
                   finally (return array)))
           (make-signed (i)
             (if (logbitp 15 i)
                 (1- (- (logandc2 #xFFFF i)))
                 i)))
      (let ((end-codes (make-and-load-array))
            (pad (read-uint16 stream))
            (start-codes (make-and-load-array))
            (id-deltas (make-and-load-array))
            (id-range-offsets (make-and-load-array))
            (glyph-index-array-size (/ (- subtable-length
                                          (- (file-position stream)
                                             table-start))
                                       2)))
        (declare (ignore pad))
        (make-instance 'unicode-cmap
                       :segment-count segment-count
                       :end-codes end-codes
                       :start-codes start-codes
                       ;; these are really signed, so sign them
                       :id-deltas (map 'vector #'make-signed id-deltas)
                       :id-range-offsets id-range-offsets
                       :glyph-indexes (make-and-load-array glyph-index-array-size))))))


(defun %decode-format-4-cmap-code-point-index (code-point cmap index)
  "Return the index of the Unicode CODE-POINT in a format 4 CMAP, if
present, otherwise NIL. Assumes INDEX points to the element of the
CMAP arrays (END-CODES etc) corresponding to code-point."
  (with-slots (end-codes start-codes
               id-deltas id-range-offsets
               glyph-indexes)
      cmap
    (declare (type cmap-value-table
                   end-codes start-codes
                   id-range-offsets
                   glyph-indexes))
    (let ((start-code (aref start-codes index))
          (end-code (aref end-codes index))
          (id-range-offset (aref id-range-offsets index))
          (id-delta (aref id-deltas index)))
      (cond
        ((< code-point start-code)
         0)
        ;; ignore empty final segment
        ((and (= 65535 start-code end-code))
         0)
        ((zerop id-range-offset)
         (logand #xFFFF (+ code-point id-delta)))
        (t
         (let* ((glyph-index-offset (- (+ index
                                          (ash id-range-offset -1)
                                          (- code-point start-code))
                                       (segment-count cmap)))
                (glyph-index (aref (glyph-indexes cmap)
                                   glyph-index-offset)))
           (logand #xFFFF
                   (+ glyph-index id-delta))))))))

(defun %decode-format-12-cmap-code-point-index (code-point cmap index)
  "Return the index of the Unicode CODE-POINT in a format 12 CMAP, if
present, otherwise NIL. Assumes INDEX points to the element of the
CMAP arrays (END-CODES etc) corresponding to code-point."
  (with-slots (end-codes start-codes glyph-starts)
      cmap
    (declare (type (simple-array (unsigned-byte 32))
                   end-codes start-codes glyph-starts))
    (let ((start-code (aref start-codes index))
          (start-glyph-id (aref glyph-starts index)))
      (if (< code-point start-code)
          0
          (+ start-glyph-id (- code-point start-code))))))

(defgeneric code-point-font-index-from-cmap (code-point cmap)
  (:documentation "Return the index of the Unicode CODE-POINT in
CMAP, if present, otherwise NIL.")
  (:method (code-point (cmap unicode-cmap))
    (with-slots (end-codes)
        cmap
      (declare (type cmap-value-table end-codes))
      (dotimes (i (segment-count cmap) 1)
        (when (<= code-point (aref end-codes i))
          (return (%decode-format-4-cmap-code-point-index code-point cmap i))))))
  (:method (code-point (cmap format-12-cmap))
    (with-slots (end-codes)
        cmap
      (declare (type (simple-array (unsigned-byte 32)) end-codes))
      (dotimes (i (group-count cmap) 1)
        (when (<= code-point (aref end-codes i))
          (return
            (%decode-format-12-cmap-code-point-index code-point cmap i)))))))

(defmethod invert-character-map (font-loader)
  "Return a vector mapping font indexes to code points."
  (with-slots (start-codes end-codes)
      (character-map font-loader)
    (let ((points (make-array (glyph-count font-loader) :initial-element -1))
          (cmap (character-map font-loader)))
      (dotimes (i (length end-codes) points)
        (loop for j from (aref start-codes i) to (aref end-codes i)
              for font-index
                = (typecase cmap
                    (unicode-cmap
                     (%decode-format-4-cmap-code-point-index j cmap i))
                    (format-12-cmap
                     (%decode-format-12-cmap-code-point-index j cmap i))
                    (t
                     (code-point-font-index-from-cmap j cmap)))
              when (minusp (svref points font-index))
                do (setf (svref points font-index) j))))))


(defgeneric code-point-font-index (code-point font-loader)
  (:documentation "Return the index of the Unicode CODE-POINT in
FONT-LOADER, if present, otherwise NIL.")
  (:method (code-point font-loader)
    (code-point-font-index-from-cmap code-point (character-map font-loader))))

(defgeneric font-index-code-point (glyph-index font-loader)
  (:documentation "Return the code-point for a given glyph index.")
  (:method (glyph-index font-loader)
    (let ((point (aref (inverse-character-map font-loader) glyph-index)))
      (if (plusp point)
          point
          0))))

(defun %load-cmap-info (font-loader platform specific)
  (seek-to-table "cmap" font-loader)
  (with-slots (input-stream)
      font-loader
    (let ((start-pos (file-position input-stream))
          (version-number (read-uint16 input-stream))
          (subtable-count (read-uint16 input-stream))
          (foundp nil))
      (declare (ignore version-number))
      (loop repeat subtable-count
            for platform-id = (read-uint16 input-stream)
            for platform-specific-id = (read-uint16 input-stream)
            for offset = (+ start-pos (read-uint32 input-stream))
            when (and (= platform-id platform)
                      (or (eql platform-specific-id specific)
                          (and (consp specific)
                               (member platform-specific-id specific))))
            do
            (file-position input-stream offset)
            (setf (character-map font-loader) (load-unicode-cmap input-stream))
            (setf (inverse-character-map font-loader)
                  (invert-character-map font-loader)
                  foundp t)
            (return))
      foundp)))

(defun %unknown-cmap-error (font-loader)
  (seek-to-table "cmap" font-loader)
  (with-slots (input-stream)
      font-loader
    (let ((start-pos (file-position input-stream))
          (version-number (read-uint16 input-stream))
          (subtable-count (read-uint16 input-stream))
          (cmaps nil))
      (declare (ignore version-number))
      (loop repeat subtable-count
            for platform-id = (read-uint16 input-stream)
            for platform-specific-id = (read-uint16 input-stream)
            for offset = (+ start-pos (read-uint32 input-stream))
            for pos = (file-position input-stream)
            do (file-position input-stream offset)
               (push (list (platform-id-name platform-id)
                           (encoding-id-name platform-id platform-specific-id)
                           :type (read-uint16 input-stream))
                     cmaps)
               (file-position input-stream pos))
      (error "Could not find supported character map in font file~% available cmap tables = ~s"
             cmaps))))

(defmethod load-cmap-info ((font-loader font-loader))
  (or (%load-cmap-info font-loader +unicode-platform-id+
                       +unicode-2.0-full-encoding-id+) ;; full unicode
      (%load-cmap-info font-loader +microsoft-platform-id+
                       +microsoft-unicode-ucs4-encoding-id+) ;; full unicode
      (%load-cmap-info font-loader +microsoft-platform-id+
                       +microsoft-unicode-bmp-encoding-id+) ;; bmp
      (%load-cmap-info font-loader +unicode-platform-id+
                       +unicode-2.0-encoding-id+) ;; bmp
      (%load-cmap-info font-loader +unicode-platform-id+
                       '(0 1 2 3 4)) ;; all except variation and last-resort
      (%load-cmap-info font-loader +microsoft-platform-id+
                       +microsoft-symbol-encoding-id+) ;; ms symbol
      (%unknown-cmap-error font-loader)))

(defun available-character-maps (loader)
  (seek-to-table "cmap" loader)
  (let ((stream (input-stream loader)))
    (let ((start-pos (file-position stream))
          (version-number (read-uint16 stream))
          (subtable-count (read-uint16 stream)))
      (declare (ignore start-pos))
      (assert (zerop version-number))
      (dotimes (i subtable-count)
        (let ((platform-id (read-uint16 stream))
              (encoding-id (read-uint16 stream))
              (offset (read-uint32 stream)))
          (declare (ignore offset))
          (format t "~D (~A) - ~D (~A)~%"
                  platform-id (platform-id-name platform-id)
                  encoding-id (encoding-id-name platform-id encoding-id)))))))

;;; post
;; "post" table functions

;; ref: https://docs.microsoft.com/en-us/typography/opentype/spec/post
;; ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6post.html
(defvar *standard-mac-glyph-names*
  #(".notdef"
    ".null"
    "nonmarkingreturn"
    "space"
    "exclam"
    "quotedbl"
    "numbersign"
    "dollar"
    "percent"
    "ampersand"
    "quotesingle"
    "parenleft"
    "parenright"
    "asterisk"
    "plus"
    "comma"
    "hyphen"
    "period"
    "slash"
    "zero" "one" "two" "three" "four"
    "five" "six" "seven" "eight" "nine"
    "colon"
    "semicolon"
    "less"
    "equal"
    "greater"
    "question"
    "at"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "bracketleft"
    "backslash"
    "bracketright"
    "asciicircum"
    "underscore"
    "grave"
    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "braceleft"
    "bar"
    "braceright"
    "asciitilde"
    "Adieresis"
    "Aring"
    "Ccedilla"
    "Eacute"
    "Ntilde"
    "Odieresis"
    "Udieresis"
    "aacute"
    "agrave"
    "acircumflex"
    "adieresis"
    "atilde"
    "aring"
    "ccedilla"
    "eacute"
    "egrave"
    "ecircumflex"
    "edieresis"
    "iacute"
    "igrave"
    "icircumflex"
    "idieresis"
    "ntilde"
    "oacute"
    "ograve"
    "ocircumflex"
    "odieresis"
    "otilde"
    "uacute"
    "ugrave"
    "ucircumflex"
    "udieresis"
    "dagger"
    "degree"
    "cent"
    "sterling"
    "section"
    "bullet"
    "paragraph"
    "germandbls"
    "registered"
    "copyright"
    "trademark"
    "acute"
    "dieresis"
    "notequal"
    "AE"
    "Oslash"
    "infinity"
    "plusminus"
    "lessequal"
    "greaterequal"
    "yen"
    "mu"
    "partialdiff"
    "summation"
    "product"
    "pi"
    "integral"
    "ordfeminine"
    "ordmasculine"
    "Omega"
    "ae"
    "oslash"
    "questiondown"
    "exclamdown"
    "logicalnot"
    "radical"
    "florin"
    "approxequal"
    "Delta"
    "guillemotleft"
    "guillemotright"
    "ellipsis"
    "nonbreakingspace"
    "Agrave"
    "Atilde"
    "Otilde"
    "OE"
    "oe"
    "endash"
    "emdash"
    "quotedblleft"
    "quotedblright"
    "quoteleft"
    "quoteright"
    "divide"
    "lozenge"
    "ydieresis"
    "Ydieresis"
    "fraction"
    "currency"
    "guilsinglleft"
    "guilsinglright"
    "fi"
    "fl"
    "daggerdbl"
    "periodcentered"
    "quotesinglbase"
    "quotedblbase"
    "perthousand"
    "Acircumflex"
    "Ecircumflex"
    "Aacute"
    "Edieresis"
    "Egrave"
    "Iacute"
    "Icircumflex"
    "Idieresis"
    "Igrave"
    "Oacute"
    "Ocircumflex"
    "apple"
    "Ograve"
    "Uacute"
    "Ucircumflex"
    "Ugrave"
    "dotlessi"
    "circumflex"
    "tilde"
    "macron"
    "breve"
    "dotaccent"
    "ring"
    "cedilla"
    "hungarumlaut"
    "ogonek"
    "caron"
    "Lslash"
    "lslash"
    "Scaron"
    "scaron"
    "Zcaron"
    "zcaron"
    "brokenbar"
    "Eth"
    "eth"
    "Yacute"
    "yacute"
    "Thorn"
    "thorn"
    "minus"
    "multiply"
    "onesuperior"
    "twosuperior"
    "threesuperior"
    "onehalf"
    "onequarter"
    "threequarters"
    "franc"
    "Gbreve"
    "gbreve"
    "Idotaccent"
    "Scedilla"
    "scedilla"
    "Cacute"
    "cacute"
    "Ccaron"
    "ccaron"
    "dcroat"))

(defun load-post-format-2 (names stream size-without-header)
  (let* ((standard-names *standard-mac-glyph-names*)
         (name-count (length names))
         (glyph-count (read-uint16 stream)))
    (when (/= glyph-count name-count)
      (warn "Glyph count in \"post\" table (~D) ~
             does not match glyph count in \"maxp\" table (~D). ~
             This font may be broken."
            glyph-count name-count))
    ;; This is done in a couple passes. First, initialize the names
    ;; tables with indexes into either the standard table or the
    ;; pstring table.
    (dotimes (i glyph-count)
      (setf (aref names i) (read-uint16 stream)))
    ;; Next, read the pstring table into a vector.
    ;; We can't know the number of extended glyph names in advance but
    ;; GLYPH-COUNT should be enough in many cases. Note that we cannot
    ;; compute the number of extended glyph names from the indices
    ;; preceding the indices might not reference all names.
    (let ((pstrings (make-array glyph-count :adjustable t :fill-pointer 0)))
      (loop with position = (+ 2 (* 2 glyph-count))
            while (< position size-without-header)
            do (let ((string (read-pstring stream)))
                 (vector-push-extend string pstrings)
                 (incf position (1+ (length string)))))
      ;; Finally, replace the indexes with names.
      (loop for i below glyph-count
            for name-index across names
            do (setf (aref names i)
                     (if (< name-index 258)
                         (aref standard-names name-index)
                         (aref pstrings (- name-index 258))))))))

(defun load-post-format-3 (names stream)
  (declare (ignore stream))
  (fill names nil))

(defmethod load-post-info ((font-loader font-loader))
  (let* ((names (make-array (glyph-count font-loader)
                            :initial-element 0))
         (stream (input-stream font-loader))
         (table-info (table-info "post" font-loader)))
    (seek-to-table table-info font-loader)
    (let ((format (read-uint32 stream))
          (header-size 32))
      (when (/= format #x00020000 #x00030000)
        (error 'unsupported-format
               :location "\"post\" table"
               :expected-values (list #x00020000 #x00030000)
               :actual-value format))
      (setf (italic-angle font-loader) (read-fixed stream)
            (underline-position font-loader) (read-fword stream)
            (underline-thickness font-loader) (read-fword stream)
            (fixed-pitch-p font-loader) (plusp (read-uint32 stream))
            (postscript-glyph-names font-loader) names)
      ;; skip minMemType* fields
      (advance-file-position stream (- header-size 16))
      (case format
        (#x00020000 (load-post-format-2
                     names stream (- (size table-info) header-size)))
        (#x00030000 (load-post-format-3 names stream))))))

(defun postscript-uni-name-p (name)
  (let ((end (or (position #\. name) (length name))))
    (and (= end 7)
         (= (mismatch "uni" name) 3)
         (loop for i from 3 below end
               always (digit-char-p (char name i) 16)))))

(defun postscript-name-code-point (name)
  "Returns, if available, the interpretation of the PostScript name NAME as a Unicode code point specifier.
Ref: http://partners.adobe.com/public/developer/opentype/index_glyph.html"
  (when (postscript-uni-name-p name)
    (parse-integer name :start 3 :end 7 :radix 16)))

;;; hhea
;; Loading data from the "hhea" table.

;; ref: https://learn.microsoft.com/en-us/typography/opentype/spec/hhea
;; ref: https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hhea.html
(defmethod load-hhea-info ((font-loader font-loader))
  (seek-to-table "hhea" font-loader)
  (with-slots (input-stream ascender descender line-gap max-width)
      font-loader
    (let ((version (read-fixed input-stream)))
      (check-version "\"hhea\" table" version #x00010000))
    (setf ascender (read-fword input-stream)
          descender (read-fword input-stream)
          line-gap (read-fword input-stream)
          max-width (read-ufword input-stream))))

(defmethod horizontal-metrics-count ((font-loader font-loader))
  (seek-to-table "hhea" font-loader)
  (with-slots (input-stream) font-loader
    ;; Skip to the end, since all we care about is the last item
    (advance-file-position input-stream 34)
    (read-uint16 input-stream)))

;;; hmtx
;; Loading data from the "hmtx" table.

;; ref: https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx
;; ref: https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6hmtx.html
(defmethod load-hmtx-info ((font-loader font-loader))
  (let* ((horizontal-metrics-count (horizontal-metrics-count font-loader))
         (advance-widths (make-array horizontal-metrics-count))
         (left-side-bearings (make-array horizontal-metrics-count)))
    (seek-to-table "hmtx" font-loader)
    (with-slots (input-stream) font-loader
      (dotimes (i horizontal-metrics-count)
        (setf (svref advance-widths i) (read-uint16 input-stream))
        (setf (svref left-side-bearings i) (read-int16 input-stream))))
    (setf (advance-widths font-loader) advance-widths
          (left-side-bearings font-loader) left-side-bearings)))

;;; vhea
;; Loading data from the "vhea" table.

;; ref: https://learn.microsoft.com/en-us/typography/opentype/spec/vhea
;; ref: https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6vhea.html

;; Tables 'vhea' and 'vmtx' are not present in some fonts. For that reason we
;; have a fallback where metrics are supplanted with default values based on
;; horizontal metrics.
(defmethod load-vhea-info ((font-loader font-loader))
  (unless (table-info "vhea" font-loader)
    (setf (vhea-missing-p font-loader) t)
    (let ((dx (/ (max-width font-loader) 2)))
      (with-slots (vascender vdescender)
          font-loader
        (setf vascender dx
              vdescender (- dx))))
    (return-from load-vhea-info))
  (seek-to-table "vhea" font-loader)
  (with-slots (input-stream vascender vdescender)
      font-loader
    (let ((version (read-fixed input-stream)))
      (check-version "\"vhea\" table" version #x00010000 #x00011000))
    (setf vascender (read-fword input-stream)
          vdescender (read-fword input-stream))))

(defmethod vertical-metrics-count ((font-loader font-loader))
  (when (or (vhea-missing-p font-loader)
            (null (table-info "vhea" font-loader)))
    ;; (warn "Table 'vhea' is missing.")
    (setf (vhea-missing-p font-loader) t)
    (return-from vertical-metrics-count))
  (seek-to-table "vhea" font-loader)
  (with-slots (input-stream) font-loader
    ;; Skip to the end, since all we care about is the last item
    (advance-file-position input-stream 34)
    (read-uint16 input-stream)))

;;; vmtx
;; Loading data from the 'vmtx' table.

;; ref: https://learn.microsoft.com/en-us/typography/opentype/spec/vmtx
;; ref: https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6vmtx.html

;; Tables 'vhea' and 'vmtx' are not present in some fonts. For that reason we
;; have a fallback where metrics are supplanted with default values based on
;; horizontal metrics.
(defmethod load-vmtx-info ((font-loader font-loader))
  (when (or (vhea-missing-p font-loader)
            (null (table-info "vmtx" font-loader)))
    (setf (vmtx-missing-p font-loader) t)
    (let ((line-height (- (ascender font-loader) (descender font-loader))))
      ;; TOP-SIDE-BEARING depends on individual glyph metric YMAX.
      (setf (advance-heights font-loader)
            (make-array 1 :initial-element line-height)))
    (return-from load-vmtx-info))
  (let* ((vertical-metrics-count (vertical-metrics-count font-loader))
         (advance-heights (make-array vertical-metrics-count))
         (top-side-bearings (make-array vertical-metrics-count)))
    (seek-to-table "vmtx" font-loader)
    (with-slots (input-stream) font-loader
      (dotimes (i vertical-metrics-count)
        (setf (svref advance-heights i) (read-uint16 input-stream))
        (setf (svref top-side-bearings i) (read-int16 input-stream))))
    (setf (advance-heights font-loader) advance-heights
          (top-side-bearings font-loader) top-side-bearings)))

;;; glyf
;; Loading data from the 'glyf' table.
(defclass control-point ()
  ((x :initarg :x :accessor cp-x)
   (y :initarg :y :accessor cp-y)
   (on-curve-p :initarg :on-curve-p :reader on-curve-p)))

(defun make-control-point (x y on-curve-p)
  (make-instance 'control-point
                 :x x
                 :y y
                 :on-curve-p on-curve-p))

(defmethod print-object ((control-point control-point) stream)
  (print-unreadable-object (control-point stream :type t)
    (format stream "~D,~D~:[~;*~]"
            (cp-x control-point) (cp-y control-point) (on-curve-p control-point))))

(defmacro do-contour-segments* ((p1 p2) contour &body body)
  (let ((length (gensym))
        (i (gensym))
        (stack (gensym))
        (next (gensym))
        (next-point (gensym "NEXT-POINT"))
        (midpoint (gensym "MIDPOINT"))
        (contour* (gensym))
        (loop (gensym "LOOP"))
        (body-tag (gensym "BODY"))
        (done-tag (gensym "DONE"))
        (mid p1)
        (end p2))
    `(let* ((,i 1)
            (,contour* ,contour)
            (,length (length ,contour*))
            ,stack ,next ,mid ,end)
       (unless (zerop ,length)
         (unless (on-curve-p (aref ,contour* 0))
           (setf ,stack (aref ,contour* 0)))
         (flet ((,next-point ()
                  (when (< ,i ,length)
                    (prog1 (aref ,contour* ,i) (incf ,i))))
                (,midpoint (p0 p1)
                  (make-control-point (/ (+ (cp-x p0) (cp-x p1)) 2)
                                      (/ (+ (cp-y p0) (cp-y p1)) 2)
                                      t)))
           (tagbody
              ,loop
              (setf ,mid nil
                    ,next (,next-point))
              (unless ,next
                (setf ,mid ,stack
                      ,end (aref ,contour* 0))
                (cond
                  ((on-curve-p ,end)
                   (go ,body-tag))
                  (,stack
                   (setf ,mid ,stack
                         ,end (,midpoint ,stack ,end))
                   (go ,body-tag))
                  (t (go ,done-tag))))
              (if (on-curve-p ,next)
                  (setf ,end ,next
                        ,mid ,stack
                        ,stack nil)
                  (cond (,stack
                         (setf ,mid ,stack
                               ,end (,midpoint ,stack ,next)
                               ,stack ,next))
                        (t
                         (setf ,stack ,next)
                         (go ,loop))))
              ,body-tag
              ,@body
              (when ,next
                (go ,loop))
              ,done-tag))))))

(defun start-of-contour (contour)
  "If first point of a contour is on the curve, return it, otherwise
find and return previous (possibly implicit) point on the curve."
  (let ((first (aref contour 0)))
   (if (on-curve-p first)
       first
       (let ((last (aref contour (1- (length contour)))))
         (if (on-curve-p last)
             last
             ;; both are off curve, return the implicit on-curve point
             (make-control-point (/ (+ (cp-x first) (cp-x last)) 2)
                                 (/ (+ (cp-y first) (cp-y last)) 2)
                                 t))))))

(defmacro do-contour-segments ((p0 p1 p2) contour &body body)
    "A contour is made up of segments. A segment may be a straight line
or a curve. For each segment, bind the P0 and P2 variables to the
start and end points of the segment. If the segment is a curve, set P1
to the control point of the curve, otherwise set P1 to NIL."
    ;; This macro started out life as a function and was converted.
    (let ((start p0)
          (contour* (gensym "CONTOUR")))
      `(let ((,contour* ,contour))
         (when (plusp (length ,contour*))
           (let ((,start (start-of-contour ,contour*)))
             (do-contour-segments* (,p1 ,p2)
                 ,contour*
               (progn ,@body)
               (setf ,start ,p2)))))))

(defun explicit-contour-points (contour)
  (let ((new-contour (make-array (length contour)
                                 :adjustable t
                                 :fill-pointer 0)))
    (when (and (plusp (length contour))
               (on-curve-p (aref contour 0)))
      (vector-push-extend (aref contour 0) new-contour))
    (do-contour-segments* (p1 p2)
        contour
      (when p1
        (vector-push-extend p1 new-contour))
      (unless (eql p2 (aref contour 0))
        (vector-push-extend p2 new-contour)))
    new-contour))


;;; Locating a glyph's contours and bounding box in the font loader's
;;; stream, and loading them

(defparameter *empty-contours*
  (make-array 0 :element-type '(signed-byte 16)))

(defparameter *empty-bounding-box*
  (make-array 4
              :initial-element 0
              :element-type '(signed-byte 16)))

(defun empty-bounding-box ()
  (copy-seq *empty-bounding-box*))

(defun empty-contours ()
  (copy-seq *empty-contours*))

(defun dump-compound-flags (flags)
  (format t "XXX flags=~16,'0B~%" flags)
  (let ((meanings '((0 . ARG_1_AND_2_ARE_WORDS)
                       (1 . ARGS_ARE_XY_VALUES)
                       (2 . ROUND_XY_TO_GRID)
                       (3 . WE_HAVE_A_SCALE)
                       (4 . OBSOLETE)
                       (5 . MORE_COMPONENTS)
                       (6 . WE_HAVE_AN_X_AND_Y_SCALE)
                       (7 . WE_HAVE_A_TWO_BY_TWO)
                       (8 . WE_HAVE_INSTRUCTIONS)
                       (9 . USE_MY_METRICS)
                       (10 . OVERLAP_COMPOUND))))
       (loop for ((bit . meaning)) on meanings
             do (when (logbitp bit flags)
                  (format t "...~A~%" meaning)))))

(defun transform-option-count (flags)
  (let ((scale-p 3)
        (xy-scale-p 6)
        (2*2-scale-p 7))
    (cond ((logbitp scale-p flags) 1)
          ((logbitp xy-scale-p flags) 2)
          ((logbitp 2*2-scale-p flags) 4)
          (t 0))))

(defun make-transformer (a b c d e f)
  "Given the elements of the transformation matrix specified by A, B,
C, D, E, and F, return a function of two arguments that returns the
arguments transformed as multiple values.
Ref: http://developer.apple.com/fonts/TTRefMan/RM06/Chap6glyf.html"
  (let ((m (max (abs a) (abs b)))
        (n (max (abs c) (abs d))))
    (when (<= (abs (- (abs a) (abs b))) 33/65536)
      (setf m (* m 2)))
    (when (<= (abs (- (abs c) (abs d))) 33/65536)
      (setf n (* n 2)))
    (lambda (x y)
      (values (* m (+ (* (/ a m) x)
                      (* (/ c m) y)
                      e))
              (* n (+ (* (/ b n) x)
                      (* (/ d n) y)
                      f))))))

(defun transform-contours (fn contours)
  "Call FN with the X and Y coordinates of each point of each contour
in the vector CONTOURS. FN should return two values, which are used to
update the X and Y values of each point."
  (loop for contour across contours do
        (loop for p across contour do
              (setf (values (cp-x p) (cp-y p))
                    (funcall fn (cp-x p) (cp-y p))))))

(defun merge-contours (contours-list)
  (let* ((total-contours (loop for contours in contours-list
                               summing (length contours)))
         (merged (make-array total-contours))
         (i 0))
    (dolist (contours contours-list merged)
      (loop for contour across contours do
            (setf (aref merged i) contour)
            (incf i)))))

(defvar *compound-contour-loop-check*)

(defun read-compound-contours (loader)
  (let ((contours-list '())
        (stream (input-stream loader)))
    (loop
     (let ((flags (read-uint16 stream))
           (font-index (read-uint16 stream)))
       (let ((position (file-position stream))
             (contours (read-contours-at-index font-index loader)))
         (push contours contours-list)
         (file-position stream position)
         (let ((args-words-p (logbitp 0 flags))
               (args-xy-values-p (logbitp 1 flags))
               (more-components-p (logbitp 5 flags))
               arg1 arg2)
           (cond ((and args-words-p args-xy-values-p)
                  (setf arg1 (read-int16 stream)
                        arg2 (read-int16 stream)))
                 (args-words-p
                  (setf arg1 (read-uint16 stream)
                        arg2 (read-uint16 stream))
                  (error "Compound glyphs relative to indexes not yet supported"))
                 (args-xy-values-p
                  (setf arg1 (read-int8 stream)
                        arg2 (read-int8 stream)))
                 (t
                  (setf arg1 (read-uint8 stream)
                        arg2 (read-uint8 stream))
                  (error "Compound glyphs relative to indexes not yet supported")))
           ;; Transform according to the transformation matrix
           (let ((a 1.0) (b 0.0) (c 0.0) (d 1.0)
                 (e arg1) (f arg2))
             (ecase (transform-option-count flags)
               (0)
               (1
                (setf a (setf d (read-fixed2.14 stream))))
               (2
                (setf a (read-fixed2.14 stream)
                      d (read-fixed2.14 stream)))
               (4
                (setf a (read-fixed2.14 stream)
                      b (read-fixed2.14 stream)
                      c (read-fixed2.14 stream)
                      d (read-fixed2.14 stream))))
             (let ((transform-fn (make-transformer a b c d e f)))
               (transform-contours transform-fn contours)))
           (unless more-components-p
             (return (merge-contours contours-list)))))))))

(defun read-points-vector (stream flags count axis)
  (let ((points (make-array count :fill-pointer 0))
        (short-index (if (eql axis :x) 1 2))
        (same-index (if (eql axis :x) 4 5)))
    (flet ((save-point (point)
             (vector-push point points)))
      (loop for flag across flags
            for short-p = (logbitp short-index flag)
            for same-p = (logbitp same-index flag)
            do (cond (short-p
                      (let ((new-point (read-uint8 stream)))
                        (save-point (if same-p new-point (- new-point)))))
                     (t
                      (if same-p
                          (save-point 0)
                          (save-point (read-int16 stream)))))))
    points))

(defun read-simple-contours (contour-count stream)
  "With the stream positioned immediately after the glyph bounding
box, read the contours data from STREAM and return it as a vector."
  (let ((contour-endpoint-indexes (make-array contour-count)))
    (loop for i below contour-count
          for endpoint-index = (read-uint16 stream)
          do (setf (svref contour-endpoint-indexes i) endpoint-index))
    ;; instructions
    (let ((n-points (1+ (svref contour-endpoint-indexes
                               (1- contour-count))))
          (instruction-length (read-uint16 stream)))
      (loop for i below instruction-length
            do (read-byte stream))
      ;; read the flags
      (let ((flags (make-array n-points)))
        (loop with i = 0
              while (< i n-points) do
              (let ((flag-byte (read-uint8 stream)))
                (setf (svref flags i) flag-byte)
                (incf i)
                (when (logbitp 3 flag-byte)
                  (let ((n-repeats (read-uint8 stream)))
                    (loop repeat n-repeats do
                          (setf (svref flags i) flag-byte)
                          (incf i))))))
        (let ((x-points (read-points-vector stream flags n-points :x ))
              (y-points (read-points-vector stream flags n-points :y))
              (control-points (make-array n-points :fill-pointer 0))
              (contours (make-array contour-count)))
          (loop for x-point across x-points
                for y-point across y-points
                for flag across flags
                for x = x-point then (+ x x-point)
                for y = y-point then (+ y y-point)
                do
                (vector-push-extend (make-control-point x y
                                                        (logbitp 0 flag))
                                    control-points))
          (loop for start = 0 then (1+ end)
                for end across contour-endpoint-indexes
                for i from 0
                do (setf (svref contours i)
                         (subseq control-points start (1+ end))))
          contours)))))

(defmacro with-compound-contour-loop (() &body body)
  `(let ((*compound-contour-loop-check*
           (if (boundp '*compound-contour-loop-check*)
               *compound-contour-loop-check*
               (make-hash-table))))
     ,@body))

(defun read-contours-at-index (index loader)
  "Read the contours at glyph index INDEX, discarding bounding box
information."
  (let ((stream (input-stream loader)))
    (file-position stream (+ (table-position "glyf" loader)
                             (glyph-location index loader)))
    (let ((contour-count (read-int16 stream))
          (xmin (read-int16 stream))
          (ymin (read-int16 stream))
          (xmax (read-int16 stream))
          (ymax (read-int16 stream)))
      (declare (ignore xmin ymin xmax ymax))
      (if (= contour-count -1)
          (with-compound-contour-loop ()
            ;; some fonts have compound contours that contain
            ;; themselves, so we try to detect that.
            (when (gethash index *compound-contour-loop-check*)
              (return-from read-contours-at-index
                (gethash index *compound-contour-loop-check*)))
            ;; store a value for when we detect a loop
            (setf (gethash index *compound-contour-loop-check*)
                  #())
            ;; It is reasonable for a particular contour to be
            ;; included multiple times within the tree of compounds,
            ;; though, so for that case we save the value and reuse
            ;; it.
            (setf (gethash index *compound-contour-loop-check*)
                  (read-compound-contours loader)))
          (read-simple-contours contour-count stream)))))

;;; glyph
;; An object for working with glyphs from the font. Some fields are
;; lazily loaded from the input-stream of the font-loader when needed.
(defclass glyph ()
  ((font-loader
    :initarg :font-loader
    :reader font-loader
    :documentation "The font-loader from which this glyph originates.")
   (font-index
    :initarg :font-index
    :accessor font-index
    :documentation "The index of this glyph within the font file, used
to look up information in various structures in the truetype file.")
   (code-point
    :initarg :code-point
    :accessor code-point)
   (contours
    :initarg :contours
    :accessor contours)
   (bounding-box
    :initarg :bounding-box
    :accessor bounding-box)))

(defmethod initialize-instance :after ((glyph glyph)
                                       &key code-point font-index font-loader
                                       &allow-other-keys)
  (flet ((argument-error (name)
           (error "Missing required initarg ~S" name)))
    (unless font-loader
      (argument-error :font-loader))
    (cond ((and code-point font-index))  ;; do nothing
          (code-point
           (setf (font-index glyph)
                 (code-point-font-index code-point font-loader)))
          (font-index
           (let ((code-point (font-index-code-point font-index font-loader)))
             (when (zerop code-point)
               (setf code-point
                     (or (postscript-name-code-point (postscript-name glyph))
                         code-point)))
             (setf (code-point glyph) code-point)))
          (t
           (argument-error (list :font-index :code-point))))))

(defmethod print-object ((glyph glyph) stream)
  (print-unreadable-object (glyph stream :type t :identity nil)
    ;; FIXME: Is this really going to be Unicode?
    (format stream "~S U+~4,'0X"
            (postscript-name glyph)
            (code-point glyph))))

;;;; Horizontal metrics
(defgeneric left-side-bearing (object)
  (:method ((glyph glyph))
    (bounded-aref (left-side-bearings (font-loader glyph))
                  (font-index glyph))))

(defmethod (setf left-side-bearing) (new-value glyph)
  (setf (bounded-aref (left-side-bearings (font-loader glyph))
                      (font-index glyph))
        new-value))

(defgeneric advance-width (object)
  (:method ((glyph glyph))
    (bounded-aref (advance-widths (font-loader glyph))
                  (font-index glyph))))

(defmethod (setf advance-width) (new-value (glyph glyph))
  (setf (bounded-aref (advance-widths (font-loader glyph))
                      (font-index glyph))
        new-value))

;;;; Vertical metrics
(defgeneric top-side-bearing (object)
  (:method ((glyph glyph))
    (let ((loader (font-loader glyph)))
      (if (vmtx-missing-p loader)
          (- (ascender loader) (bbox-ymax glyph))
          (bounded-aref (top-side-bearings (font-loader glyph))
                        (font-index glyph))))))

(defmethod (setf top-side-bearing) (new-value glyph)
  (setf (bounded-aref (top-side-bearings (font-loader glyph))
                      (font-index glyph))
        new-value))

(defgeneric advance-height (object)
  (:method ((glyph glyph))
    (bounded-aref (advance-heights (font-loader glyph))
                  (font-index glyph))))

(defmethod (setf advance-height) (new-value (glyph glyph))
  (setf (bounded-aref (advance-heights (font-loader glyph))
                      (font-index glyph))
        new-value))

;;;; Kerning
(defgeneric kerning-offset (left right loader))

(defmethod kerning-offset ((left-glyph glyph) (right-glyph glyph)
                           (font-loader font-loader))
  (let ((kerning-table-key (logior (ash (font-index left-glyph) 16)
                                   (font-index right-glyph))))
    (gethash kerning-table-key (kerning-table font-loader) 0)))

(defmethod kerning-offset ((left character) (right character)
                           (font-loader font-loader))
  (kerning-offset (find-glyph left font-loader)
                  (find-glyph right font-loader)
                  font-loader))

(defmethod kerning-offset ((left null) right font-loader)
  (declare (ignore left right font-loader))
  0)

(defmethod kerning-offset (left (right null) font-loader)
  (declare (ignore left right font-loader))
  0)

(defgeneric kerned-advance-width (object next)
  (:method ((object glyph) next)
    (+ (advance-width object)
       (kerning-offset object next (font-loader object)))))

(defgeneric location (object)
  (:method ((glyph glyph))
    (with-slots (font-index font-loader)
        glyph
      (+ (table-position "glyf" font-loader)
         (glyph-location font-index font-loader)))))

(defgeneric data-size (object)
  (:method ((glyph glyph))
    (with-slots (font-index font-loader)
        glyph
      (- (glyph-location (1+ font-index) font-loader)
         (glyph-location font-index font-loader)))))

;;;; Initializing delayed data
(defmethod initialize-bounding-box ((glyph glyph))
  (if (zerop (data-size glyph))
      (setf (bounding-box glyph) (empty-bounding-box))
      (let ((stream (input-stream (font-loader glyph))))
        ;; skip contour-count
        (file-position stream (+ (location glyph) 2))
        (setf (bounding-box glyph)
              (vector (read-fword stream)
                      (read-fword stream)
                      (read-fword stream)
                      (read-fword stream))))))

(defmethod initialize-contours ((glyph glyph))
  (if (zerop (data-size glyph))
      (setf (contours glyph) (empty-contours))
      (let ((stream (input-stream (font-loader glyph))))
        (file-position stream (location glyph))
        (let ((contour-count (read-int16 stream)))
          ;; skip glyph bounding box, 4 FWords
          (advance-file-position stream 8)
          (if (= contour-count -1)
              (setf (contours glyph)
                    (read-compound-contours (font-loader glyph)))
              (setf (contours glyph)
                    (read-simple-contours contour-count stream)))))))

(defmethod bounding-box :before ((glyph glyph))
  (unless (slot-boundp glyph 'bounding-box)
    (initialize-bounding-box glyph)))

(defmethod contours :before ((glyph glyph))
  (unless (slot-boundp glyph 'contours)
    (initialize-contours glyph)))

(defgeneric contour-count (object)
  (:method (object)
    (length (contours object))))

(defgeneric contour (object idex)
  (:method (object index)
    (aref (contours object) index)))

(defmacro do-contours ((contour object &optional result) &body body)
  (let ((i (gensym))
        (obj (gensym)))
    `(let ((,obj ,object))
       (dotimes (,i (contour-count ,obj) ,result)
         (let ((,contour (contour ,obj ,i)))
           ,@body)))))

(defgeneric right-side-bearing (object)
  (:method ((glyph glyph))
    (- (advance-width glyph)
       (- (+ (left-side-bearing glyph) (bbox-xmax glyph))
          (bbox-xmin glyph)))))

;;;; Producing a bounding box for a sequence of characters
(defgeneric string-bounding-box (string loader &key kerning))

(defmethod string-bounding-box (string (font-loader font-loader)
                                &key (kerning t))
  (cond ((zerop (length string))
         (empty-bounding-box))
        ((= 1 (length string))
         (copy-seq (bounding-box (find-glyph (char string 0) font-loader))))
        (t
         (let ((origin 0)
               (left (find-glyph (char string 0) font-loader))
               (xmin most-positive-fixnum) (ymin most-positive-fixnum)
               (xmax most-negative-fixnum) (ymax most-negative-fixnum))
           (flet ((update-bounds (glyph)
                    (setf xmin (min (+ (bbox-xmin glyph) origin) xmin)
                          xmax (max (+ (bbox-xmax glyph) origin) xmax)
                          ymin (min (bbox-ymin glyph) ymin)
                          ymax (max (bbox-ymax glyph) ymax))))
             (update-bounds left)
             (loop for i from 1 below (length string)
                   for glyph = (find-glyph (char string i) font-loader)
                   do
                   (incf origin (advance-width left))
                   (when kerning
                     (incf origin (kerning-offset left glyph font-loader)))
                   (setf left glyph)
                   (update-bounds glyph)))
           (vector xmin ymin xmax ymax)))))

;;;; Producing glyphs from loaders
(defgeneric glyph-exists-p (character font-loader)
  (:method ((character glyph) font-loader)
    (let ((index (font-index character)))
      (not (zerop index))))
  (:method (character font-loader)
    (glyph-exists-p (find-glyph character font-loader) font-loader)))

(defgeneric find-glyph (character font-loader)
  (:documentation "Find the glyph object for CHARACTER in FONT-LOADER
and return it. If CHARACTER is an integer, treat it as a Unicode code
point. If CHARACTER is a Lisp character, treat its char-code as a
Unicode code point.")
  (:method ((character integer) (font-loader font-loader))
    (index-glyph (code-point-font-index character font-loader) font-loader))
  (:method ((character character) (font-loader font-loader))
    (find-glyph (char-code character) font-loader)))

(defgeneric index-glyph (index font-loader)
  (:documentation "Return the GLYPH object located at glyph index
INDEX in FONT-LOADER, or NIL if no glyph is defined for that
index. Despite the name, NOT the inverse of GLYPH-INDEX.")
  (:method (index font-loader)
    (let* ((cache (glyph-cache font-loader))
           (glyph (aref cache index)))
      (if glyph
          glyph
          (setf (aref cache index)
                (make-instance 'glyph
                               :font-index index
                               :font-loader font-loader))))))

;;;; Misc
(defmethod postscript-name ((glyph glyph))
  (let* ((names (postscript-glyph-names (font-loader glyph)))
         (index (font-index glyph))
         (name (aref names index)))
    (cond (name)
          ((slot-boundp glyph 'code-point)
           (setf (aref names index)
                 (format nil "uni~4,'0X" (code-point glyph))))
          (t "unknown"))))

;;; font-loader-interface
;; Interface functions for creating, initializing, and closing a FONT-LOADER
;; object.
(defun arrange-finalization (object stream)
  (flet ((quietly-close (&optional object)
           (declare (ignore object))
           (ignore-errors (close stream))))
    (sb-ext:finalize object #'quietly-close)))

(defun check-magic (magic &rest ok)
  (cond
    ((member magic ok)
     t)
    ((= magic (tag->number "typ1"))
     (error 'unsupported-format
            :location "font header"
            :description "Old style of PostScript font housed in a sfnt wrapper not supported."
            :actual-value magic
            :expected-values ok))
    ((= magic (tag->number "OTTO"))
     (error 'unsupported-format
            :location "font header"
            :description "OpenType font with PostScript outlines not supported."
            :actual-value magic
            :expected-values ok))
    (t
     (error 'bad-magic
            :location "font header"
            :expected-values ok
            :actual-value magic))))

;; FIXME: move most/all of this stuff into initialize-instance
(defun open-font-loader-from-stream (input-stream &key (collection-index 0))
  (let ((magic (read-uint32 input-stream))
        (font-count))
    (check-magic magic #x00010000
                 (tag->number "true")
                 (tag->number "ttcf"))
    (when (= magic (tag->number "ttcf"))
      (let ((version (read-uint32 input-stream)))
        (check-version "ttc header" version #x00010000 #x00020000)
        (setf font-count (read-uint32 input-stream))
        (let* ((offset-table (make-array font-count))
               (dsig))
          (when (> collection-index font-count)
            (error 'unsupported-value
                   :description "Font index out of range"
                   :actual-value collection-index
                   :expected-values (list font-count)))
          (loop for i below font-count
                do (setf (aref offset-table i) (read-uint32 input-stream)))
          (when (= version #x00020000)
            (let ((flag (read-uint32 input-stream))
                  (length (read-uint32 input-stream))
                  (offset (read-uint32 input-stream)))
              (list flag length offset)
              (when (= #x44534947 flag)
                (setf dsig (list length offset)))))
          ;; seek to font offset table
          (file-position input-stream (aref offset-table collection-index))
          (let ((magic2 (read-uint32 input-stream)))
            (check-magic magic2 #x00010000 (tag->number "true"))))))

    (let* ((table-count (read-uint16 input-stream))
           (font-loader (make-instance 'font-loader
                                       :input-stream input-stream
                                       :table-count table-count
                                       :collection-font-cont font-count
                                       :collection-font-index
                                       (when font-count
                                         collection-index))))
      ;; skip the unused stuff:
      ;; searchRange, entrySelector, rangeShift
      (read-uint16 input-stream)
      (read-uint16 input-stream)
      (read-uint16 input-stream)
      (loop repeat table-count
            for tag = (read-uint32 input-stream)
            for checksum = (read-uint32 input-stream)
            for offset = (read-uint32 input-stream)
            for size = (read-uint32 input-stream)
            do (setf (gethash tag (tables font-loader))
                     (make-instance 'table-info
                                    :offset offset
                                    :name (number->tag tag)
                                    :size size)))
      (load-maxp-info font-loader)
      (load-head-info font-loader)
      (load-kern-info font-loader)
      (load-loca-info font-loader)
      (load-name-info font-loader)
      (load-cmap-info font-loader)
      (load-post-info font-loader)
      (load-hhea-info font-loader)
      (load-hmtx-info font-loader)
      (load-vhea-info font-loader)
      (load-vmtx-info font-loader)
      (setf (glyph-cache font-loader)
            (make-array (glyph-count font-loader) :initial-element nil))
      font-loader)))

(defun open-font-loader-from-file (thing &key (collection-index 0))
  (let ((stream (open thing
                      :direction :input
                      :element-type '(unsigned-byte 8))))
    (let ((font-loader (open-font-loader-from-stream
                        stream :collection-index collection-index)))
      (arrange-finalization font-loader stream)
      font-loader)))

(defun open-font-loader (thing &key (collection-index 0))
  (typecase thing
    (font-loader
     (cond
       ;; We either don't have a collection, or want same font from
       ;; collection.
       ((or (not (collection-font-index thing))
            (= collection-index (collection-font-index thing)))
        (unless (open-stream-p (input-stream thing))
          (setf (input-stream thing) (open (input-stream thing))))
        thing)
       (t
        (open-font-loader-from-file (input-stream thing)
                                    :collection-index collection-index))))
    (stream
     (if (open-stream-p thing)
         (open-font-loader-from-stream thing :collection-index collection-index)
         (error "~A is not an open stream" thing)))
    (t
     (open-font-loader-from-file thing :collection-index collection-index))))

(defun close-font-loader (loader)
  (close (input-stream loader)))

(defmacro with-font-loader ((loader file &key (collection-index 0)) &body body)
  `(let (,loader)
    (unwind-protect
         (progn
           (setf ,loader (open-font-loader ,file
                                           :collection-index ,collection-index))
           ,@body)
      (when ,loader
        (close-font-loader ,loader)))))

;;; Font Cache
(defun ttf-pathname-p (pathname)
  (string-equal "ttf" (pathname-type pathname)))

(defvar *font-dirs* 
  (list "/usr/share/fonts/" 
        (namestring (merge-pathnames ".fonts/" (user-homedir-pathname))))
    "List of directories, which contain TrueType fonts.")

(defparameter *font-cache* (make-hash-table :test 'equal)
  "Hashmap for caching font families, subfamilies and files.")

;; (pushnew (xlib:font-path *display*) *font-dirs*)
(defun cache-font-file (pathname)
  "Caches font file."
  (handler-case
      (with-font-loader (font pathname)
        (multiple-value-bind (hash-table exists-p)
            (gethash (family-name font) *font-cache*
                     (make-hash-table :test 'equal))
          (setf (gethash (subfamily-name font) hash-table)
                pathname)
          (unless exists-p
            (setf (gethash (family-name font) *font-cache*)
                  hash-table))))
    (condition () (return-from cache-font-file))))

(defun cache-fonts ()
  "Caches fonts from *font-dirs* directories."
  (clrhash *font-cache*)
  (dolist (font-dir *font-dirs*)
    (walk-directory font-dir (constantly t) (constantly t) 
                    (lambda (x)
                      (dolist (f (directory-files x))
                        (when (ttf-pathname-p f)
                          (cache-font-file f)))))))

(defun get-font-families ()
  "Returns cached font families."
  (declare (special *font-cache*))
  (let ((result (list)))
    (maphash (lambda (key value)
               (declare (ignorable value))
               (push key result))
             *font-cache*)
    (nreverse result)))

(defun get-font-subfamilies (font-family)
  "Returns font subfamilies for current @var{font-family}. For e.g. regular, italic, bold, etc."
  (declare (special *font-cache*))
  (let ((result (list)))
    (maphash (lambda (family value)
               (declare (ignorable family))
               (when (string-equal font-family family)
                 (maphash (lambda (subfamily pathname)
                            (declare (ignorable pathname))
                            (push subfamily result)) value)
                 (return-from get-font-subfamilies 
                   (nreverse result)))) *font-cache*)
    (nreverse result)))

(defclass font ()
  ((family :type string :initarg :family :accessor font-family :documentation "Font family.")
   (subfamily :type string :initarg :subfamily :accessor font-subfamily :documentation "Font subfamily. For e.g. regular, italic, bold, bold italib.")
   (size :type real :initarg :size :accessor font-size :initform 12 :documentation "Font size in points.")
   (underline :type boolean :initarg :underline :initform nil :accessor font-underline :documentation "Draw line under text string.")
   (strikethrough :type boolean :initarg :strikethrough :initform nil :accessor font-strikethrough :documentation "Draw strike through text string.")
   (overline :type boolean :initarg :overline :initform nil :accessor font-overline :documentation "Draw line over text string.")
   (background :initarg :background :initform nil :accessor font-background :documentation "Background color.")
   (foreground :initarg :foreground :initform nil :accessor font-foreground :documentation "Foreground color.")
   (overwrite-gcontext :type boolean :initarg :overwrite-gcontext :initform nil 
                       :accessor font-overwrite-gcontext :documentation "Use font values for background and foreground colors.")
   (antialias :type boolean :initarg :antialias :initform t :accessor font-antialias :documentation "Antialias text string.")
   ;; These slots use the OBJ/CACHE protocol
   (string-bboxes :type cache:cache :accessor font-string-bboxes
                  :documentation "Cache for text bboxes")
   (string-line-bboxes :type cache:cache :accessor font-string-line-bboxes
                  :documentation "Cache for text line bboxes")
   (string-alpha-maps :type cache:cache :accessor font-string-alpha-maps
                      :documentation "Cache for text alpha maps")
   (string-line-alpha-maps :type cache:cache :accessor font-string-line-alpha-maps
                           :documentation "Cache for text line alpha maps"))
  (:documentation "Class for representing font information."))

(defun check-valid-font-families (family subfamily)
  (when (or (null (gethash family *font-cache*))
            (null (gethash subfamily (gethash family *font-cache*))))
    (error "Font is not found: ~A ~A" family subfamily)))

(defmethod initialize-instance :before ((instance font) &rest initargs &key family subfamily &allow-other-keys)
  (declare (ignorable initargs))
  (check-valid-font-families family subfamily))

(defmethod (setf font-family) :before (family (instance font))
  (check-valid-font-families family (font-subfamily instance)))

(defmethod (setf font-subfamily) :before (subfamily (instance font))
  (check-valid-font-families (font-family instance) subfamily))

(defmethod (setf font-family) :after (family (font font))
  (cache:cache-flush (font-string-bboxes font))
  (cache:cache-flush (font-string-line-bboxes font)))

(defmethod (setf font-subfamily) :after (subfamily (font font))
  (cache:cache-flush (font-string-bboxes font))
  (cache:cache-flush (font-string-line-bboxes font)))

(defmethod (setf font-size) :after (value (font font))
  (cache:cache-flush (font-string-bboxes font))
  (cache:cache-flush (font-string-line-bboxes font)))

(defmethod (setf font-underline) :after (value (font font))
  (cache:cache-flush (font-string-bboxes font)))

(defmethod (setf font-overline) :after (value (font font))
  (cache:cache-flush (font-string-bboxes font)))

(defgeneric font-equal (font1 font2)
  (:documentation "Returns t if two font objects are equal, else returns nil.")
  (:method ((font1 font) (font2 font))
    (and (string-equal (font-family font1)
                       (font-family font2))
         (string-equal (font-subfamily font1)
                       (font-subfamily font2))
         (= (font-size font1) (font-size font2))
         (eql (font-underline font1) (font-underline font2))
         (eql (font-strikethrough font1) (font-strikethrough font2))
         (eql (font-overline font1) (font-overline font2))
         (equal (font-background font1) (font-background font2))
         (equal (font-foreground font1) (font-foreground font2))
         (eql (font-overwrite-gcontext font1) (font-overwrite-gcontext font2))
         (eql (font-antialias font1) (font-antialias font2)))))

(defmethod equiv:equiv ((a font) (b font)) (font-equal a b))

(defmethod print-object ((instance font) stream)
  "Pretty printing font object"
  (with-slots (family subfamily underline strikethrough
                   overline background foreground overwrite-gcontext
                   antialias)
      instance
    (if *print-readably*
        (format stream
                "#.(~S '~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S)"
                'cl:make-instance 'font
                :family family :subfamily subfamily :underline underline 
                :strikethrough strikethrough
                :overline overline :background background :foreground foreground 
                :overwrite-gcontext overwrite-gcontext
                :antialias antialias)
        (format stream
                "#<'~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S ~S>"
                'font
                :family family :subfamily subfamily :underline underline 
                :strikethrough strikethrough
                :overline overline :background background :foreground foreground 
                :overwrite-gcontext overwrite-gcontext
                :antialias antialias))))

;;; TTF font objects cache
(defun get-font-pathname (font)
  (gethash (font-subfamily font) (gethash (font-family font) *font-cache*)))

(defvar *font-loader-cache* (make-hash-table :test 'equal))

(defmacro with-font ((loader font) &body body)
  (let ((exists-p (gensym))
        (font-path (gensym)))
    `(let ((,font-path (get-font-pathname ,font)))
       (multiple-value-bind (,loader ,exists-p)
           (gethash ,font-path *font-loader-cache*)
         (unless ,exists-p
           (setf ,loader (setf (gethash ,font-path *font-loader-cache*)
                               (open-font-loader ,font-path))))
         ,@body))))
