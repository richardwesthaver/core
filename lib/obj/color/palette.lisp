;;; lib/obj/color/palette.lisp --- Color Palettes

;; A color palette is a hash-table mapping keywords to RGB instances.

;;; Code:
(in-package :obj/color)

(define-condition missing-palette (color-error invalid-item) ()
  (:report
   (lambda (condition stream)
     (format stream "No palette available for ~A"
             (error-item condition)))))

;;; Vars
(defvar *color-palettes* (make-hash-table))
(defvar *palette* nil)

;;; Types
(deftype palette () 'hash-table)

(deftype base-color-key ()
  '(member
    :base00 :base01 :base02 :base03 :base04 :base05 :base06 :base07 :base08 :base09
    :base0A :base0B :base0C :base0D :base0E :base0F))

;;; Proto
(defgeneric palette (self)
  (:documentation "Return the palette associated with SELF, defaults to *PALETTE*.")
  (:method (self) (if self (palette self) *palette*)))

(defgeneric (setf palette) (new self)
  (:documentation "Set the palette associated with SELF, defaults to *PALETTE*.")
  (:method (new (self hash-table)) (setf self new)))

(defgeneric get-color (key &optional self)
  (:documentation "Get the color associated with KEY in SELF which defaults to *PALETTE*.")
  (:method (key &optional (self *palette*))
    (gethash key self)))
(defgeneric (setf get-color) (new key &optional self)
  (:documentation "Set the color associated with KEY in SELF to NEW. SELF defaults to *PALETTE*.")
  (:method (new key &optional (self *palette*))
    (setf (gethash key self) new)))

;;; Utils
(definline base-color-palette-p (palette)
  "Return T if all keys of PALETTE are of type base-color-key."
  (every (lambda (x) (typep x 'base-color-key)) (hash-table-keys palette)))

(defun make-palette (name &rest colors &aux (tbl (make-hash-table)))
  (let ((*palette* tbl))
    (doplist (k v) colors
      (setf (get-color k)
            (etypecase v
              (color v)
              (string (parse-hex-rgb v))
              (symbol (get-color v)))))
    (setf (gethash name *color-palettes*) *palette*)
    *palette*))

(defun remove-palette (name)
  "Remove a palette by NAME from *COLOR-PALETTES*."
  (remhash name *color-palettes*))

(definline find-palette (name)
  (gethash name *color-palettes*))

(definline load-palette (name)
  (when-let ((p (find-palette name)))
    (setf *palette* p)))

(defwith palette (name) (*palette* (find-palette name)))

(defun parse-x11-palette (&key
                          (input #.(stash-pathname "rgb.txt"))
                          (output #.(system-relative-pathname :core "lib/obj/color/x11.lisp")))
  "Parse X11 color definitions and write them into a file. Return the
list of colors.

Note that the input file we expect called rgb.txt is no longer
distributed with X11 by default (AFAIK). You should be able to find it
with a quick google search."
  (let ((color-scanner ;will only take names w/o spaces
          (ppcre:create-scanner
           "^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+([\\s\\w]+\?)\\s*$"
           :extended-mode t))
        (comment-scanner (ppcre:create-scanner "^\\s*!"))
        colornames)
    (with-open-file (source input
                       :direction :input
                       :if-does-not-exist :error)
      (with-open-file (colordefs output
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)
        (format colordefs ";;; ~a --- X11 Colors -*- buffer-read-only:t -*-

;; input = ~a

;; This file was generated automatically by
;; OBJ/COLOR:PARSE-X11-PALETTE.

;; Do not modify.

;;; Code:
(in-package :obj/color)
(make-palette :x11"
                output input)
        (labels ((parse-channel (string)
                   (let ((i (read-from-string string)))
                     (assert (and (typep i 'integer) (<= i 255)))
                     (/ i 255))))
          (do ((line (read-line source nil nil) (read-line source nil nil)))
              ((not line))
            (unless (ppcre:scan-to-strings comment-scanner line)
              (multiple-value-bind (match registers)
                  (ppcre:scan-to-strings color-scanner line)
                ;; we don't ingest color names with spaces because they are
                ;; duplicates - 'dark goldenrod' has the same value as
                ;; 'darkgoldenrod' so just use that.
                (when (and match (not (find #\space (aref registers 3))))
                  (let ((colorname (string-downcase (aref registers 3))))
                    (format colordefs
                            "~% :~A (rgb ~A ~A ~A)"
                            colorname
                            (parse-channel (aref registers 0))
                            (parse-channel (aref registers 1))
                            (parse-channel (aref registers 2)))
                    (push colorname colornames))))))
          (write-char #\) colordefs)))
      (nreverse colornames))))
