;;; lib/obj/color/palette.lisp --- Color Palettes

;; A color palette is a hash-table mapping keywords to RGB instances.

;;; Code:
(in-package :obj/color)

;;; Vars
;; place-holder definition incase x11.lisp doesn't exist
(defvar *x11-palette* (make-hash-table))

(defvar *palette* *x11-palette*)

;;; Types
(deftype palette () 'hash-table)

;;; Proto
(defgeneric palette (self)
  (:method ((self null)) *palette*))

(defgeneric (setf palette) (new self)
  (:method (new (self null)) (setf *palette* new)))

;;; Utils
(defun parse-x11-palette (&key (name '*x11-palette*)
                               (input #.(asdf:system-relative-pathname :core ".stash/rgb.txt"))
                               (output #.(asdf:system-relative-pathname :core "lib/obj/color/x11.lisp")))
  "Parse X11 color definitions and write them into a file. Return the
list of colors.

Note that the input file we expect called rgb.txt is no longer
distributed with X11 by default (AFAIK). You should be able to find it
with a quick google search."
  (let ((color-scanner ;will only take names w/o spaces
          (cl-ppcre:create-scanner
           "^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+([\\s\\w]+\?)\\s*$"
           :extended-mode t))
        (comment-scanner (cl-ppcre:create-scanner "^\\s*!"))
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
(in-package :obj/color)"
                output input)
        (format colordefs "~2%")
        (labels ((parse-channel (string)
                   (let ((i (read-from-string string)))
                     (assert (and (typep i 'integer) (<= i 255)))
                     (/ i 255))))
          (do ((line (read-line source nil nil) (read-line source nil nil)))
              ((not line))
            (unless (cl-ppcre:scan-to-strings comment-scanner line)
              (multiple-value-bind (match registers)
                  (cl-ppcre:scan-to-strings color-scanner line)
                ;; we don't ingest color names with spaces because they are
                ;; duplicates - 'dark goldenrod' has the same value as
                ;; 'darkgoldenrod' so just use that.
                (when (and match (not (find #\space (aref registers 3))))
                  (let ((colorname (string-downcase (aref registers 3))))
                    (format colordefs
                            "(setf (gethash :~A ~A) (rgb ~A ~A ~A))~%"
                            colorname
                            (string-downcase name)
                            (parse-channel (aref registers 0))
                            (parse-channel (aref registers 1))
                            (parse-channel (aref registers 2)))
                    (push colorname colornames))))))))
      (nreverse colornames))))
