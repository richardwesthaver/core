;;; lib/obj/color/util.lisp --- Color utilities

;; This file provides utilities for working with colors defined in
;; external systems and grouping them into color-palettes.

;;; Code:
(in-package :obj/color)

(eval-always
  (defparameter *rgb-colors-path*
    (probe-file #.(asdf:system-relative-pathname :prelude #P"../.stash/rgb.txt")))
  (defvar *x11-colors* nil))

;;; macros used by color generator scripts
(defmacro define-rgb-color (name red green blue)
  "Macro for defining color constants."
  (let ((constant-name (symbolicate #\+ name #\+)))
    `(prog1
         (define-constant ,constant-name (rgb ,red ,green ,blue)
           :test #'equalp :documentation ,(format nil "X11 color ~A." name))
       (pushnew ',constant-name *x11-colors*))))

(defun parse-x11-color-definitions (&key
                                    (input *rgb-colors-path*)
                                    (output "lisp/lib/obj/color/x11.lisp"))
  "Parse X11 color definitions and write them into a file. Return the
list of colors (for exporting).

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
;; OBJ/COLOR:PARSE-X11-COLOR-DEFINITIONS.

;; Do not modify.

;;; Code:
(in-package :obj/color)" output input)
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
                (if (and match (not (find #\space (aref registers 3))))
                    (let ((colorname (string-downcase (aref registers 3))))
                      (format colordefs
                              "(export (define-rgb-color ~A ~A ~A ~A))~%"
                              colorname
                              (parse-channel (aref registers 0))
                              (parse-channel (aref registers 1))
                              (parse-channel (aref registers 2)))
                      (push colorname colornames))
                    (log:trace! (format nil "ignoring line ~A~%" line))))))))
      (nreverse colornames))))
