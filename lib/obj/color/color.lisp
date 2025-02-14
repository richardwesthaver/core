;;; lib/obj/color/colors.lisp --- Color Types

;; from https://github.com/tpapp/cl-colors/blob/master/colors.lisp

;; this file includes RGB, HSV, and HEX color definitions.

;;; Code:
(in-package :obj/color)

(deftype unit-real ()
  "Real number in [0,1]."
  '(real 0 1))

(defstruct (rgb (:constructor rgb (red green blue)))
  (red nil :type unit-real :read-only t)
  (green nil :type unit-real :read-only t)
  (blue nil :type unit-real :read-only t))

(defmethod make-load-form ((self rgb) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots self))

(defun gray (value)
  "Create an RGB representation of a gray color (value in [0,1)."
  (rgb value value value))

(defstruct (hsv (:constructor hsv (hue saturation value)))
  "HSV color object."
  (hue nil :type (real 0 360))
  (saturation nil :type unit-real :read-only t)
  (value nil :type unit-real :read-only t))

(defmethod make-load-form ((self hsv) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots self))

(defun normalize-hue (hue)
  "Normalize hue to the interval [0,360)."
  (mod hue 360))

;;; conversions
(defun rgb-to-hsv (rgb &optional (undefined-hue 0))
  "Convert RGB to HSV representation.  When hue is undefined (saturation is
zero), UNDEFINED-HUE will be assigned."
  (with-slots (red green blue) rgb
    (let* ((value (max red green blue))
           (delta (- value (min red green blue)))
           (saturation (if (plusp value)
                           (/ delta value)
                           0)))
      (flet ((normalize (constant right left)
               (let ((hue (+ constant (/ (* 60 (- right left)) delta))))
                 (if (minusp hue)
                     (+ hue 360)
                     hue))))
        (hsv (cond
               ((zerop saturation) undefined-hue) ; undefined
               ((= red value) (normalize 0 green blue)) ; dominant red
               ((= green value) (normalize 120 blue red)) ; dominant green
               (t (normalize 240 red green)))
             saturation
             value)))))

(defun hsv-to-rgb (hsv)
  "Convert HSV to RGB representation.  When SATURATION is zero, HUE is
ignored."
  (with-slots (hue saturation value) hsv
    ;; if saturation=0, color is on the gray line
    (when (zerop saturation)
      (return-from hsv-to-rgb (gray value)))
    ;; nonzero saturation: normalize hue to [0,6)
    (let ((h (/ (normalize-hue hue) 60)))
      (multiple-value-bind (quotient remainder) (floor h)
        (let ((p (* value (- 1 saturation)))
              (q (* value (- 1 (* saturation remainder))))
              (r (* value (- 1 (* saturation (- 1 remainder))))))
          (case quotient
            (0 (rgb value r p))
            (1 (rgb q value p))
            (2 (rgb p value r))
            (3 (rgb p q value))
            (4 (rgb r p value))
            (t (rgb value p q))))))))

(defun hex-to-rgb (string)
  "Parse hexadecimal notation (eg ff0000 or f00 for red) into an RGB color."
  (destructuring-bind (width max)
      (case (length string)
        (3 (list 1 15))
        (6 (list 2 255))
        (t (error "string ~A doesn't have length 3 or 6, can't parse as ~
                       RGB specification" string)))
    (flet ((parse (index)
             (/ (parse-integer string :start (* index width)
                                      :end (* (1+ index) width)
                                      :radix 16)
                max)))
      (rgb (parse 0) (parse 1) (parse 2)))))

;;; conversion with generic functions
(defgeneric as-hsv (color &optional undefined-hue)
  (:method ((color rgb) &optional (undefined-hue 0))
    (rgb-to-hsv color undefined-hue))
  (:method ((color hsv) &optional undefined-hue)
    (declare (ignore undefined-hue))
    color))

(defgeneric as-rgb (color)
  (:method ((rgb rgb))
    rgb)
  (:method ((hsv hsv))
    (hsv-to-rgb hsv))
  (:method ((string string))
    ;; TODO in the long run this should recognize color names too
    (hex-to-rgb string)))

;;; internal functions
(declaim (inline cc))
(defun cc (a b alpha)
  "Convex combination (1-ALPHA)*A+ALPHA*B, ie  ALPHA is the weight of A."
  (declare (type (real 0 1) alpha))
  (+ (* (- 1 alpha) a) (* alpha b)))

;;; parsing and printing of CSS-like colors
(defun print-hex-rgb (color &key short (hash T) alpha destination)
  "Converts a COLOR to its hexadecimal RGB string representation.  If
SHORT is specified each component gets just one character.

A hash character (#) is prepended if HASH is true (default).

If ALPHA is set it is included as an ALPHA component.

DESTINATION is the first argument to FORMAT, by default NIL."
  (let ((rgb (as-rgb color))
        (factor (if short 15 255)))
    (flet ((c (x) (round (* x factor))))
      (format destination (if short
                              "~@[~C~]~X~X~X~@[~X~]"
                              "~@[~C~]~2,'0X~2,'0X~2,'0X~@[~X~]")
              (and hash #\#)
              (c (rgb-red rgb)) (c (rgb-green rgb)) (c (rgb-blue rgb))
              (and alpha (c alpha))))))

;; TODO: a JUNK-ALLOWED parameter, like for PARSE-INTEGER, would be nice
(defun parse-hex-rgb (string &key (start 0) end)
  "Parses a hexadecimal RGB(A) color string.  Returns a new RGB color value
and an alpha component if present."
  (let* ((length (length string))
         (end (or end length))
         (sub-length (- end start)))
    (cond
      ;; check for valid range, we need at least three and accept at most
      ;; nine characters
      ((and (<= #.(length "fff") sub-length)
            (<= sub-length #.(length "#ffffff00")))
       (when (char= (char string start) #\#)
         (incf start)
         (decf sub-length))
       (labels ((parse (string index offset)
                  (parse-integer string :start index :end (+ offset index)
                                        :radix 16))
                (short (string index)
                  (/ (parse string index 1) 15))
                (long (string index)
                  (/ (parse string index 2) 255)))
         ;; recognize possible combinations of alpha component and length
         ;; of the rest of the encoded color
         (multiple-value-bind (shortp alphap)
             (case sub-length
               (#.(length "fff") (values T NIL))
               (#.(length "fff0") (values T T))
               (#.(length "ffffff") (values NIL NIL))
               (#.(length "ffffff00") (values NIL T)))
           (if shortp
               (values
                (rgb
                 (short string start)
                 (short string (+ 1 start))
                 (short string (+ 2 start)))
                (and alphap (short string (+ 3 start))))
               (values
                (rgb
                 (long string start)
                 (long string (+ 2 start))
                 (long string (+ 4 start)))
                (and alphap (long string (+ 6 start))))))))
      (t
       (error "not enough or too many characters in indicated sequence: ~A"
              (subseq string start end))))))
