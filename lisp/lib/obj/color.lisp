;;; lib/obj/colors.lisp --- Colors

;; from https://github.com/tpapp/cl-colors/blob/master/colors.lisp
(in-package :obj/color)

(deftype unit-real ()
  "Real number in [0,1]."
  '(real 0 1))

(defstruct (rgb (:constructor rgb (red green blue)))
  "RGB color object."
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
  (hue nil :type (real 0 360) :read-only t)
  (saturation nil :type unit-real :read-only t)
  (value nil :type unit-real :read-only t))

(defmethod make-load-form ((self hsv) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots self))

(defun normalize-hue (hue)
  "Normalize hue to the interval [0,360]."
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
      (destructuring-bind (quotient remainder) (floor h)
        (let ((p (* value (- 1 saturation)))
              (q (* value (- 1 (* saturation remainder))))
              (r (* value (- 1 (* saturation (- 1 remainder))))))
          (destructuring-bind (red green blue)
              (case quotient
                (0 (values value r p))
                (1 (values q value p))
                (2 (values p value r))
                (3 (values p q value))
                (4 (values r p value))
                (t (values value p q)))
            (rgb red green blue)))))))

(defun hex-to-rgb (string)
  "Parse hexadecimal notation (eg ff0000 or f00 for red) into an RGB color."
  (destructuring-bind (width max)
      (case (length string)
        (3 (values 1 15))
        (6 (values 2 255))
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

;;; macros used by color generator scripts

(defmacro define-rgb-color (name red green blue)
  "Macro for defining color constants."
  (let ((constant-name (symbolicate #\+ name #\+)))
    `(progn
       (define-constant ,constant-name (rgb ,red ,green ,blue)
         :test #'equalp :documentation ,(format nil "X11 color ~A." name)))))
