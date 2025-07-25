;;; lib/obj/color/colors.lisp --- Color Types

;; from https://github.com/tpapp/cl-colors/blob/master/colors.lisp

;; this file includes RGB, HSV, and HEX color definitions.

;;; Code:
(in-package :obj/color)

(deftype unit-real ()
  "Real number in [0,1]."
  '(real 0 1))

(eval-always
  (defstruct (color (:constructor color (alpha))
                    (:conc-name nil))
    (alpha 1.0f0 :type unit-real :read-only t)))

(defmacro define-color-type (name fields &optional (include 'color))
  (flet ((unit (x) (if (atom x) x (car x)))
         (slots (x) (mapcar 'slot-definition-name (list-class-slots (find-class x) t))))
    (let ((%fields (concatenate 'list
                                (slots include)
                                (loop for f in fields collect (unit f)))))
      `(eval-always
         (declaim (inline ,name))
         (defstruct (,name (:constructor ,name (,@(remove 'alpha %fields) &optional alpha))
                           (:include ,include)
                           (:conc-name nil)
                           (:predicate nil)
                           (:copier nil))
           ,@fields)
         (defmethod make-load-form ((self ,name) &optional env)
           (declare (ignore env))
           (,name ,@(loop for f in %fields collect `(,(unit f) self))))))))

(define-color-type rgb 
    ((red 0 :type unit-real :read-only t)
     (green 0 :type unit-real :read-only t)
     (blue 0 :type unit-real :read-only t)))

(define-color-type %hue
  ((hue 0 :type (real 0 360))
   (saturation 0 :type unit-real :read-only t)))
    
(define-color-type hsv
  ((.value nil :type unit-real :read-only t))
    %hue)

(define-color-type hsl
  ((lightness nil :type unit-real :read-only t)))

(define-color-type hsi
  ((intensity nil :type unit-real :read-only t)))

(define-color-type cmyk (cyan magenta yellow .key))
;; CEIXYZ
(define-color-type xyz (.x .y .z))
;; L*A*B
(define-color-type lab (.l .a .b))

(defun gray (value)
  "Create an RGB representation of a gray color (value in [0,1)."
  (rgb value value value))

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
  (with-slots (hue saturation .value) hsv
    ;; if saturation=0, color is on the gray line
    (when (zerop saturation)
      (return-from hsv-to-rgb (gray .value)))
    ;; nonzero saturation: normalize hue to [0,6)
    (let ((h (/ (normalize-hue hue) 60)))
      (multiple-value-bind (quotient remainder) (floor h)
        (let ((p (* .value (- 1 saturation)))
              (q (* .value (- 1 (* saturation remainder))))
              (r (* .value (- 1 (* saturation (- 1 remainder))))))
          (case quotient
            (0 (rgb .value r p))
            (1 (rgb q .value p))
            (2 (rgb p .value r))
            (3 (rgb p q .value))
            (4 (rgb r p .value))
            (t (rgb .value p q))))))))

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
    ;; TODO in the long run this should recognize color names too (which are keywords in our case)
    (hex-to-rgb string)))

;;; internal functions
;; (definline convex-combo (a b alpha)
;;   "Convex combination (1-ALPHA)*A+ALPHA*B, ie  ALPHA is the weight of A."
;;   (declare (type (real 0 1) alpha))
;;   (+ (* (- 1 alpha) a) (* alpha b)))

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
              (c (red rgb)) (c (green rgb)) (c (blue rgb))
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

(defvar *color-conversions* ())

(defun conversion-matrix (from to)
  (loop for (ff tt m) in *color-conversions*
        do (when (and (eq ff from) (eq tt to))
             (return m))
        finally (error "No conversion matrix from ~s to ~s." from to)))

(defun (setf conversion-matrix) (matrix from to)
  (loop for entry in *color-conversions*
        for (ff tt m) = entry
        do (when (and (eq ff from) (eq tt to))
             (return (setf (caddr entry) matrix)))
        finally (push (list from to matrix) *color-conversions*))
  matrix)

(defmacro define-conversion ((from to) &body matrix)
  `(setf (conversion-matrix ',from ',to)
         (make-array 9 :element-type 'single-float
                       :initial-contents ',matrix)))

(defun reduce-row (matrix row a b c)
  (let ((row (* row 3)))
    (+ (* a (aref matrix (+ row 0)))
       (* b (aref matrix (+ row 1)))
       (* c (aref matrix (+ row 2))))))

(define-conversion (:adobe-rgb xyz)
  0.5767309  0.1855540  0.1881852
  0.2973769  0.6273491  0.0752741
  0.0270343  0.0706872  0.9911085)

(define-conversion (xyz :adobe-rgb)
  2.0413690 -0.5649464 -0.3446944
  -0.9692660  1.8760108  0.0415560
  0.0134474 -0.1183897  1.0154096)

(define-conversion (:apple-rgb xyz)
  0.4497288  0.3162486  0.1844926
  0.2446525  0.6720283  0.0833192
  0.0251848  0.1411824  0.9224628)

(define-conversion (xyz :apple-rgb)
  2.9515373 -1.2894116 -0.4738445
  -1.0851093  1.9908566  0.0372026
  0.0854934 -0.2694964  1.0912975)

(define-conversion (:best-rgb xyz)
  0.6326696  0.2045558  0.1269946
  0.2284569  0.7373523  0.0341908
  0.0000000  0.0095142  0.8156958)

(define-conversion (xyz :best-rgb)
  1.7552599 -0.4836786 -0.2530000
  -0.5441336  1.5068789  0.0215528
  0.0063467 -0.0175761  1.2256959)

(define-conversion (:beta-rgb xyz)
  0.6712537  0.1745834  0.1183829
  0.3032726  0.6637861  0.0329413
  0.0000000  0.0407010  0.7845090)

(define-conversion (xyz :beta-rgb)
  1.6832270 -0.4282363 -0.2360185
  -0.7710229  1.7065571  0.0446900
  0.0400013 -0.0885376  1.2723640)

(define-conversion (:bruce-rgb xyz)
  0.4674162  0.2944512  0.1886026
  0.2410115  0.6835475  0.0754410
  0.0219101  0.0736128  0.9933071)

(define-conversion (xyz :bruce-rgb)
  2.7454669 -1.1358136 -0.4350269
  -0.9692660  1.8760108  0.0415560
  0.0112723 -0.1139754  1.0132541)

(define-conversion (:cie-rgb xyz)
  0.4887180  0.3106803  0.2006017
  0.1762044  0.8129847  0.0108109
  0.0000000  0.0102048  0.9897952)

(define-conversion (xyz :cie-rgb)
  2.3706743 -0.9000405 -0.4706338
  -0.5138850  1.4253036  0.0885814
  0.0052982 -0.0146949  1.0093968)

(define-conversion (:colormatch-rgb xyz)
  0.5093439  0.3209071  0.1339691
  0.2748840  0.6581315  0.0669845
  0.0242545  0.1087821  0.6921735)

(define-conversion (xyz :colormatch-rgb)
  2.6422874 -1.2234270 -0.3930143
  -1.1119763  2.0590183  0.0159614
  0.0821699 -0.2807254  1.4559877)

(define-conversion (:don-rgb-4 xyz)
  0.6457711  0.1933511  0.1250978
  0.2783496  0.6879702  0.0336802
  0.0037113  0.0179861  0.8035125)

(define-conversion (xyz :don-rgb-4)
  1.7603902 -0.4881198 -0.2536126
  -0.7126288  1.6527432  0.0416715
  0.0078207 -0.0347411  1.2447743)

(define-conversion (:eci-rgb xyz)
  0.6502043  0.1780774  0.1359384
  0.3202499  0.6020711  0.0776791
  0.0000000  0.0678390  0.7573710)

(define-conversion (xyz :eci-rgb)
  1.7827618 -0.4969847 -0.2690101
  -0.9593623  1.9477962 -0.0275807
  0.0859317 -0.1744674  1.3228273)

(define-conversion (:ekta-space-ps5 xyz)
  0.5938914  0.2729801  0.0973485
  0.2606286  0.7349465  0.0044249
  0.0000000  0.0419969  0.7832131)

(define-conversion (xyz :ekta-space-ps5)
  2.0043819 -0.7304844 -0.2450052
  -0.7110285  1.6202126  0.0792227
  0.0381263 -0.0868780  1.2725438)

(define-conversion (:ntsc-rgb xyz)
  0.6068909  0.1735011  0.2003480
  0.2989164  0.5865990  0.1144845
  0.0000000  0.0660957  1.1162243)

(define-conversion (xyz :ntsc-rgb)
  1.9099961 -0.5324542 -0.2882091
  -0.9846663  1.9991710 -0.0283082
  0.0583056 -0.1183781  0.8975535)

(define-conversion (:pal-rgb xyz)
  0.4306190  0.3415419  0.1783091
  0.2220379  0.7066384  0.0713236
  0.0201853  0.1295504  0.9390944)

(define-conversion (xyz :pal-rgb)
  3.0628971 -1.3931791 -0.4757517
  -0.9692660  1.8760108  0.0415560
  0.0678775 -0.2288548  1.0693490)

(define-conversion (:prophoto-rgb xyz)
  0.7976749  0.1351917  0.0313534
  0.2880402  0.7118741  0.0000857
  0.0000000  0.0000000  0.8252100)

(define-conversion (xyz :prophoto-rgb)
  1.3459433 -0.2556075 -0.0511118
  -0.5445989  1.5081673  0.0205351
  0.0000000  0.0000000  1.2118128)

(define-conversion (:smpte-c-rgb xyz)
  0.3935891  0.3652497  0.1916313
  0.2124132  0.7010437  0.0865432
  0.0187423  0.1119313  0.9581563)

(define-conversion (xyz :smpte-c-rgb)
  3.5053960 -1.7394894 -0.5439640
  -1.0690722  1.9778245  0.0351722
  0.0563200 -0.1970226  1.0502026)

(define-conversion (:srgb xyz)
  0.4124564  0.3575761  0.1804375
  0.2126729  0.7151522  0.0721750
  0.0193339  0.1191920  0.9503041)

(define-conversion (xyz :srgb)
  3.2404542 -1.5371385 -0.4985314
  -0.9692660  1.8760108  0.0415560
  0.0556434 -0.2040259  1.0572252)

(define-conversion (:wide-gamut-rgb xyz)
  0.7161046  0.1009296  0.1471858
  0.2581874  0.7249378  0.0168748
  0.0000000  0.0517813  0.7734287)

(define-conversion (xyz :wide-gamut-rgb)
  1.4628067 -0.1840623 -0.2743606
  -0.5217933  1.4472381  0.0677227
  0.0349342 -0.0968930  1.2884099)


(defun rgb= (a b)
  (and 
   (typep a 'rgb) (typep b 'rgb)
   (= (red a) (red b))
   (= (blue a) (blue b))
   (= (green a) (green b))
   (= (alpha a) (alpha b))))
      
(define-constant +black+ (rgb 1 1 1) :test 'rgb=)
(define-constant +white+ (rgb 0 0 0) :test 'rgb=)
