;;; dat/xml/svg.lisp --- SVG data encoders

;; based on https://github.com/orthecreedence/cl-svg-polygon

;;; Code:
(in-package :dat/svg)

;;; MATRIX
(defun id-matrix (dims)
  "Return a square identity matrix with the specified "
  (let ((array (make-array (* dims dims) :initial-element 0.0 :element-type 'single-float)))
    (dotimes (d dims)
      (setf (aref array (* d (1+ dims))) 1.0))
    array))

(defun mat* (m1 m2)
  "Multiply 3x3 matrices m1 by m2."
  (let ((new (make-array 9 :initial-element 0.0 :element-type 'single-float)))
    (dotimes (x 3)
      (dotimes (y 3)
        (let ((prod (+ (* (aref m1 (* x 3)) (aref m2 y))
                       (* (aref m1 (+ (* x 3) 1)) (aref m2 (+ y 3)))
                       (* (aref m1 (+ (* x 3) 2)) (aref m2 (+ y 6))))))
          (setf (aref new (+ y (* x 3))) (coerce prod 'single-float)))))
    new))

(defun matv* (m v)
  "Multiple a matrix by a vector, return the resulting vector."
  (let ((new (make-list 3))
        (vx (car v))
        (vy (cadr v))
        (vz 1))
    (dotimes (i 3)
      (setf (nth i new) (+ (* vx (aref m (* i 3)))
                           (* vy (aref m (+ (* i 3) 1)))
                           (* vz (aref m (+ (* i 3) 2))))))
    new))

(defun m-rotate (degrees &key reverse)
  "Generate a rotation matrix."
  (let* ((matrix (id-matrix 3))
         (angle-rad (* (mod degrees 360) (/ PI 180)))
         (cos (coerce (cos angle-rad) 'single-float))
         (sin (coerce (sin angle-rad) 'single-float)))
    (setf (aref matrix 0) cos
          (aref matrix 1) (if reverse sin (- sin))
          (aref matrix 3) (if reverse (- sin) sin)
          (aref matrix 4) cos)
    matrix))

(defun m-scale (x y)
  "Generate a scaling matrix."
  (let ((matrix (id-matrix 3)))
    (setf (aref matrix 0)  (coerce x 'single-float)
          (aref matrix 4)  (coerce y 'single-float))
    matrix))
  
(defun m-translate (x y)
  "Generate a translation matrix."
  (let ((translatrix (id-matrix 3)))
    (setf (aref translatrix 2) (coerce x 'single-float)
          (aref translatrix 5) (coerce y 'single-float))
    translatrix))

(defun m-skew (degrees &key (axis :x))
  "Generate a skew matrix along the :axis axis (:x or :y)."
  (let ((matrix (id-matrix 3))
        (angle-rad (* (mod degrees 360) (/ PI 180)))
        (idx (if (equal axis :x) 1 3)))
    (setf (aref matrix idx) (coerce (tan angle-rad) 'single-float))
    matrix))

;;; VECTOR
(defun norm (v)
  "Calculate a vector norm."
  (expt (loop for x in v sum (expt x 2)) .5))

(defun normalize (v)
  "Normalize a 2D vector"
  (let ((x (car v))
        (y (cadr v)))
    (let ((norm (norm v)))
      (list (/ x norm) (/ y norm)))))

(defun dot-prod (v1 v2)
  "Give the dot product of two 2D vectors."
  (+ (* (car v1) (car v2))
     (* (cadr v1) (cadr v2))))

;;; TRANSFORMATIONS
(defun parse-transform (transform)
  "Turn a transform(...) into an easily-parsable list structure."
  ;; convert "translate(-10,-20) scale(2) rotate(45) translate(5,10)" into
  ;; "(translate -10 -20) (scale 2) (rotate 45) (translate 5 10)"
  ;; (ie read-from-string'able)
  (let* ((transform (cl-ppcre::regex-replace-all "([a-z]+)\\(" transform "(\\1 "))
         (transform (cl-ppcre::regex-replace-all "," transform " ")))
    (read-from-string (format nil "( ~a )" transform))))

(defun get-transformations (object groups)
  "Given an SVG object and a tree of groups, grab all transformations, starting
   from the top down, into a flat list so they can be applied sequentially."
  (let ((object-transform (getf object :transform))
        (object-group (getf object :group))
        (transformations nil))
    (labels ((traverse-groups (path groups)
               (dolist (group groups)
                 (when (eql (car (getf group :group)) (car path))
                   (let* ((groups (getf group :groups))
                          (transform (getf group :transform))
                          (transform (if (listp transform) (car transform) transform)))
                     (when transform
                       (push transform transformations))
                     (when groups
                       (traverse-groups (cdr path) groups)))))))
      (traverse-groups object-group groups))
    (when object-transform
      (push object-transform transformations))
    transformations))

(defun get-matrix-from-transformation (transformation)
  "Given a transformation in list form (FN ARG1 ARG2 ...), turn it into a matrix
  which can be multipled to give the overall transformation for an object."
  (macrolet ((idx (var idx default)
               (let ((name (gensym)))
                 `(let ((,name (nth ,idx ,var)))
                    (if ,name ,name ,default)))))
    (let ((transformation (if (listp (car transformation))
                              (car transformation)
                              transformation)))
      (case (intern (write-to-string (car transformation)) :dat/svg)
        (matrix (vector (nth 1 transformation) (nth 3 transformation) (nth 5 transformation)
                        (nth 2 transformation) (nth 4 transformation) (nth 6 transformation)
                        0 0 1))
        (translate (m-translate (nth 1 transformation) (idx transformation 2 0)))
        (scale (m-scale (nth 1 transformation) (idx transformation 2 0)))
        (rotate (let ((angle (nth 1 transformation))
                      (center-x (idx transformation 2 0))
                      (center-y (idx transformation 3 0)))
                  (if (and (eq 0 center-x) (eq 0 center-y))
                      ;; just rotate, no offset funny business
                      (m-rotate angle)
                      (mat* (mat* (m-translate center-x center-y) (m-rotate angle)) (m-translate (- center-x) (- center-y))))))
        (skewx (m-skew (nth 1 transformation) :axis :x))
        (skewy (m-skew (nth 1 transformation) :axis :y))))))

(defun apply-transformations (points object groups &key scale)
  "Apply all transformations for an object, starting from its top-level group
  and working down to the object itself."
  (let ((transformations (get-transformations object groups))
        (matrix (id-matrix 3))
        (trans-points nil))
    (dolist (transform transformations)
      (setf matrix (mat* (get-matrix-from-transformation transform) matrix)))
    (when scale
      (setf matrix (mat* (m-scale (car scale) (cadr scale)) matrix)))
    (loop for p across points do
      (push (butlast (matv* matrix (append p '(1)))) trans-points))
    (values (reverse trans-points)
            matrix)))
;;; PATHS
(define-condition unsupported-path-command (error)
  ((text :initarg :text :reader text))
  (:documentation "Thrown when an unsupported action/feature is parsed in a path."))

(defun points-close-equal-p (point1 point2 &key (precision 10))
  "Determine if two points are (about) the same. Yes, this is open to
   interpretation, which is why it takes a precision argument =]."
  (flet ((round-point (point)
           (mapcar (lambda (x) (/ (floor (* x precision)) precision)) point)))
    (equal (round-point point1) (round-point point2))))

(defun replace-char (char rep str)
  "Replace all instances of char with rep in str (non-destructive)."
  (let ((new-str (make-string (length str))))
    (loop for i from 0
          for c across str do
      (setf (aref new-str i) (if (eq c char)
                                 rep
                                 c)))
    new-str))

(defmacro cmd-repeat (args-and-count &body body)
  "Some commands can repeat values with the command, namely the curve commands:
       c,1,2,4,4,5,5 c,8,8,3,4,3,1
    can be written as
       c,1,2,4,4,5,5,8,8,3,4,3,1
  yay. This macro helps alleviate some of the issues caused by this wonderful
  feature in the get-points-from-path function."
  (let ((i (gensym))
        (a (gensym))
        (args (car args-and-count))
        (count (cadr args-and-count)))
    `(dotimes (,i (floor (/ (length ,args) ,count)))
       ,@body
       (setf cur-x (car cur-point)
             cur-y (cadr cur-point))
       (dotimes (,a ,count)
         (setf ,args (cdr ,args))))))
(defun get-points-from-path (str-data &key (curve-resolution 10))
  "Given a string describing an SVG path, do our best to retrieve points along
  that path. Bezier curves are approximated as accurately as needed (defined by
  :curve-resolution).

  If the path generates an arc between x1,y1 and x2,y2, we just ignore the whole
  arc thing and set x2,y2 as the next point in the path.

  If Z/z ends the path in the middle, we silently return the current set of 
  points without continuing the path. The idea here is we are generating
  polygons so breaks or cutouts are not acceptable."
    (let ((commands (print (split "(?=[a-zA-Z])" str-data)))
          (scanner-empty-p (cl-ppcre:create-scanner (concatenate 'string "[" *whitespaces* "]") :multi-line-mode t))
          (points nil)
        (parts nil)
        (first-point nil)
        (cur-point '(0 0))
        (last-anchor nil)
        (disconnected nil))
    (dolist (cmd-str commands)
      ;; this (let) splits the command from "M-113-20" to
      ;; ("M" "-113" "-20")
      (let* ((cmd-parts (cl-ppcre:split "( |,|(?<=[A-Za-z])|(?=\-))" cmd-str))
             (cmd (aref (car cmd-parts) 0))
             ;(forget (format t "cmd: ~s~%" cmd-parts))
             (args (remove-if #'null (mapcar (lambda (a)
                                               (if (cl-ppcre:scan scanner-empty-p a)
                                                   nil
                                                   (read-from-string a)))
                                             (cdr cmd-parts))))
             (cur-x (car cur-point))
             (cur-y (cadr cur-point)))
        ;; process the commands (http://www.w3.org/TR/SVG/paths.html)
        (case (if (eq cmd #\z)
                  (aref (string-upcase cmd) 0)
                  cmd)
          (#\M
           (cmd-repeat (args 2)
             (setf cur-point args)
             (push cur-point points)))
          (#\m
           (cmd-repeat (args 2)
             (setf cur-point (list (+ cur-x (car args))
                                   (+ cur-y (cadr args))))
             (push cur-point points)))
          (#\L
           (cmd-repeat (args 2)
             (setf cur-point args)
             (push cur-point points)))
          (#\l
           (cmd-repeat (args 2)
             (setf cur-point (list (+ cur-x (car args))
                                   (+ cur-y (cadr args))))
             (push cur-point points)))
          (#\H
           (cmd-repeat (args 1)
             (setf (car cur-point) (car args))
             (push cur-point points)))
          (#\h
           (cmd-repeat (args 1)
             (setf (car cur-point) (+ cur-x (car args)))
             (push cur-point points)))
          (#\V
           (cmd-repeat (args 1)
             (setf (cadr cur-point) (car args))
             (push cur-point points)))
          (#\v
           (cmd-repeat (args 1)
             (setf (cadr cur-point) (+ cur-y (car args)))
             (push cur-point points)))
          (#\C
           (cmd-repeat (args 6)
             (let ((x1 (car args))
                   (y1 (cadr args))
                   (x2 (nth 2 args))
                   (y2 (nth 3 args))
                   (x (nth 4 args))
                   (y (nth 5 args)))
               (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                     last-anchor (list x2 y2)
                     cur-point (list x y)))))
          (#\c
           (cmd-repeat (args 6)
             (let ((x1 (+ (car args) cur-x))
                   (y1 (+ (cadr args) cur-y))
                   (x2 (+ (nth 2 args) cur-x))
                   (y2 (+ (nth 3 args) cur-y))
                   (x (+ (nth 4 args) cur-x))
                   (y (+ (nth 5 args) cur-y)))
               (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                     last-anchor (list x2 y2)
                     cur-point (list x y)))))
          (#\S
           (cmd-repeat (args 4)
             (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                   (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                   (x2 (car args))
                   (y2 (cadr args))
                   (x (nth 2 args))
                   (y (nth 3 args)))
               (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                     last-anchor (list x2 y2)
                     cur-point (list x y)))))
          (#\s
           (cmd-repeat (args 4)
             (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                   (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                   (x2 (+ (car args) cur-x))
                   (y2 (+ (cadr args) cur-y))
                   (x (+ (nth 2 args) cur-x))
                   (y (+ (nth 3 args) cur-y)))
               (setf points (append (bezier-cubic cur-x cur-y x y x1 y1 x2 y2 :resolution curve-resolution) points)
                     last-anchor (list x2 y2)
                     cur-point (list x y)))))
          (#\Q
           (cmd-repeat (args 4)
             (let ((x1 (car args))
                   (y1 (cadr args))
                   (x (nth 2 args))
                   (y (nth 3 args)))
               (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                     last-anchor (list x1 y1)
                     cur-point (list x y)))))
          (#\q
           (cmd-repeat (args 4)
             (let ((x1 (+ (car args) cur-x))
                   (y1 (+ (cadr args) cur-y))
                   (x (+ (nth 2 args) cur-x))
                   (y (+ (nth 3 args) cur-y)))
               (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                     last-anchor (list x1 y1)
                     cur-point (list x y)))))
          (#\T
           (cmd-repeat (args 2)
             (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                   (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                   (x (car args))
                   (y (cadr args)))
               (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                     last-anchor (list x1 y1)
                     cur-point (list x y)))))
          (#\t
           (cmd-repeat (args 2)
             (let ((x1 (+ cur-x (- cur-x (car last-anchor))))
                   (y1 (+ cur-y (- cur-y (cadr last-anchor))))
                   (x (+ (car args) cur-x))
                   (y (+ (cadr args) cur-y)))
               (setf points (append (bezier-quadratic cur-x cur-y x y x1 y1 :resolution curve-resolution) points)
                     last-anchor (list x1 y1)
                     cur-point (list x y)))))
          (#\A
           (cmd-repeat (args 7)
             (let ((rx (car args))
                   (ry (cadr args))
                   (x-rot (caddr args))
                   (large-arc (cadddr args))
                   (sweep-flag (cadr (cdddr args)))
                   (x1 (car cur-point))
                   (y1 (cadr cur-point))
                   (x2 (+ (caddr (cdddr args)) (car cur-point)))
                   (y2 (+ (cadddr (cdddr args)) (cadr cur-point))))
               (setf points (append (elliptical-arc x1 y1 x2 y2 rx ry x-rot large-arc sweep-flag :resolution curve-resolution) points)
                     cur-point (list x2 y2)))))
          (#\a
           (cmd-repeat (args 7)
             (let ((rx (car args))
                   (ry (cadr args))
                   (x-rot (caddr args))
                   (large-arc (cadddr args))
                   (sweep-flag (cadr (cdddr args)))
                   (x1 (car cur-point))
                   (y1 (cadr cur-point))
                   (x2 (+ (caddr (cdddr args)) (car cur-point)))
                   (y2 (+ (cadddr (cdddr args)) (cadr cur-point))))
               (setf points (append (elliptical-arc x1 y1 x2 y2 rx ry x-rot large-arc sweep-flag :resolution curve-resolution) points)
                     cur-point (list x2 y2)))))
          (#\Z
           (push (coerce (reverse (if (points-close-equal-p (car points) first-point)
                                      (cdr points)
                                      points)) 'vector) parts)
           (setf points nil))))
      (when (= (length points) 1)
        (setf first-point (car points))))
    (when (not (zerop (length points)))
      ;; we have unfinished points. add them to the part list
      (setf disconnected t)
      (push (coerce (reverse (if (points-close-equal-p (car points) first-point)
                                 (cdr points)
                                 points)) 'vector) parts))
    (values (reverse parts) disconnected)))

(defun bezier-cubic (x1 y1 x2 y2 ax1 ay1 ax2 ay2 &key (resolution 10))
  "Sample resolution points off of a cubic bezier curve from (x1,y1) to (x2,y2)
  using anchor points (ax1,ay1) (ax2,ay2)."
  (let ((points nil))
    (flet ((cubic (t-val p0 p1 p2 p3)
             (+ (* (expt (- 1 t-val) 3) p0)
                (* 3 (expt (- 1 t-val) 2) t-val p1)
                (* 3 (- 1 t-val) (expt t-val 2) p2)
                (* (expt t-val 3) p3))))
      (dotimes (i resolution)
        (let ((t-val (* (1+ i) (/ 1 resolution))))
          (push (list (cubic t-val x1 ax1 ax2 x2)
                      (cubic t-val y1 ay1 ay2 y2))
                points))))
    points))

(defun bezier-quadratic (x1 y1 x2 y2 ax1 ay1 &key (resolution 10))
  "Sample resolution points off of a quadratic bezier curve from (x1,y1) to
  (x2,y2) using anchor points (ax1,ay1) (ax2,ay2)."
  (let ((points nil))
    (flet ((quadratic (t-val p0 p1 p2)
             (+ (* (expt (- 1 t-val) 2) p0)
                (* 2 (- 1 t-val) t-val p1)
                (* (expt t-val 2) p2))))
      (dotimes (i resolution)
        (let ((t-val (* (1+ i) (/ 1 resolution))))
          (push (list (quadratic t-val x1 ax1 x2)
                      (quadratic t-val y1 ay1 y2)) points))))
    points))

(defun elliptical-arc (x1 y1 x2 y2 rx ry x-rotation large-arc-flag sweep-flag &key (resolution 10))
  "Calculate an arc in a path. Yuck."
  (let ((rot-mat-i (m-rotate x-rotation :reverse t))
        (rot-mat (m-rotate x-rotation)))
    ;; calculate a bunch of crap, mainly ellipse center x,y
    (let* ((xy-i (matv* rot-mat-i (list (/ (- x1 x2) 2)
                                        (/ (- y1 y2) 2))))
           (x-i (car xy-i))
           (y-i (cadr xy-i))
           (rx2 (expt rx 2))
           (ry2 (expt ry 2))
           (x-i2 (expt x-i 2))
           (y-i2 (expt y-i 2))
           (cxy-m (expt (/ (- (* rx2 ry2) (* rx2 y-i2) (* ry2 x-i2))
                           (+ (* rx2 y-i2) (* rx2 x-i2)))
                        .5))
           (cxy-m (if (eq large-arc-flag sweep-flag)
                      (- cxy-m)
                      cxy-m))
           (cx-i (* cxy-m (/ (* rx y-i) ry)))
           (cy-i (* cxy-m (/ (* ry x-i) (- rx))))
           (cxy (matv* rot-mat (list cx-i cy-i)))
           (cx (+ (car cxy) (/ (+ x1 x2) 2)))
           (cy (+ (cadr cxy) (/ (+ y1 y2) 2))))
      (flet ((angle (v1 v2)
               (let ((x1 (car v1))
                     (y1 (cadr v1))
                     (x2 (car v2))
                     (y2 (cadr v2)))
                 (let ((sign (if (< 0 (- (* x1 y2) (* y1 x2)))
                                 1
                                 -1)))
                   (* sign (acos (/ (dot-prod v1 v2)
                                    (* (norm v1) (norm v2)))))))))
        ;; calculate the start/delta angles
        (let ((theta-1 (angle (list 1 0) (list (/ (- x-i cx-i) rx)
                                               (/ (- y-i cy-i) ry))))
              (theta-delta (angle (list (/ (- x-i cx-i) rx)
                                        (/ (- y-i cy-i) ry))
                                  (list (/ (- (- x-i) cx-i) rx)
                                        (/ (- (- y-i) cy-i) ry)))))
          (let ((theta-step (/ theta-delta resolution))
                (points nil))
            ;; create our points for the ellipse. if this were a true
            ;; implementation, we'd do radii correction such that x2,y2 always
            ;; fall ON the ellipse path, but i truly do not care enough to
            ;; bother. if your SVG generator sucks, take it up with them, or
            ;; better yet do the proper calculations and issue a pull request.
            (dotimes (i resolution)
              (let ((angle (+ theta-1 (* theta-step i))))
                (let ((xy (matv* rot-mat (list (* rx (cos angle))
                                               (* ry (sin angle))))))
                  (push (list (+ (car xy) cx)
                              (+ (cadr xy) cy)) points))))
            ;; get the last point on there.
            (push (list x2 y2) points)
            (reverse points)))))))

;;; SVG
(define-condition not-an-object (simple-condition) ())

(defun get-points-from-ellipse (x y rx ry &key (curve-resolution 20))
  "Calculate curve-resolution points along an ellipse. Can be used for circles
  too (when rx == ry)."
  (let ((points (make-array curve-resolution)))
    (dotimes (i curve-resolution)
      (let ((rad (* i (/ (* 2 PI) curve-resolution))))
        (setf (aref points i)
              (list (coerce (+ x (* (cos rad) rx)) 'single-float)
                    (coerce (+ y (* (sin rad) ry)) 'single-float)))))
    points))

(defmacro with-plist-string-reads (plist bindings &body body)
  "Helper macro to make convert-to-points much more readable. Basically wraps
  around reading values from a string in a plist and binding the result to a
  variable:
  
    (with-plist-string-reads my-plist ((x :x) (y :y))
      (+ x y))
  
  Expands to:

    (let ((x (read-from-string (getf my-plist :x)))
          (y (read-from-string (getf my-plist :y))))
      (+ x y))

  Much cleaner."
  `(let ,(loop for binding in bindings collect
          (list (car binding) `(read-from-string (getf ,plist ,(cadr binding)))))
     ,@body))

(defun convert-to-points (obj &key (curve-resolution 10))
  "Take an object loaded from and SVG file (most likely using parse-svg-nodes)
  and turn it into a set of points describing a polygon. Curves are
  approximated using :curve-resolution. The higher the resolution, the more
  accurate the curve will be. This works for paths with bezier curves as well
  as ellipses and circles."
  (case (intern (string-upcase (getf obj :type)) :dat/svg)
    (rect
      (with-plist-string-reads obj ((x :x) (y :y) (w :width) (h :height)) 
        (list :points (list (vector (list x y)
                                    (list (+ x w) y)
                                    (list (+ x w) (+ y h))
                                    (list x (+ y h)))))))
    (polygon
      (let* ((pairs (split-sequence:split-sequence #\space (getf obj :points)))
             (points (loop for pair in pairs
                           if (find #\, pair) collect (progn (setf (aref pair (search "," pair)) #\space)
                                                             (read-from-string (format nil "(~a)" pair))))))
        (list :points (list (coerce points 'vector)))))
    (path
      (multiple-value-bind (parts disconnected)
          (get-points-from-path (getf obj :d) :curve-resolution curve-resolution)
        (list :points parts :meta (list :disconnected disconnected))))
    (ellipse 
      (with-plist-string-reads obj ((x :cx) (y :cy) (rx :rx) (ry :ry))
        (list :points (list (get-points-from-ellipse x y rx ry :curve-resolution curve-resolution)))))
    (circle
      (with-plist-string-reads obj ((x :cx) (y :cy) (r :r))
        (list :points (list (get-points-from-ellipse x y r r :curve-resolution curve-resolution)))))
    (t
      (error 'not-an-object))))

(defun get-node-attr (node attr-name)
  "Given a node, get the attribute stored under attr-name."
  (cadr (dat/xml::find-attrib attr-name node)))

(defun parse-svg-nodes (nodes &key parent-group (next-id 0) save-attributes (group-id-attribute-name "id"))
  "Given an SVG doc read via dat/xml:parse, return two things:

    1. A list of plist objects describing ALL the objects found in the SVG file.
       Each object stores the group it's part of along with its attributes and
       transformations.
    2. A list of plist objects describing ALL the groups found, each storing its
       group id (created if not explicit) and any transformations that group has.
  
  The idea is that given this data, we can easily generate polygons for each
  object and then apply transformations to it starting with its top-level group
  and working down to the object's transformations itself."
  (let ((objs nil)
        (groups nil))
    (loop for node in (xml-node-children nodes)
          do (let ((tag (xml-node-name node)))
               (if (equal tag "g")
                   (let* ((gid (get-node-attr node group-id-attribute-name))
                          (gid (if gid gid (get-node-attr node "id")))
                          (gid (list (if gid gid (incf next-id))))
                          (full-gid (if parent-group
                                        (append parent-group gid)
                                        gid)))
                     (multiple-value-bind (sub-nodes sub-groups) (parse-svg-nodes node
                                                                                  :parent-group full-gid
                                                                                  :next-id next-id
                                                                                  :save-attributes save-attributes
                                                                                  :group-id-attribute-name group-id-attribute-name)
                       (setf objs (append sub-nodes objs))
                       (push (list :group gid :transform (parse-transform (get-node-attr node "transform")) :groups sub-groups) groups)))
                   (let* ((gid parent-group)
                          (obj (list :type tag :group gid))
                          (tagsym (intern (string-upcase tag) :dat/svg))
                          (attrs (append (case tagsym
                                           (rect (list "x" "y" "width" "height"))
                                           (polygon (list "points"))
                                           (path (list "d"))
                                           (ellipse (list "cx" "cy" "rx" "ry"))
                                           (circle (list "cx" "cy" "r"))
                                           (t nil))
                                         save-attributes)))
                     (when attrs
                       (push (append obj (loop for attr in (append attrs (list "transform" "fill" "style" "opacity"))
                                               for val = (get-node-attr node attr)
                                               for parsed = (if (and val (equal attr "transform")) (parse-transform val) val)
                                               if parsed append (list (read-from-string (format nil ":~a" attr)) parsed)))
                             objs))))))
    (values objs groups)))

(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
  returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun parse-svg-string (svg-str &key (curve-resolution 10) scale save-attributes (group-id-attribute-name "id"))
  "Parses an SVG string, creating the nodes and groups from the SVG, then
  converts each object into a set of points using the data in that object and
  the transformations from the groups the object belongs to (and the object's
  own transformations).

  SVG object curve resolutions can be set via :curve-resolution (the higher the
  value, the more accurate curves are)."
  (multiple-value-bind (nodes groups)
      (parse-svg-nodes (xml-parse svg-str :quash-errors nil) :save-attributes save-attributes :group-id-attribute-name group-id-attribute-name)
    (remove-if
      'null
      (mapcar (lambda (node)
                (handler-case
                  (let* ((points-and-meta (convert-to-points node :curve-resolution curve-resolution))
                         (points-and-holes (getf points-and-meta :points))
                         (points (apply-transformations (car points-and-holes) node groups :scale scale))
                         (holes nil))
                    (dolist (hole (cdr points-and-holes))
                      (push (coerce (apply-transformations hole node groups :scale scale) 'vector) holes))
                    (append node (list :point-data (coerce points 'vector) :holes holes :meta (getf points-and-meta :meta))))
                  (not-an-object ()
                    nil)))
              nodes))))

(defun parse-svg-file (filename &key (curve-resolution 10) scale save-attributes (group-id-attribute-name "id"))
  "Simple wrapper around parse-svg-string.
  
  SVG object curve resolutions can be set via :curve-resolution (the higher the
  value, the more accurate curves are)."
  (parse-svg-string (file-contents filename) :curve-resolution curve-resolution :scale scale :save-attributes save-attributes :group-id-attribute-name group-id-attribute-name))
