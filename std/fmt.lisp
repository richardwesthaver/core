;;; std/fmt.lisp --- printer and format utils

;;; Code:
(in-package :std/fmt)

(defun iprintln (x &optional (n 2) stream)
  "Print object X with indentation N to stream followed by a new line."
  (println (format nil "~A~A" (make-string n :initial-element #\Space) x) stream))

(defun printer-status ()
  "Return the current printer status."
  (format t ";;           *print-array* = ~a~%" *print-array*)
  (format t ";;            *print-base* = ~a~%" *print-base*)
  (format t ";;            *print-case* = ~a~%" *print-case*)
  (format t ";;          *print-circle* = ~a~%" *print-circle*)
  (format t ";;          *print-escape* = ~a~%" *print-escape*)
  (format t ";;          *print-gensym* = ~a~%" *print-gensym*)
  (format t ";;          *print-length* = ~a~%" *print-length*)
  (format t ";;           *print-level* = ~a~%" *print-level*)
  (format t ";;           *print-lines* = ~a~%" *print-lines*)
  (format t ";;     *print-miser-width* = ~a~%" *print-miser-width*)
  (format t ";; *print-pprint-dispatch* = ~a~%" *print-pprint-dispatch*)
  (format t ";;          *print-pretty* = ~a~%" *print-pretty*)
  (format t ";;           *print-radix* = ~a~%" *print-radix*)
  (format t ";;        *print-readably* = ~a~%" *print-readably*)
  (format t ";;    *print-right-margin* = ~a~%" *print-right-margin*))

(defun fmt-row (data &optional stream)
  "Format DATA as a table row to STREAM."
  (format stream "| ~{~A~^ | ~} |~%" data))

(defun format-sxhash (code &optional stream)
  "Turn the fixnum value CODE into a human-friendly string. CODE should
be produced by `sxhash'."
  (let (r)
    (dotimes (i 8 r)
      (push (ldb (byte 8 (* i 8)) code) r))
    (format
     stream
     "~{~A~^-~}"
     (mapcar
      (lambda (x) (format nil "~{~(~2,'0x~)~}" x))
      (group r 2)))))

;;; Trees

;; from https://gist.github.com/WetHat/9682b8f70f0241c37cd5d732784d1577

;; Example:

;; (let ((tree '(A B1 B2 (B3 C1) C2)))
;;     ; enumerate all layout options and draw the tree for each one.
;;     (dolist (layout '(:up :centered :down))
;;         (format t "Layout = :~A~%" layout)
;;         (fmt-tree t tree :layout layout)))

;; Layout = :UP
;;  ╭─ C2
;;  │   ╭─ C1
;;  ├─ B3
;;  ├─ B2
;;  ├─ B1
;;  A
;; Layout = :CENTERED
;;  ╭─ B2
;;  ├─ B1
;;  A
;;  ├─ B3
;;  │   ╰─ C1
;;  ╰─ C2
;; Layout = :DOWN
;;  A
;;  ├─ B1
;;  ├─ B2
;;  ├─ B3
;;  │   ╰─ C1
;;  ╰─ C2

;;                       Unicode    plain ASCII representation
(defvar *space*      "    ")
(defvar *upper-knee* " ╭─ ") ; " .- "
(defvar *pipe*       " │  ") ; " |  "
(defvar *tee*        " ├─ ") ; " +- "
(defvar *lower-knee* " ╰─ ") ; " '- "

(defun format-tree-segments (node &key (layout :centered)
                                       (node-formatter #'write-to-string))
  "Format the tree-segments of NODE."
  (unless node
    (return-from format-tree-segments nil)) ; nothing to do here
  (setq node (ensure-cons node))
  (flet ((prefix-node-strings (child-node &key layout node-formatter
                                               (upper-connector *pipe*)
                                               (root-connector  *tee*)
                                               (lower-connector *pipe*))
           "A local utility to add connectors to a string representation
                 of a tree segment to connect it to other tree segments."
           (multiple-value-bind (u r l)
               (format-tree-segments child-node
                                     :layout         layout
                                     :node-formatter node-formatter)
                                        ; prefix tree segment with connector glyphs to connect it to
                                        ; other segments.
             (nconc
              (mapcar
               (lambda (str) (concatenate 'string upper-connector str))
               u)
              (list (concatenate 'string root-connector r))
              (mapcar
               (lambda (str) (concatenate 'string lower-connector str))
               l)))))
    (let* ((children (rest node))
           (pivot (case layout ; the split point of the list of children
                    (:up   (length children)) ; split at top
                    (:down 0)                 ; split at bottom
                    (otherwise (round (/ (length children) 2))))) ; bisect
           (upper-children (reverse (subseq children 0 pivot))) ; above root
           (lower-children (subseq children pivot))) ; nodes below root
      (values ; compile multiple value return of upper-children root lower children
       (when upper-children
         (loop with top = (prefix-node-strings (first upper-children)
                                               :layout layout
                                               :node-formatter node-formatter
                                               :upper-connector *space*
                                               :root-connector  *upper-knee*) ; top node has special connectors
               for child-node in (rest upper-children)
               nconc (prefix-node-strings child-node
                                          :layout layout
                                          :node-formatter node-formatter)
               into strlist
               finally (return (nconc top strlist))))
       (let ((root-name (funcall node-formatter (car node)))) ; root node
         (if (= 1 (length root-name))
             (concatenate 'string " " root-name) ; at least 2 chars needed
                                        ;else
             root-name))
       (when lower-children
         (loop for (head . tail) on lower-children
               while tail ; omit the last child
               nconc (prefix-node-strings head
                                          :layout layout
                                          :node-formatter node-formatter)
               into strlist
               finally (return
                         (nconc
                          strlist
                                        ; bottom node has special connectors
                          (prefix-node-strings head
                                               :layout layout
                                               :node-formatter  node-formatter
                                               :root-connector  *lower-knee*
                                               :lower-connector *space*)))))))))

(defun fmt-tree (stream root &key 
			     (plist nil)
			     (layout :centered)
                             (node-formatter #'write-to-string))
  "Format ROOT as a tree of nodes, printing to STREAM."
  (multiple-value-bind (u r l)
      (format-tree-segments (if plist (cons (car root) (group (cdr root) 2)) root)
                            :layout layout
                            :node-formatter node-formatter)
    (format stream "~{~A~%~}" (nconc u (list r) l))))

(defun human-readable-size (number)
  (check-type number integer)
  (loop for size in '(80 70 60 50 40 30 20 10)
        and unit in '("YB" "ZB" "EB" "PB" "TB" "GB" "MB" "KB")
        when (> (ash number (- size)) 0)
        do (return-from human-readable-size
             (format nil "~,2F ~A"
                     (float (/ number (ash 1 size)))
                     unit))))

;;; MOP
(defvar *print-slot-indent* 0)

(defun describe-slot (name value &optional (max-slot-name-length 30) (stream t) (indent *print-slot-indent*))
  "Describe slot NAME with associated VALUE."
  (format stream "~%~A~VA = ~A" (make-string indent :initial-element #\space) max-slot-name-length name (prin1-to-line value)))

;; FROM: sb-impl describe
(defun %describe-object (object stream)
  (let* ((class (class-of object))
	 (slotds (sb-mop:class-slots class))
	 (max-slot-name-length 30)
	 (plist nil))
    ;; Figure out a good width for the slot-name column.
    (flet ((adjust-slot-name-length (name)
	     (setf max-slot-name-length
		   (max max-slot-name-length (length (symbol-name name))))))
      (dolist (slotd slotds)
	(adjust-slot-name-length (sb-mop:slot-definition-name slotd))
	(push slotd (getf plist (sb-mop:slot-definition-allocation slotd)))))
    ;; Now that we know the width, we can print.
      (sb-int:doplist (allocation slots) plist
	(dolist (slotd (nreverse slots))
	  (describe-slot
	   (sb-mop:slot-definition-name slotd)
	   (sb-pcl::slot-value-for-printing object (sb-mop:slot-definition-name slotd)))))
    (unless slotds
      (format stream "~@:_No slots."))))

(defun print-slots (object &optional (stream t))
  "Print the slots of OBJECT to STREAM."
  (let ((*print-right-margin* (or *print-right-margin* 72))
	(*print-circle* t)
	(*print-circle-not-shared* t)
	(*print-pretty* t)
	(*suppress-print-errors*
	  (if (subtypep 'serious-condition *suppress-print-errors*)
	      *suppress-print-errors*
	      'serious-condition)))
    (%describe-object object stream)))

(defun format-slots (stream &rest slots)
  "Print SLOTS to STREAM."
  (let ((*print-right-margin* (or *print-right-margin* 72))
	(*print-circle* t)
	(*print-circle-not-shared* t)
	(*print-pretty* t)
	(*suppress-print-errors*
	  (if (subtypep 'serious-condition *suppress-print-errors*)
	      *suppress-print-errors*
	      'serious-condition)))
    (sb-int:doplist (k v) (print slots)
      (describe-slot (string k) v 30 stream))
    (force-output stream)))

;;; Box
;; TODO 2025-04-04: 
;; APL Box Formatting (BQN/Dyalog/J)

;;; Drawing

;; These bits of lovely code are sourced from here:
;; https://github.com/whalliburton/academy/blob/87a1a13ffbcd60d8553e42e647c59486c761e8cf/drawing.lisp
(defun make-bitmap (width height &optional contents)
  (if contents
    (make-array (list height width) :initial-contents contents)
    (make-array (list height width) :initial-element nil)))

(defvar *bitmap*)
(defvar *bitmap-overwrite* nil)

(defmacro with-bitmap ((width height) &body body)
  `(let ((*bitmap* (or *bitmap-overwrite* (make-bitmap ,width ,height))))
     ,@body))

(defun outside-bounds (x y &optional (bitmap *bitmap*))
  (destructuring-bind (height width) (array-dimensions bitmap)
    (or (< x 0) (< y 0) (>= x width) (>= y height))))

(defun set-pixel (x y &optional (bitmap *bitmap*) (value t))
  (unless (outside-bounds x y bitmap)
    (setf (aref bitmap y x) value)))

(defvar *comic-strip*)
(defvar *save-drawing-name* nil)

(defun draw (&optional (bitmap *bitmap*))
  (cond
    ((and (boundp '*comic-strip*) (not (eq bitmap (comic-strip-bitmap *comic-strip*))))
     (draw-on-comic-strip *comic-strip* bitmap))
    (t (destructuring-bind (height width) (array-dimensions bitmap)
         (loop for y from 0 to (1- height) by 2
               do (loop for x from 0 to (1- width)
                        do (princ
                            (let ((top (aref bitmap y x))
                                  (bottom (when (< y (1- height)) (aref bitmap (1+ y) x))))
                              (cond
                                ((or (stringp top) (stringp bottom))
                                 (incf x (length (or top bottom)))
                                 (or top bottom))
                                ((and top bottom) #\FULL_BLOCK)
                                (top              #\UPPER_HALF_BLOCK)
                                (bottom           #\LOWER_HALF_BLOCK )
                                (t                #\space)))))
                  (fresh-line))))))

(defun draw-from-list (bit-list width)
  (let ((rows (group bit-list width)))
    (draw (make-array (list (length rows) width) :initial-contents rows))))

(defun copy-onto-bitmap (bitmap pattern x y)
  (loop for row in pattern
        for yi from y
        do (loop for character across row
                 for xi from x
                 do (setf (aref bitmap yi xi) (not (eq character #\space)))))
  bitmap)

(defun center-on-bitmap (bitmap pattern)
  (destructuring-bind (height width) (array-dimensions bitmap)
    (copy-onto-bitmap bitmap pattern
                      (- (floor width 2) (floor (length (car pattern)) 2))
                      (- (floor height 2) (floor (length pattern) 2)))))

(defun pattern-to-bitmap (pattern)
  (let ((bitmap (make-bitmap (length (car pattern)) (length pattern))))
    (loop for row in pattern
          for y from 0
          do (loop for character in (coerce row 'list)
                   for x from 0
                   do (when (not (char= character #\space))
                        (setf (aref bitmap y x) t))))
    bitmap))

(defun smile ()
  "When you're smiling, the whole world smiles with you."
  (draw (pattern-to-bitmap '("  ****  "
                             " *    * "
                             "* *  * *"
                             "*      *"
                             "* *  * *"
                             "*  **  *"
                             " *    * "
                             "  ****  "))))

(defun draw-border (&optional (bitmap *bitmap*))
  (destructuring-bind (height width) (array-dimensions bitmap)
    (loop for x from 0 to (1- width)
          do (setf (aref bitmap 0 x) t
                   (aref bitmap (1- height) x) t))
    (loop for y from 1 to (- height 2)
          do (setf (aref bitmap y 0) t
                   (aref bitmap y (1- width)) t))))

;;; Described in
;;; Computer Graphics - Principles and Practice by Donald Hearn and M. Pauline Baker

(defun draw-circle (x-center y-center radius &optional (bitmap *bitmap*))
  (labels ((pixel (x y) (set-pixel (+ x-center x) (+ y-center y) bitmap))
           (draw-points (x y)
             (pixel x     y)
             (pixel (- x) y)
             (pixel x     (- y))
             (pixel (- x) (- y))
             (pixel y     x)
             (pixel (- y) x)
             (pixel y     (- x))
             (pixel (- y) (- x))))
    (loop with x = 0
          with y = radius
          with p = (- 1 radius)
          initially (draw-points x y)
          while (< x y)
          do (incf x)
             (if (< p 0)
               (incf p (+ (* 2 x) 1))
               (progn
                 (decf y)
                 (incf p (+ (* 2 (- x y)) 1))))
             (draw-points x y))))

(defun bullseye (&key (size 64) (step 4) filled (draw t))
  "Draw a bullseye."
  (with-bitmap (size size)
    (let ((mid (floor size 2)))
      (loop for radius from 2 to mid by step
            do (draw-circle mid mid radius))
      (when filled
        (loop for x from 2 to mid by (* 2 step)
              do (fill-bitmap (+ mid x 1) mid)))
      (if draw
        (draw)
        *bitmap*))))

(defun moire (&key (size 64) (step 4) (filled t) (offset 16))
  "Draw a Moiré pattern."
  (let ((*bitmap* (make-bitmap (+ size offset) size))
        (one (bullseye :size size :step step :filled filled :draw nil)))
    (copy-bitmap-onto-bitmap one *bitmap* 0 0)
    (copy-bitmap-onto-bitmap one *bitmap* offset 0)
    (draw *bitmap*)))

(defun draw-line (xa ya xb yb &optional (bitmap *bitmap*))
  (let* ((dx (- xb xa))
         (dy (- yb ya))
         (steps (if (> (abs dx) (abs dy)) (abs dx) (abs dy)))
         (xi (/ dx steps))
         (yi (/ dy steps)))
    (set-pixel xa ya bitmap)
    (loop with x = xa
          with y = ya
          for k from 0 to (1- steps)
          do (incf x xi)
             (incf y yi)
             (set-pixel (floor x) (floor y) bitmap))))

(defun sunbeam (&key (step 8) (size 64))
  "Draw a sunbeam."
  (with-bitmap (size size)
    (loop for x from 0 to size by step
          do (draw-line 0 (1- size) x 0)
             (draw-line 0 (1- size) (1- size) x))
    (draw)))

(defun fill-bitmap (x y &optional (bitmap *bitmap*))
  (unless (outside-bounds x y bitmap)
    (unless (aref bitmap y x)
      (setf (aref bitmap y x) t)
      (fill-bitmap (+ x 1) y bitmap)
      (fill-bitmap (- x 1) y bitmap)
      (fill-bitmap x (+ y 1) bitmap)
      (fill-bitmap x (- y 1) bitmap))))

(defun draw-filled-circle (x-center y-center radius &optional (bitmap *bitmap*))
  (draw-circle x-center y-center radius bitmap)
  (fill-bitmap x-center y-center bitmap))

(defun sun (&key (size 64))
  "Draw a sun."
  (with-bitmap (size size)
    (let ((mid (floor size 2)))
      (draw-filled-circle mid mid (1- mid))
      (draw))))

;;; Attention Hackers! Exercises are good for the soul.
;;;
;;;    Someone with the desire could expand PEACE to draw peace symbols of any size.

(defun peace ()
  "Peace on Earth."
  (with-bitmap (12 12)
    (draw-circle 6 6 5)
    (draw-line 6 10 6 1)
    (draw-line 6 6 3 9)
    (draw-line 6 6 9 9)
    (draw)))

(defun copy-bitmap-onto-bitmap (from-bitmap to-bitmap x y &key (fn (lambda (a b) (or a b))))
  (destructuring-bind (height width) (array-dimensions from-bitmap)
    (loop for yi from 0 to (1- height)
          do (loop for xi from 0 to (1- width)
                   do (let ((from (aref from-bitmap yi xi))
                            (to (aref to-bitmap (+ y yi) (+ x xi))))
                        (set-pixel (+ x xi) (+ y yi)
                                   to-bitmap
                                   (funcall fn from to)))))))

(defun center-bitmap-onto-bitmap (from-bitmap to-bitmap)
  (destructuring-bind (fh fw) (array-dimensions from-bitmap)
    (destructuring-bind (th tw) (array-dimensions to-bitmap)
      (copy-bitmap-onto-bitmap from-bitmap to-bitmap
                               (floor (- tw fw) 2)
                               (floor (- th fh) 2)))))

(defstruct comic-strip bitmap width height columns rows column)

(defmacro with-comic-strip ((&key (width 32) (height 32) (columns 3) (action 'draw)) &body body)
  `(let ((*comic-strip* (make-comic-strip :bitmap (make-bitmap (* ,width ,columns) 0)
                                          :width ,width :height ,height :columns ,columns
                                          :rows 0 :column 0)))
     ,@body
     (,action (comic-strip-bitmap *comic-strip*))))

(defun draw-on-comic-strip (strip cell-bitmap)
  (with-slots (rows column width height columns bitmap) strip
    (when (= column 0)
      (incf rows)
      (setf bitmap
            (adjust-array bitmap (list (* rows height) (* columns width)) :initial-element nil)))
    (copy-bitmap-onto-bitmap cell-bitmap bitmap (* column width) (* (1- rows) height))
    (setf column (mod (1+ column) columns))))

(defun plot-function (fn start end &optional (width 64) (height 32))
  "Show a graph of FN of size WIDTHxHEIGHT with the X axis bounded by START and END."
  (with-bitmap (width height)
    (let ((step (/ (- end start) width))
          (mid (floor height 2)))
      (loop for x from start to end by step
            for xi from 0
            do (let ((y (- mid (floor (funcall fn x) step))))
                 (set-pixel xi (floor y))))
      (draw-border)
      (draw))))

(defun rotate-rows-to-columns (rows)
  (loop for remaining = rows then (mapcar #'cdr remaining)
        while (not (every #'null remaining))
        collect (mapcar #'car remaining)))

(defun maximize-length (list &key (key #'identity))
  (loop for element in list maximizing (length (funcall key element))))

(defun pad-list (list length &optional (pad-element nil))
  (loop for el on list
        for x from 1
        do (when (null (cdr el))
             (setf (cdr el) (make-list (- length x) :initial-element pad-element))
             (return list))))

(defun print-table (rows &key (gap "  ") (align :left))
  (when rows
    (loop
      with max-row-length = (apply #'max (mapcar #'length rows))
      with control-string =
                          (format nil
                                  (concatenate
                                   'string "~{~~~D" (ecase align (:right "@") (:left "")) "A~^" gap "~}~%")
                                  (mapcar (lambda (row) (maximize-length row :key #'princ-to-string))
                                          (rotate-rows-to-columns rows)))
      for row in (mapcar (lambda (row) (pad-list row max-row-length "")) rows)
      do (apply #'format t control-string row))))

(defun print-heading (text &key (underline "▀"))
  (terpri)
  (write-string text)
  (fresh-line)
  (dotimes (i (length text)) (write-string underline))
  (fresh-line)
  (terpri))

(defun print-in-box (string)
  (flet ((print-times (count string) (dotimes (x count) (princ string))))
    (let* ((lines (sb-unicode:lines string))
           (columns (apply #'max (mapcar #'length lines))))
      (princ "┌") (print-times columns "─") (princ "┐") (fresh-line)
      (loop for line in lines
            do (princ "│")
               (princ line)
               (print-times (- columns (length line)) " ")
               (princ "│")
               (fresh-line))
      (princ "└") (print-times columns "─") (princ "┘")
      (fresh-line))))

(defmacro print-boxed (&rest body)
  `(print-in-box
    (with-output-to-string (*standard-output*)
      ,@body)))
