;;; auto.lisp --- Cellular Automata

;; 

;;; Code:
(in-package :math/auto)

(defparameter *rule-patterns*
  '((nil nil nil) (nil nil t) (nil t nil) (nil t t) (t nil nil) (t nil t) (t t nil) (t t t)))

(defun pick-rules (rule)
  (loop for row in *rule-patterns*
        for index from 0
        when (logbitp index rule)
        collect row))

(defun cellular-automata (&optional (rule 30) (size 64))
  "Draw an elementary cellular automata."
  (let ((rules (pick-rules rule)))
    (let ((bitmap (make-bitmap size size)))
      (setf (aref bitmap 0 (floor size 2)) t)
      (loop for y from 1 to (1- size)
            do (loop for x from 1 to (- size 2)
                     do (when (find (list
                                     (aref bitmap (1- y) (1- x))
                                     (aref bitmap (1- y) x)
                                     (aref bitmap (1- y) (1+ x)))
                                    rules :test #'equalp)
                          (setf (aref bitmap y x) t))))
      (draw bitmap))))

(defun age-bitmap (bitmap)
  (destructuring-bind (height width) (array-dimensions bitmap)
    (let ((next-generation (make-bitmap width height)))
      (loop for y from 0 to (1- height)
            do (loop for x from 0 to (1- width)
                     do (let ((neighbors
                                (let ((total 0))
                                  (loop for (dx dy) in '((-1 -1) (0 -1) (1 -1)
                                                         (-1 0)         (1 0)
                                                         (-1 1)  (0 1)  (1 1))
                                        do (let ((xi (+ dx x))
                                                 (yi (+ dy y)))
                                             (unless (or (minusp xi) (minusp yi)
                                                         (= xi width)
                                                         (= yi height))
                                               (when (aref bitmap yi xi)
                                                 (incf total)))))
                                  total)))
                          (setf (aref next-generation y x)
                                (if (aref bitmap y x) ; we got a live one
                                  (cond
                                    ((< neighbors 2) nil)  ; lonliness
                                    ((<= neighbors 3) t)   ; party on dude!
                                    ((> neighbors 3) nil)) ; overcrowding
                                  (when (= neighbors 3) ; kinky!
                                    t))))))
      next-generation)))

(defun life (&key (pattern '("  ***"
                             " *  *"
                             "*   *"))
                  (steps 32)
                  (size 32))
  "Play Conway's Game of Life."
  (let ((grid (make-bitmap size size)))
    (cond
      ((consp pattern)
       (std/print::center-on-bitmap grid pattern))
      ((arrayp pattern)
       (std/print::center-bitmap-onto-bitmap pattern grid)))
    (draw grid)
    (dotimes (x steps)
      (setf grid (age-bitmap grid))
      (draw grid))))
