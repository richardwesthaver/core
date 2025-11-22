;;; std/macs/unit.lisp

;;

;;; Commentary:

;; ref: https://www.dot.state.wy.us/files/live/sites/wydot/files/shared/Highway_Development/Surveys/Survey%20Manual/Appendix%20G%20-%20Units%20of%20Measure.pdf

;;; Code:
(in-package :std/macs)
(eval-always
  (labels ((defunits-chaining (u units prev)
             (if (member u prev)
                 (error "~{ ~a~^ depends on~}"
                        (cons u prev)))
             (let ((spec (find u units :key #'car)))
               (if (null spec)
                   (error "Unknown unit ~a" u)
                   (let ((chain (cadr spec)))
                     (if (listp chain)
                         (* (car chain)
                            (defunits-chaining
                                (cadr chain)
                              units
                              (cons u prev)))
                         chain))))))
    (defmacro! defunits (quantity base-unit &rest units)
      `(progn
         (defmacro ,(symbolicate 'unit-of- quantity) (,g!val ,g!un)
           `(* ,,g!val
               ,(case ,g!un
                  ((,base-unit) 1)
                  ,@(mapcar (lambda (x)
                              `((,(car x))
                                ,(defunits-chaining
                                     (car x)
                                     (cons
                                      `(,base-unit 1)
                                      (group units 2))
                                   nil)))
                     (group units 2)))))
         (deftype ,(symbolicate quantity '-designator) ()
           '(member ,@(loop for k in units by 'cddr
                            collect (keywordicate k))))))))

(defunits distance m
  km 1000
  cm 1/100
  mm (1/10 cm)
  nm (1/1000 mm)
  yard 9144/10000 ; Defined in 1956
  foot (1/3 yard)
  inch (1/12 foot)
  mile (1760 yard)
  furlong (1/8 mile)
  fathom (2 yard) ; Defined in 1929
  nautical-mile 1852
  cable (1/10 nautical-mile)
  light-year (9.4607e+12 mile))
