;;; ana.lisp --- anaphoric macros

;;; Code:
(in-package :std)

(in-readtable :std)

;; Graham's alambda
(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; Graham's aif
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

;; ;; TODO 2023-09-05: wrap, document, optimize, hack
;; (reexport-from :sb-int :include '(:awhen :acond))

(defmacro! nlet-tail (n letargs &body body)
  (let ((gs (loop for i in letargs
               collect (gensym))))
    `(macrolet
         ((,n ,gs
            `(progn
               (psetq
                ,@(apply #'nconc
                         (mapcar
                          #'list
                          ',(mapcar #'car letargs)
                          (list ,@gs))))
               (go ,',g!n))))
       (block ,g!b
         (let ,letargs
           (tagbody
              ,g!n (return-from
                    ,g!b (progn ,@body))))))))

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

;; swiped from fiveam. This is just like acond except it assumes that
;; the TEST in each element of CLAUSES returns two values as opposed
;; to one.
(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (with-gensyms (val foundp)
        (destructuring-bind ((test &rest progn) &rest others)
            clauses
          `(multiple-value-bind (,val ,foundp)
               ,test
             (if (or ,val ,foundp)
                 (let ((it ,val))
                   (declare (ignorable it))
                   ,@progn)
                 (acond2 ,@others)))))))
