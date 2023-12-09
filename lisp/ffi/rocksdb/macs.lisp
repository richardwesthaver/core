(in-package :rocksdb)

;;; Macros
(defmacro def-with-errptr (name result-type &rest args)
  `(define-alien-routine ,name ,result-type ,@args (errptr rocksdb-errptr)))

(defmacro define-opt (name &rest fields)
  `(progn
     (define-alien-type ,name (struct ,(symbolicate name '-t)))
     (define-alien-routine ,(symbolicate name '-create) (* ,name))
     (define-alien-routine ,(symbolicate name '-destroy) void
       (opt (* ,name)))
  ,@(dolist (f fields)
      (if (listp f)
          (eval
           (nconc
            (list 
             'define-alien-routine 
             (symbolicate name '-set- (car f))
             'void 
             `(opt (* ,name)))
            (cdr f)))
          (eval
           (list
            'define-alien-routine 
            (symbolicate name '-set- f) 
            'void 
            `(opt (* ,name)) 
            '(val boolean)))))))
