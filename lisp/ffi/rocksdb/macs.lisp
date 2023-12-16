(in-package :rocksdb)

;;; Macros
(defmacro def-with-errptr (name result-type &rest args)
  `(define-alien-routine ,name ,result-type ,@args (errptr rocksdb-errptr)))

(defmacro define-opt (name)
  `(progn
     (define-alien-type ,name (struct ,(symbolicate name '-t)))
     (define-alien-routine ,(symbolicate name '-create) (* ,name))
     (define-alien-routine ,(symbolicate name '-destroy) void
       (opt (* ,name)))))

(defmacro define-opt-accessor (opt name &optional val)
  (if val
      `(progn
         (define-alien-routine ,(symbolicate opt '-set- name) void
           (opt (* ,opt))
           (val ,val))
         (define-alien-routine ,(symbolicate opt '-get- name) ,val
           (opt (* ,opt))))
      `(progn
         (define-alien-routine ,(symbolicate opt '-set- name) void
           (opt (* ,opt)) 
           (val unsigned-char))
         (define-alien-routine ,(symbolicate opt '-get- name) unsigned-char
           (opt (* ,opt))))))
