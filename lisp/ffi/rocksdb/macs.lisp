(in-package :rocksdb)

;;; Macros
(defmacro def-with-errptr (name result-type &rest args)
  `(progn
     (define-alien-routine ,name ,result-type ,@args (errptr rocksdb-errptr))
     (export '(,name) :rocksdb)))

(defmacro define-opt (name)
  (let ((c-fn (symbolicate name '-create))
        (d-fn (symbolicate name '-destroy))
        (typ (symbolicate name '-t)))
    `(progn
       (define-alien-type ,name (struct ,typ))
       (define-alien-routine ,c-fn (* ,name))
       (define-alien-routine ,d-fn void
         (opt (* ,name)))
       (export '(,c-fn ,d-fn ,name) :rocksdb))))

(defmacro define-opt-accessor (opt name &optional val)
  (let* ((g-fn (symbolicate opt '-get- name))
         (s-fn (symbolicate opt '-set- name)))
    (if val
        `(progn
           (define-alien-routine ,s-fn void
             (opt (* ,opt))
             (val ,val))
           (define-alien-routine ,g-fn ,val
             (opt (* ,opt)))
           (export '(,g-fn ,s-fn) :rocksdb))
        `(progn
           (define-alien-routine ,s-fn void
             (opt (* ,opt)) 
             (val unsigned-char))
           (define-alien-routine ,g-fn unsigned-char
             (opt (* ,opt)))
           (export '(,g-fn ,s-fn) :rocksdb)))))

(defmacro export-opt-accessors (opt &rest names)
  (let ((forms
          (loop for n in names
                collect `(export (list
                                  ',(symbolicate opt '-get- n)
                                  ',(symbolicate opt '-set- n)) :rocksdb))))
    `(progn ,@forms)))
