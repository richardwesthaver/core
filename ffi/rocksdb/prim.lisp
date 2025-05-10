;;; rocksdb/prim.lisp --- RocksDB Primitive FFI Macros

;;

;;; Code:
(in-package :rocksdb)

;;; Early Macros
#|
  note: 
    unable to
      optimize away %SAP-ALIEN
    because:
      forced to do runtime allocation of alien-value structure
    --> STD/ALIEN:DEFAR PROGN LOCALLY DEFINE-ALIEN-ROUTINE PROGN DEFUN 
    --> PROGN SB-IMPL::%DEFUN SB-IMPL::%DEFUN SB-INT:NAMED-LAMBDA FUNCTION 
    --> BLOCK WITH-ALIEN SYMBOL-MACROLET SYMBOL-MACROLET SYMBOL-MACROLET 
    --> VALUES 
    ==>
      1

  note: 
    doing SAP to pointer coercion (cost 20)
    --> STD/ALIEN:DEFAR PROGN LOCALLY DEFINE-ALIEN-ROUTINE PROGN DEFUN 
    --> PROGN SB-IMPL::%DEFUN SB-IMPL::%DEFUN SB-INT:NAMED-LAMBDA FUNCTION 
    --> BLOCK WITH-ALIEN SYMBOL-MACROLET SYMBOL-MACROLET SYMBOL-MACROLET 
    --> VALUES 
    ==>
      1
|#
(defmacro def-with-errptr (name result-type &rest args)
  `(progn
     (defar ,name ,result-type ,@args (errptr rocksdb-errptr))
     (export '(,name) :rocksdb)))

(defmacro define-opt (name)
  (let ((c-fn (symbolicate name '-create))
        (d-fn (symbolicate name '-destroy))
        (typ (symbolicate name '-t)))
    `(progn
       (define-alien-type ,name (struct ,typ))
       (defar ,c-fn (* ,name))
       (defar ,d-fn void
         (opt (* ,name)))
       (export '(,c-fn ,d-fn ,name) :rocksdb))))

(defmacro define-opt-accessor (opt name &optional val)
  (let* ((g-fn (symbolicate opt '-get- name))
         (s-fn (symbolicate opt '-set- name)))
    (if val
        `(progn
           (defar ,s-fn void
             (opt (* ,opt))
             (val ,val))
           (defar ,g-fn ,val
             (opt (* ,opt)))
           (export '(,g-fn ,s-fn) :rocksdb))
        `(progn
           (defar ,s-fn void
             (opt (* ,opt)) 
             (val boolean))
           (defar ,g-fn boolean
             (opt (* ,opt)))
           (export '(,g-fn ,s-fn) :rocksdb)))))

(defmacro export-opt-accessors (opt &rest names)
  (let ((forms
          (loop for n in names
                collect `(export (list
                                  ',(symbolicate opt '-get- n)
                                  ',(symbolicate opt '-set- n)) 
                                 :rocksdb))))
    `(progn ,@forms)))
