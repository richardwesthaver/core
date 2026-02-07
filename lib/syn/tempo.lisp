;;; tempo.lisp --- Lisp Template Interpreter

;; Base on Emacs tempo.el

;;; Commentary:

;; A template is defined as a list of items to be inserted in the current
;; buffer at point. Some of the items can be simple strings, while other can
;; control formatting or define special points of interest in the inserted
;; text.

;;; Code:
(in-package :syn/tempo)

(defvar *default-tempo-langs* '(:c :lisp))

(defparameter *tempo-interactive* nil)

(defvar *tempo-table* (make-hash-table)
  "A hash-table containing lang designators such as :LISP or :C. T is the global
designator. The values are hash-tables where the keys are a template name and
values are templates.")

(defclass template (vertex ast) 
  ((lang :initarg :lang :initform :lisp :allocation :class :accessor lang))
  (:documentation "Base class for template objects. Template forms are stored
  in the AST slot. LANG is a class-allocated keyword. ID is a unique
  identifier for this template.")
  (:default-initargs :id (required-argument :id)))

(defclass simple-template (template)
  ((tags :initarg :tags :initform nil :accessor tags))
  (:documentation "A template which may be interpreted from one of the symbols in TAGS."))

(defun get-template (lang name)
  (gethash name (gethash lang *tempo-table*)))

(defun (setf get-template) (val lang name)
  (setf (gethash name (gethash lang *tempo-table* (setf (gethash lang *tempo-table*) (make-hash-table))))
        val))

(define-condition duplicate-template-tag () 
  ((tag :initarg :tag)
   (lang :initarg :lang))
  (:report (lambda (c s) 
             (format s "Duplicate tag found in *TEMPO-TABLE* for lang ~A: ~A" 
                     (slot-value c 'lang)
                     (slot-value c 'tag)))))
             
(define-condition duplicate-tempo-tag-warning (warning duplicate-template-tag) ())
(define-condition duplicate-tempo-tag-error (error duplicate-template-tag) ())

(defun check-tempo-tags ()
  "Assert there are no conflicts in the TAGS of all templates."
    (maphash 
     (lambda (lk lv)
       (let ((tags))
         (maphash 
          (lambda (k v)
            (dolist (x (tags v))
              (assert (not (member x tags)) nil 'duplicate-tempo-tag-error :tag x :lang lk :id (id k)))
            (nconsc tags (tags v)))
          lv)))
     *tempo-table*))

(defmethod init ((self (eql :tempo)) &key templates (interactive t))
  (dolist (l templates)
    (setf (get-template (lang l) (id l)) l))
  (check-tempo-tags)
  (setf *tempo-interactive* interactive))

(defun parse-template-element (elt ret)
  "Parse the given template element ELT and modify the list RET."
  (typecase elt
    ((or character string) (push elt ret))
    (symbol
     (ecase elt
       (n (push #\newline ret))
       (s (push #\space ret))
       (m (push '(mark) ret))))
    (number
     (typecase (car ret)
       ((cons (eql mark)) (setf (car ret) (recons (car ret) 'mark elt)))
       (t (loop repeat (1- elt) do (push (car ret) ret)))))
    (t (push (compile-and-eval elt) ret))))

(defun parse-template-body (body)
  (mapcar 'parse-template-element body))

;; (defun compile-template (tmp))

(defmacro deftemplate (name opts &body body)
  "Define a new TEMPLATE if NAME is a symbol - if it is a list the first
  element is the unique ID and the cdr is TAGS.

BODY is parsed according to PARSE-TEMPLATE-BODY. Each form is one of the
following:

- A string or character: inserted as is
- The symbol N: Insert a newline.
- The symbol S: Insert a space.
- The symbol M: saves position as a mark with an optional index.
- A number: repeat count of the previous element or mark index if previous
  element was the symbol M.
- NIL: Ignored.
- Otherwise: passed unevaluated to the handlers associated with this
  template. If all handlers return a NIL second value the form is evaluated
  and inserted as is."
  (with-gensyms (tmp)
    `(let ((,tmp ,(if (consp name)
                      (make-instance 'simple-template :id (car name) :tags (cdr name))
                      (make-instance 'template :id name))))
       (setf (ast ,tmp) (parse-template-body ',body))
       (setf (get-template ,(getf opts :lang) (id ,tmp)) ,tmp))))
