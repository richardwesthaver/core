;;; proto.lisp --- SYN/GEN Protocol

;; 

;;; Code:
(in-package :syn/gen)

(defvar *indent* "  ")
(defgeneric load-gen (self))
(defgeneric unload-gen (self))
(defgeneric gen-package (self))
(defgeneric gen-reader (self))
(defgeneric gen-reader-switch (self))

(defnode function-call () (function arguments))
(defmethod ast ((self function-call)) 
  (list (slot-value self 'function) (slot-value self 'arguments)))
(defnode src-location (ast) (line file info))
(defexpr ident (literal-expr) ())
(defmethod id ((self ident)) (val self))
(defexpr str-literal (literal-expr) ())
(defexpr num-literal (literal-expr) ())
(defexpr char-literal (literal-expr) ())
(defnode proxy (ast) (info))
(defnode empty () ())

;; Inverts the case when interning a string.
;; This is needed to keep the correct internal (inverted) case.
;; Use this function for all c depending code.
(defun cintern (name &optional package)
  (macrolet ((case-test (test string)
               `(reduce #'(lambda (a b) (and a b))
                        (mapcar (lambda(x) (or (not (both-case-p x)) (,test x)))
                                (coerce ,string 'list)))))
    (let ((string (cond ((case-test upper-case-p name) (string-downcase name))
                        ((case-test lower-case-p name) (string-upcase name))
                        (t name))))
      (if package
          (intern string package)
          (intern string)))))

(defmacro defsyntax (tags langs lambda-list &body body)
  "Define syntax for tags from specific langs."
  (let ((tags (if (consp tags) tags (list tags))))
    `(progn
       ,@(loop for i in tags 
               append
                  (loop for k in langs
                        collect
                           `(let ((tag ',i))
                              (declare (ignorable tag))
                              (defmacro ,(intern (format nil "~:@(~a~)" i) (gen-package k)) ,lambda-list
                                ,@body)))))))

;;; Utils
(defmacro quoty (item &environment env)
  "Quote undefined symbols, build functions from unknown lists."
  (cond ((eql item nil)
         (values))
        ((listp item)
         (if (or (listp (car item))
                 (not (fboundp! (car item) env)))
             `(make-instance 'function-call 
                :function (make-node ,(car item))
                :arguments (make-nodes ,(cdr item)))
             item))
        ((symbolp item)
         (if (vboundp! item env)
             item
             `',item))
        (t item)))

(defmacro make-nodes (nodes &key prepend quoty)
  "Build general or specific AST."
  (let ((prepend (if (listp prepend) prepend `(,prepend))))
    `(make-instance 'ast
       :ast 
       (list ,@(loop for i in nodes 
                     collect
                        (if prepend
                            (if quoty
                                `(,@prepend (quoty ,i))
                                `(,@prepend ,i))
                            `(%make-node (quoty ,i))))))))

(defmacro make-node (item)
  "Try to identify and make a NODE from ITEM."
  `(%make-node (quoty ,item)))

(defun %make-node (item)
  "Build NODE from ITEM."
  (cond
    ;; no item
    ((eql item nil) (values))
    ;; item is already c-mera node
    ((typep item 'node) item)
    ;; Item is most possibly an atom or a quoted symbol
    ((symbolp item) (make-instance 'ident :val item))
    ((numberp item) (make-instance 'num-literal :val item))
    ((stringp item) (make-instance 'str-literal :val item))
    ((characterp item) (make-instance 'char-literal :val item))
    ;; Item is not a known atom
    (t (error "code generator encountered an unknown atom: ~a" item))))

(defclass ast-traverser () ())

(defmethod traverse :before ((self ast-traverser) (item ast) level)
  "remove unnecessary trees"
  (declare (ignore level))
  (with-slots (nodes) item
    (loop 
      do (if (not (= (length nodes) 1))
             (loop-finish)
             (cond 
               ((eql (class-of (first nodes)) (find-class 'ast))
                (setf nodes (slot-value nodes 'ast)))
               ((eql (class-of (first nodes)) (find-class 'src-location))
                (if (eql (class-of (slot-value (first nodes) 'subnode)) (find-class 'ast))
                    (setf nodes (slot-value (slot-value (first nodes) 'subnode) 'nodes))
                    (loop-finish)))
               (t (loop-finish)))))))

(defclass code-printer ()
  ((indent :initform 0)
   (sign-stack :initform nil)
   (info-stack :initform nil)
   (stream :initform t :initarg :stream :accessor stream-of)))

;;; Printer

;; The PRINT-CODE and WRITE-CODE functions are defined here in addition to
;; several macros which may be used to define new lang-specific dispatch
;; tables, printer methods, and printer dispatch entries.
(defvar *code-dispatch-table* (copy-pprint-dispatch *ast-dispatch-table*))

(defun write-code (expr &rest args)
  (apply 'write expr :pprint-dispatch *code-dispatch-table* args))

(defun print-code (tree)
  (let ((pp (make-instance 'code-printer))
        (d (make-instance 'debug-traverser)))
    (traverse d tree 0)
    (traverse pp tree 0)))

(defmacro define-code-printer (qual node &body body)
  (if (eql :self qual)
      `(defmethod traverse ((self code-printer)
                            (node ,node)
                            level)
         (declare (ignorable level))
         ,@body)
      `(defmethod traverse ,qual ((self code-printer) (node ,node) level)
         (declare (ignorable level))
         ,@body)))

(defmacro delete-code-printer (qual node)
  (let ((quali (if (eql qual :self)
                   '()
                   `(,qual))))
    `(remove-method #'traverser
                    (find-method #'traverser
                                 ',quali
                                 (list
                                  ,(find-class 'code-printer)
                                  (find-class ',node)
                                  ,(find-class t))))))

(defmacro with-code-printer (&body body)
  `(symbol-macrolet ((stream (slot-value self 'stream))
                     (indent (format nil "~{~A~}"
                                     (loop for i
                                           from 1 
                                           to (slot-value self 'indent)
                                           collect *indent*)))
                     (%self self)
                     (%level level)
                     (--indent (decf (slot-value self 'indent)))
                     (++indent (incf (slot-value self 'indent))))
     (macrolet ((push-sign (x) `(push ,x (slot-value self 'sign-stack)))
                (pop-sign () `(pop (slot-value self 'sign-stack)))
                (top-sign () `(car (slot-value self 'sign-stack)))
                (find-sign (x) `(find ,x (slot-value self 'sign-stack)))
                (push-info (x) `(push ,x (slot-value self 'info-stack)))
                (pop-info () `(pop (slot-value self 'info-stack)))
                (top-info () `(car (slot-value self 'info-stack)))
                (find-info (x) `(find ,x (slot-value self 'info-stack)))
                (info-size () `(length (slot-value self 'info-stack)))
                (node-slot (x) `(slot-value node ',x)))
       ,@body)))

(with-code-printer
  (define-code-printer :self literal-expr
    (let ((val (val node)))
      (when val
        (cond 
          ((stringp val)
           (format stream "\"~a\"" val))
          ((characterp val)
           (cond
             ((eql val #\tab)
              (format stream "'\\t'"))
             ((or (eql val #\nul) (eql val #\null))
              (format stream "'\\0'"))
             ((eql val #\return)
              (format stream "'\\r'"))
             ((eql val #\newline)
              (format stream "'\\n'"))
             ((eql val #\')
              (format stream "'\\''"))
             (t (format stream "'~a'" val))))
          ((floatp val) (format stream "~a"
                                (substitute #\e #\d
                                            (format nil "~,8e" val))))
          (t (format stream "~a" val)))))))

;; TODO 2024-10-20: gen-file-header
;; (defclass gen-file-header (file-header)
;;   ())
