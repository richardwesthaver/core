;;; proto.lisp --- SYN/GEN Protocol

;; 

;;; Code:
(in-package :syn/gen)

(defgeneric load-generator (self))
(defgeneric generator-package (self))

(defnode function-call () (function arguments))
(defnode source-location (ast) (line file info))
(defnode ident (id) ())
(defexpr str-literal (literal-expr) ())
(defexpr num-literal (literal-expr) ())
(defexpr char-literal (literal-expr) ())
(defnode proxy () (info subnode))
(defnode empty () ())

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
                              (defmacro ,(intern (format nil "~:@(~a~)" i) (generator-package k)) ,lambda-list
                                ,@body)))))))

;;; Utils
(defmacro make-nodes (nodes &key prepend quoty)
  "Build general or specific AST."
  (let ((prepend (if (listp prepend) prepend `(,prepend))))
    `(nodelist
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
    ((symbolp item) (make-instance 'ident :id item))
    ((numberp item) (make-instance 'num-literal :val item))
    ((stringp item) (make-instance 'str-literal :val item))
    ((characterp item) (make-instance 'char-literal :val item))
    ;; Item is not a known atom
    (t (error "code generator encountered an unknown atom: ~a" item))))

(defclass ast-traverser () ())

(defmethod traverse :before ((self ast-traverser) (item ast) level)
  "remove unecessary trees"
  (declare (ignore level))
  (with-slots (nodes) item
    (loop 
      do (if (not (= (length nodes) 1))
             (loop-finish)
             (cond 
               ((eql (class-of (first nodes)) (find-class 'ast))
                (setf nodes (slot-value nodes 'ast)))
               ((eql (class-of (first nodes)) (find-class 'source-position))
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

(defmacro define-code-printer (qual node &body body)
  (if (eql :self qual)
      `(defmethod traverse ((self code-printer)
                            (node ,node)
                            level)
         (declare (ignorable level))
         ,@body
         (call-next-method))
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
                     (indent (slot-value self 'indent))
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
