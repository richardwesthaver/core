;;; alien/tree-sitter/api.lisp --- High-level API for Tree-sitter library

;; High-level Tree-sitter API

;;; Code:
(in-package :tree-sitter)

(defstruct (node (:type list))
  type range children)

(defun make-lisp-name (string)
  (intern (string-upcase (substitute #\- #\_ string))
          (load-time-value (find-package "KEYWORD"))))

(define-condition tree-sitter-error (error)
  ())

(define-condition create-parser-error (tree-sitter-error)
  ())

(define-condition set-language-error (tree-sitter-error)
  ((language :initarg :language :reader tree-sitter-error-language)))

(define-condition parse-string-error (tree-sitter-error)
  ((string :initarg :string :reader tree-sitter-error-string)
   (string-start :initarg :string-start :reader tree-sitter-error-string-start)
   (string-end :initarg :string-end :reader tree-sitter-error-string-end)
   (language :initarg :language :reader tree-sitter-error-language)))

(define-condition null-node (tree-sitter-error)
  ())

(define-condition null-tree-cursor (tree-sitter-error)
  ())

(define-condition tree-sitter-query-error (tree-sitter-error)
  ((offset :initarg :offset :reader tree-sitter-error-offset)
   (type :initarg :type :reader tree-sitter-error-type))
  (:report (lambda (c s)
             (format s "~A of type ~A occurred at offset ~A."
                     (class-name (class-of c))
                     (tree-sitter-error-type c)
                     (tree-sitter-error-offset c)))))

(defun check-ts-query-error (type &optional (offset 0))
  (unless (zerop type) ;; pass
    (error 'tree-sitter-query-error :type (ts-query-error* type) :offset offset)))

;; util
(defmacro with-ts-parser ((sym &key lang) &body body)
  (let ((%lang (when lang (language-module lang))))
    `(let ((,sym (ts-parser-new))
           ,@(if (atom lang) nil `(,(pop lang) ,%lang)))
       ,@(when lang `(ts-parser-set-language ,%lang))
       (unwind-protect (progn ,@body)
         (ts-parser-delete ,sym)))))

(defmacro with-ts-lang (lang sym &body body)
  `(let ((,sym (language-module ,lang)))
     ,@body))

(defmacro with-ts-cursor ((cursor node tree) &body forms)
  `(let* ((,node (ts-tree-root-node ,tree)))
     (with-alien ((,cursor (* ts-tree-cursor) (make-alien ts-tree-cursor))) 
       (setf (deref ,cursor) (ts-tree-cursor-new ,node))
       ,@forms)))

(defmacro with-ts-query (lang (var expr) &body body)
  (with-gensyms (eoff etype exp len)
      `(let* ((,exp (with-output-to-string (s) (write ,expr :stream s :pretty nil :case :downcase)))
              (,len (length ,exp)))
         (with-alien ((,eoff unsigned-int 0)
                      (,etype ts-query-error 0))
           (let ((,var (ts-query-new (language-module ,lang) ,exp ,len
                                     (addr ,eoff) (addr ,etype))))
             (check-ts-query-error ,etype ,eoff)
             ,@body)))))

(defmacro with-ts-query-cursor (var &body body)
  `(let ((,var (ts-query-cursor-new)))
     ,@body))

(defun parse-string (language string &key (start 0) end)
  "Parse a STRING that represents LANGUAGE code using tree-sitter. START is
where to start parsing STRING. END is where to stop parsing STRING.

NAME-GENERATOR is a function which converts a string from tree-sitter into a
desired name for use in lisp."
  (let ((parser (ts-parser-new)))
    (when (sb-alien:null-alien parser)
      (error 'cant-create-parser))
    (unwind-protect (parse-string-with-language language string parser
                                                :start start
                                                :end end)
      (ts-parser-delete parser))))
        

(defun parse-string-with-language (language string parser
                                   &key (start 0) end)
  (unless (ts-parser-set-language parser (language-module language))
    (error 'cant-set-language :language language))
  (let* ((string-start start)
         (string-end (or end (length string)))
         (string-length (- string-end string-start))
         (string-to-pass (if (plusp string-start)
                             (subseq string string-start string-end)
                             string))
         (tree (ts-parser-parse-string parser nil string-to-pass string-length)))
    (when (sb-alien:null-alien tree)
      (error 'cant-parse-string
             :string string
             :string-start start
             :string-end end
             :language language))
    tree))

(defun ts-point-cons (p)
  (unless (sb-alien:null-alien p)
    (with-alien-slots (tree-sitter::row tree-sitter::column) p
      (cons tree-sitter::row tree-sitter::column))))

(defun ts-node-start (node)
  "Return a cons (ROW . COL) indicating the file-position of the start of NODE."
  (sb-alien:with-alien ((p ts-point (ts-node-start-point node)))
    (ts-point-cons p)))

(defun ts-node-end (node)
  "Return a cons (ROW . COL) indicating the file-position of the end of NODE."
  (sb-alien:with-alien ((p ts-point (ts-node-end-point node)))
    (unless (print (sb-alien:null-alien p))
      (with-alien-slots (tree-sitter::row tree-sitter::column) p
        (cons tree-sitter::row tree-sitter::column)))))

(defun convert-ts-tree (tree &key (name-generator #'make-lisp-name)
                             &aux did-visit-children parse-stack)
  (with-ts-cursor (tc node tree)
    ;; Closely follows tree-sitter-cli parse implementation
    ;; (with-ts-node?
    (loop
      (let ((node (ts-tree-cursor-current-node tc)))
        (when (null-alien node) (return parse-stack))
        (let ((is-named (ts-node-is-named node))
              (cursor tc))
          (cond (did-visit-children
                 (when (and is-named (second parse-stack))
                   (let ((item (pop parse-stack)))
                     (setf (node-children item)
                           (nreverse (node-children item)))
                     (push item (node-children (first parse-stack)))))
                 (cond ((ts-tree-cursor-goto-next-sibling cursor)
                        (setf did-visit-children nil))
                       ((ts-tree-cursor-goto-parent cursor)
                        (setf did-visit-children t))
                       (t
                        (let ((root (first parse-stack)))
                          (setf (node-children root)
                                (nreverse (node-children root)))
                          (return root)))))
                (t
                 (when is-named
                   (let ((start-point (ts-node-start-byte node))
                         (end-point (ts-node-end-byte node))
                         (type (funcall name-generator (ts-node-type node)))
                         (field-name (ts-tree-cursor-current-field-name cursor)))
                     (when field-name (setf type (list (funcall name-generator field-name) type)))
                     (push (make-node :type type :range (list start-point end-point))
                           parse-stack)))
                 (setf did-visit-children
                       (not (ts-tree-cursor-goto-first-child cursor))))))))))
