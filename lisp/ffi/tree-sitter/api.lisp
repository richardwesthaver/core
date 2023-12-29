;;; ffi/tree-sitter/api.lisp --- High-level API for Tree-sitter library

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

(define-condition null-node-pointer (tree-sitter-error)
  ())

(define-condition null-tree-cursor-pointer (tree-sitter-error)
  ())

;; util
(defmacro with-ts-node ((var node) &body forms)
  `(let ((,var ,node))
     (when (sb-alien:null-alien ,var)
       (error 'null-node-pointer))
     (unwind-protect
          (progn ,@forms)
       (sb-alien:free-alien ,var))))

(defmacro with-tree-cursor ((var tree) &body forms &aux (node (gensym)))
  `(with-ts-node (,node (ts-tree-root-node ,tree))
     (let ((,var (ts-tree-cursor-new ,node)))
       (when (sb-alien:null-alien ,var)
         (error 'null-tree-cursor-pointer))
       (unwind-protect
            (progn ,@forms)
         (ts-tree-cursor-delete ,var)))))

(defun parse-string (language string &key (start 0) end produce-cst (name-generator #'make-lisp-name))
  "Parse a STRING that represents LANGUAGE code using tree-sitter. START is
where to start parsing STRING. END is where to stop parsing STRING.
When PRODUCE-CST is set, the full concrete syntax tree will be produced as
opposed to the abstract syntax tree. See 'Named vs Anonymous Nodes':
http://tree-sitter.github.io/tree-sitter/using-parsers#named-vs-anonymous-nodes
NAME-GENERATOR is a function which converts a string from tree-sitter into a
desired name for use in lisp."
  (let ((parser (ts-parser-new)))
    (when (sb-alien:null-alien parser)
      (error 'cant-create-parser))
    (unwind-protect (parse-string-with-language language string parser
                                                :start start
                                                :end end
                                                :produce-cst produce-cst
                                                :name-generator name-generator)
      (ts-parser-delete parser))))

(defun parse-string-with-language (language string parser
                                   &key (start 0) end produce-cst name-generator)
  (unless (ts-parser-set-language parser (language-module language))
    (error 'cant-set-language :language language))
  (let* ((string-start start)
         (string-end (or end (length string)))
         ;; TODO: this might need to be +1 if it's actually a c-string for null
         (string-length (- string-end string-start))
         (string-to-pass (if (plusp string-start)
                             (subseq string string-start string-end)
                             string))
         (tree (ts-parser-parse-string parser string-to-pass string-length)))
    (when (sb-alien:null-alien tree)
      (error 'cant-parse-string
             :string string
             :string-start start
             :string-end end
             :language language))
    (unwind-protect (convert-foreign-tree-to-list tree :produce-cst produce-cst
                                                       :name-generator name-generator)
      (ts-tree-delete tree))))

(defun convert-foreign-tree-to-list (tree &key produce-cst name-generator
                                     &aux did-visit-children parse-stack)
  (with-tree-cursor (cursor tree)
    ;; Closely follows tree-sitter-cli parse
    ;; implementation with a modification to
    ;; allow for production of the full CST.
    (loop
      (with-ts-node (node (ts-tree-cursor-current-node cursor))
        (let ((is-named (or produce-cst (ts-node-is-named node))))
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
                   (let ((start-point (ts-node-start-point node))
                         (end-point (ts-node-end-point node))
                         (type (funcall name-generator (ts-node-type node)))
                         (field-name-ptr (ts-tree-cursor-current-field-name cursor)))
                     (unless (sb-alien:null-alien field-name-ptr)
                       ;; TODO
                       (let ((field-name (deref field-name-ptr)))
                         (setf type (list (funcall name-generator field-name) type))))
                     (push (make-node :type type
                                      :range (list (list (second start-point) (fourth start-point))
                                                   (list (second end-point) (fourth end-point))))
                           parse-stack)))
                 (setf did-visit-children
                       (not (ts-tree-cursor-goto-first-child cursor))))))))))
