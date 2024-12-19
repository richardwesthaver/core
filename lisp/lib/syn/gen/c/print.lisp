;;; print.lisp --- SYN/GEN/C Code Printer

;; TODO

;;; Code:
(in-package :syn/gen/c)

;;; Simply prints the ast, useful in REPL.
(defun simple-print (tree)
  "Pretty prints C ast"
  (let (
        ;(nc (make-instance 'nodelist-traverser))
        (ei (make-instance 'else-if-traverser))
        (ib (make-instance 'if-blocker))
        (db (make-instance 'decl-blocker))
        (rn (make-instance 'renamer))
        (pp (make-instance 'code-printer)))
    (progn
      ;(traverser nc tree 0)
      (traverse ei tree 0)
      (traverse ib tree 0)
      (traverse db tree 0)
      (traverse rn tree 0)
      (traverse pp tree 0))))

(with-code-printer
  (define-code-printer :before expression-statement
    (push-info 'expression-statement)
    (when (or (typep (node-slot expression) 'function-call)
              (typep (node-slot expression) 'infix-expression)
              (typep (node-slot expression) 'prefix-expression)
              (typep (node-slot expression) 'postfix-expression))
      (format stream "~&~a" indent)))
  (define-code-printer :after expression-statement
    (pop-info)
    (when (or (typep (node-slot expression) 'function-call)
              (typep (node-slot expression) 'infix-expression)
              (typep (node-slot expression) 'prefix-expression)
              (typep (node-slot expression) 'postfix-expression)
              (typep (node-slot expression) 'empty)
              (node-slot force-semicolon))
      (format stream ";")))
  ;; Compound-Statement
  (define-code-printer :before compound-statement
    ;; Begin new line (if not in 'for', 'while', or 'if' statement).
    ;; Open and close brackets if needed (managed with a traverser).
    (if (node-slot braces)
        (progn
          ;; do not start new line for these cases
          (if (or (eql (top-info) 'for)
                  (eql (top-info) 'while)
                  (eql (top-info) 'do)
                  (eql (top-info) 'if)
                  (eql (top-info) 'else))
              ;; simply append brace
              (format stream " {~%")
              ;; start new line+indent+brace
              (format stream "~&~a{~%" indent))
          ;; add info for following subnodes
          (push-info 'block)))
    ;; increase indent
    ++indent)
  (define-code-printer :self compound-statement
    (traverse %self (node-slot statements) %level))
  (define-code-printer :after compound-statement
    --indent
    (pop-info)
    (if (node-slot braces)
        (progn (format stream "~&~a}" indent))))
  (define-code-printer :after c-type
    (let ((info (top-info)))
      (when (and (node-slot type)
                 (not (eql info 'cast-expression))
                 (not (eql info 'funcall))
                 (not (eql info 'declaration-item)))
        (format stream " "))))
  (define-code-printer :self c-type
    (traverse %self (ast node) %level))
  (define-code-printer :before function-definition
    (push-info 'function-definition)
    (format stream "~&~%~A" indent))
  (define-code-printer :self function-definition
    (with-slots (parameter item body) node
      (format stream "~A ~A"
              (val
               (slot-value
                (slot-value item 'type)
                'type))
              (val (slot-value item 'identifier)))
      (format stream "(~{~A~^, ~})"
              (mapcar (lambda (x) (format nil "~A ~A" 
                                          (val
                                           (slot-value
                                            (slot-value x 'type)
                                            'type))
                                          (val (slot-value x 'identifier))))
                      (ast parameter)))
      (traverse %self body %level)))
  (define-code-printer :after function-definition
    (pop-info)
    (when (not (node-slot body))
      (format stream ";")))
  (define-code-printer :before parameter-list
    (unless (ast node)
      (format stream " void"))
    (push-sign 'skip-first))
  (define-code-printer :self parameter-list
    (format stream "~{~A~^, ~}" (ast node)))
  (define-code-printer :after parameter-list
    (when (eql (top-sign) 'skip-first)
      (pop-sign)))
  ;; (define-code-printer :before parameter
  ;;   (if (eql (top-sign) 'skip-first)
  ;;       (pop-sign)
  ;;       (format stream ", ")))
  (define-code-printer :before struct-definition
    (format stream "~&~%~Astruct " indent))
  (define-code-printer :after struct-definition
    (format stream ";"))
  (define-code-printer :before enum-definition
    (if (or (eql (top-info) 'typedef)
            (eql (top-info) 'decl))
        (format stream "enum")
        (format stream "~&~Aenum" indent))
    (when (and (not (eql (top-info) 'decl))
               (> (length (slot-value (node-slot members) 'ast)) 3))
      (push-sign 'enum-break))
    (push-info 'enum-definition)
    ++indent)
  (define-code-printer :self enum-definition
    (format stream " {")
    (let ((lprinter (copy-object %self)))
      (setf (slot-value lprinter 'stream) nil)
      (format stream "~{~#[~;~A~:;~A,~]~}"
              (loop for x in (ast (node-slot members))
                    collect 
                       (format nil "~A~A~@[=~A~]"
                               (if (eql (top-sign) 'enum-break)
                                   (format nil "~%~A" indent)
                                   "")
                               (val (slot-value x 'identifier))
                               (std:when-let ((val (slot-value x 'value)))
                                 (traverse lprinter (val val) %level))))))
    --indent
    (if (eql (top-sign) 'enum-break)
        (progn
          (pop-sign)
          (format stream "~&}"))
        (format stream "}"))
    (std:when-let ((id (and (id node) (val (id node)))))
      (format stream " ~A" id)))
  (define-code-printer :after enum-definition
    (pop-info)
    (when (not (or (eql (top-info) 'typedef)
                   (eql (top-info) 'decl)))
      (format stream ";")))
  ;; (define-code-printer :before enum
  ;;   (if (eql (top-sign) 'first-enum)
  ;;       (progn
  ;;         (format stream " {")
  ;;         (pop-sign))
  ;;       (format stream ","))
  ;;   (if (eql (top-sign) 'enum-break)
  ;;       (format stream "~&~A" indent)
  ;;       (format stream " ")))
  (define-code-printer :before declaration-list
    (push-info 'decl)
    (if (node-slot braces)
        (progn
          (format stream "~&~A{" indent)
          ++indent)))
  (define-code-printer :self declaration-list
    (traverse %self (ast node) %level))
  (define-code-printer :after declaration-list
    (pop-info)
    (when (node-slot braces)
      --indent
      (format stream "~&~A}" indent)))
  (define-code-printer :before declaration-item
    (push-info 'declaration-item))
  (define-code-printer :self declaration-item
    (format stream "~A ~A~@[=~A~];~%"
            (val (slot-value (node-slot type) 'type))
            (val (node-slot identifier))
            (std:when-let ((val (node-slot value)))
              (val (val val)))))
  (define-code-printer :after declaration-item
    (pop-info))
  (define-code-printer :self declaration-value
    (traverse %self (ast node) %level))
  ;; TODO 2024-12-15: 
  (define-code-printer :before for-statement
    (push-info 'for))
  (define-code-printer :after for-statement
    (pop-info))
  ;; init test step

  ;; while - test
  ;; do
  ;; if
  ;; switch case
  ;; infix
  (define-code-printer :before infix-expression
    (if (or (eql (top-info) 'infix)
            (eql (top-info) 'oref)
            (eql (top-info) 'not)
            (eql (top-info) 'cast-expression)
            (eql (top-info) 'prefix))
        (format stream "("))
    (push-info 'infix)
    (cond ((eql (node-slot op) 'or)
           (push-sign '\|\|))
          ((eql (node-slot op) 'and)
           (push-sign '&&))
          (t (push-sign (node-slot op))))
    (push-sign 'skip-first))
  (define-code-printer :after infix-expression
    (pop-info)
    (pop-sign)
    (if (or (eql (top-info) 'infix)
            (eql (top-info) 'oref)
            (eql (top-info) 'not)
            (eql (top-info) 'cast-expression)
            (eql (top-info) 'prefix))
        (format stream ")")))
  ;; assignment
  (define-code-printer :before assignment-expression
    (cond
      ((eql (top-info) 'infix)
       (format stream "("))
      ((eql (top-info) 'expression-statement)
       (format stream "~&~A" indent)))
    (push-info 'assignment)
    (push-sign (node-slot op)))
  (define-code-printer :after assignment-expression
    (pop-info)
    (pop-sign)
    (cond
      ((eql (top-info) 'infix)
       (format stream ")"))
      ((eql (top-info) 'expression-statement)
       (format stream ";"))))
  ;; conditional
  (define-code-printer :before conditional-expression
    (when (eql (top-info) 'infix)
      (format stream "("))
    (push-info 'conditional))
  (define-code-printer :after conditional-expression
    (pop-info)
    (when (eql (top-info) 'infix)
      (format stream ")")))
  ;; jump
  (define-code-printer :before jump-statement
    (format stream "~&~A" indent)
    (push-info 'jump-statement))
  (define-code-printer :after jump-statement
    (format stream ";")
    (pop-info))
  ;; label
  (define-code-printer :before label-statement
    (format stream "~&"))
  (define-code-printer :after label-statement
    (format stream ":~%"))
  ;; specifier
  (define-code-printer :self specifier
    (traverse %self (ast node) %level))
  ;; float
  (define-code-printer :after float-type
    (format stream "f"))
  ;; pointer-ref
  ;; object-ref
  (define-code-printer :before object-reference
    (push-info 'oref))
  (define-code-printer :after object-reference
    (pop-info))
  ;; function-pointer
  (define-code-printer :before function-pointer
    (push-info 'function-pointer))
  (define-code-printer :after function-pointer
    (pop-info))
  ;; arrays
  (define-code-printer :before array-reference
    (push-info 'aref))
  (define-code-printer :after array-reference
    (pop-info))
  (define-code-printer :before clist
    (push-info 'skip-first)
    (format stream "{ "))
  (define-code-printer :after clist
    (format stream " }"))
  (define-code-printer :before prefix-expression
    (when (eql (top-info) 'aref)
      (format stream "("))
    (push-info 'prefix)
    (format stream "~A" (node-slot op)))
  (define-code-printer :after postfix-expression
    (format stream "~A" (node-slot op)))
  (define-code-printer :before not-expression
    (push-info 'not)
    (format stream "(not "))
  (define-code-printer :after not-expression
    (pop-info)
    (format stream ")"))
  ;; function
  (define-code-printer :before function-call
    (when (eql (info-size) 0)
      (format stream "~&"))
    (push-sign 'nested-funcall-sentinel)
    (push-sign 'skip-first-funcall))
  (define-code-printer :self function-call
    (format stream "~&")
    (traverse %self (node-slot function) %level)
    (let ((arg-printer (std:copy-object %self)))
      (setf (slot-value arg-printer 'stream) nil)
      (format stream "(~{~A~^, ~})"
              (traverse arg-printer (node-slot syn/gen::arguments) %level))))
  (define-code-printer :after function-call
    (when (eql (top-sign) 'skip-first-funcall)
      (pop-sign))
    (if (eql (top-sign) 'nested-funcall-sentinel)
        (pop-sign)
        (warn "funcall top-sign missmatch"))
    (unless (eql (info-size) 0)
      (format stream ";")))
  ;; include
  (define-code-printer :self include
    (format stream (if (stringp (node-slot file))
                       "~&#include \"~A\""
                       "~&#include ~A")
            (node-slot file)))
  ;; cast
  (define-code-printer :before cast-expression
    (push-info 'cast-expression)
    (format stream "("))
  (define-code-printer :after cast-expression
    (pop-info)
    (format stream ")"))
  ;; typedef
  (define-code-printer :before typedef
    (push-info 'typedef)
    (format stream "~&~Atypedef " indent))
  (define-code-printer :self typedef
    (traverse %self (slot-value (node-slot declaration) 'identifier) %level)
    (format stream " ")
    (traverse %self (slot-value (slot-value (node-slot declaration) 'type) 'type) %level))
  (define-code-printer :after typedef
    (pop-info)
    (format stream ";"))
  ;; comment
  (define-code-printer :self comment
    (when (node-slot linebreak)
      (format stream "~&~A" indent))
    (format stream "~A" (node-slot chars))
    (format stream "~A" (node-slot comment)))
  ;; attr
  (define-code-printer :before attribute-expression
    (push-sign 'skip-first-attribute)
    (format stream "__attribute__ (("))
  (define-code-printer :after attribute-expression
    (when (eql (top-sign) 'skip-first-attribute)
      (pop-sign))
    (format stream "))")))
