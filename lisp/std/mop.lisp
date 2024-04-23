;;; std/mop.lisp --- Standard MOP

;;

;;; Code:
(in-package :std/mop)

;; make-specializer-form-using-class
;; make-method-lambda-using-specializers

(defun list-indirect-class-methods (class)
  "List all indirect methods of CLASS."
  (remove-duplicates (mapcan #'specializer-direct-generic-functions (compute-class-precedence-list class))))

(defun list-class-methods (class methods &optional indirect)
  "List all methods specializing on CLASS modulo METHODS. When INDIRECT is
non-nil, also include indirect (parent) methods."
  (if (eq methods t)
      (if indirect
          (list-indirect-class-methods class)
          (specializer-direct-generic-functions class))
      (mapcar
       (lambda (s)
         (car (member s (specializer-direct-generic-functions class) :key #'generic-function-name)))
       methods)))

;; FIX 2023-09-13: need exclude param
(defun list-class-slots (class slots &optional exclude)
  ;; should probably convert slot-definition-name here
  (let ((cs (remove-if
             (lambda (s)
               (or
                (null s)
                (member t (mapcar
                           (lambda (x)
                             (string= (slot-definition-name s) x))
                           exclude))))
             (class-slots class))))
    (if (eq slots t)
        cs
        (loop for s in slots
              with sn = (symb s)
              for c in cs
              with cn = (symb (slot-definition-name c))
              when (eq sn cn)
                collect c))))

;; TODO 2023-09-09: slot exclusion from dynamic var
(defun list-slot-values-using-class (class obj slots &optional nullp unboundp)
  (remove-if
   #'null
   (mapcar
    (lambda (s)
      (let ((n (slot-definition-name s)))
        (let ((ns (make-keyword (symbol-name n))))
          (if (slot-boundp-using-class class obj s)
              (let ((v (slot-value-using-class class obj s)))
                (if nullp
                    `(,ns ,v)
                    (unless (null v)
                      `(,ns ,v))))
              (when unboundp (list ns))))))
    slots)))
