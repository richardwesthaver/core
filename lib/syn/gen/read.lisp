;;; read.lisp --- Codegen Reader Macros

;; 

;;; Code:
(in-package :syn/gen)

(defmacro define-code-reader (&key file-reader string-reader macro-character)
  `(progn
     (defun ,file-reader (file)
       ,(format nil "Read ~X source code file and return AST." (package-name *package*))
       (let ((ast)
             (*readtable* (copy-readtable nil)))
         (setf (readtable-case *readtable*) :invert)
         ,@macro-character
         (with-open-file (f file)
           (loop for form = (read f nil nil nil)
                 while form
                 do
                 (let* ((form (eval form))
                        (evaled (if (consp form)
                                    form
                                    (list form))))
                   (when (typep (car evaled) 'node)
                     (setf ast (append ast evaled))))))
         (make-instance 'ast :ast ast)))
     (defun ,string-reader (str)
       "Read syn/gen source code string and return AST."
       (let ((ast)
             (*readtable* (copy-readtable nil)))
         (setf (readtable-case *readtable*) :invert)
         ,@macro-character
         (let* ((form (eval (read-from-string str)))
                (evaled (if (consp form)
                            form
                            (list form))))
           (when (typep (car evaled) 'node)
             (setf ast evaled)))
         (make-instance 'ast :ast ast)))))

(defmacro define-code-processor (name &key file-reader string-reader traverse)
  (let ((extras (loop for i in traverse collect
                         `(traverse (make-instance ',i) tree 0))))
    `(defun ,name (in &optional out)
       (let ((tree)
             (printer (make-instance 'code-printer))
             (*package* ,*package*))
         (setf tree (if (pathnamep in) (,file-reader in) (,string-reader in)))
         ,@extras
         (if out
             (with-open-file
                 (stream out :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
               (setf (slot-value printer 'stream) stream)
               (traverse printer tree 0))
             (progn
               (setf (slot-value printer 'stream) *standard-output*)
               (traverse printer tree 0)
               (format t "~&")))))))

(defmacro define-code-switch (name &key macro-character)
  "Define a syn/gen reader switch (in repl) allowing preprocessing and mixed
case."
  `(defun ,name ()
     (cond ((eql *code-reader* 'cl)
            (setf *code-reader* 'gen)
            ,@macro-character
            (setf (readtable-case *readtable*) :invert))
           ((eql *code-reader* 'gen)
            (setf *code-reader* 'cl)
            (setf *readtable* *backup-readtable*))
           (t (error "Unknown code reader status: ~A" *code-reader*)))))

(defmacro define-code-switches (&key cl-reader code-reader macro-character)
  "Define syn/gen and common-lisp reader switches."
  `(progn
     ,@(when cl-reader
         `((defun ,cl-reader ()
             (setf *code-reader* 'cl
                   *readtable* *backup-readtable*
                   (readtable-case *readtable*) *print-case*
                   *package* (find-package *default-package*)))))
     (defun ,code-reader ()
       (setf *code-reader* 'gen)
       ,@macro-character
       (setf (readtable-case *readtable*) :invert))))

;;; Context switches
(defun build-context-switches (&key package symbols)
  (let ((lisp-macrolet
         (loop for i in symbols collect
           (let ((symbol (format nil "~a" i)))
             ;; get <package>::<symbol> name
             `(,(intern symbol package) (&rest rest)
               ;; map to cl::<symbol> 
               (list* ',i rest)))))
        (gen-macrolet
         (loop for i in symbols collect
           (let ((symbol (intern (format nil "~a" i) package)))
             ;; get <package>::<symbol> name
             `(,symbol (&rest rest)
               ;; map to its macroexpansion
               ;; -> expansion without local environment
               (macroexpand-1 `(,',symbol ,@rest))))))
        (lisp-switch (intern "LISP" package))
        (gen-switch (intern "GEN" package)))
    (eval
     `(progn
        ;; define macro package::lisp
        ;; use common-lisp functions for macrolet scope
        (defmacro ,lisp-switch (&body body)
          `(macrolet ,',lisp-macrolet ,@body))
        ;; define macro package::gen
        ;; use syn/gen functions for macrolet scope
        ;; used to switch back within lisp-scope
        (defmacro ,gen-switch (&body body)
          `(macrolet ,',gen-macrolet (progn ,@body)))))))

(defun build-swap-package (&key package swap-package symbols)
  (eval
   `(progn
      ,@(loop for i in symbols collect
          (let ((gen-symbol (intern (format nil "~a" i) package))
                (sw-symbol (intern (format nil "~a" i) swap-package)))
            `(defmacro ,sw-symbol (&rest rest)
               (macroexpand-1 `(,',gen-symbol ,@rest))))))))
