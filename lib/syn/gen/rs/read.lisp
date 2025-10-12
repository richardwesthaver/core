;;; read.lisp --- SYN/GEN/RS readers

;; 

;;; Code:
(in-package :syn/gen/rs)

;;; Readers
(define-code-reader
  :file-reader read-rs-file
  :string-reader read-rs-string
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)))

;; Define a start-up function
(define-code-processor gen-rs
  :file-reader   read-rs-file
  :string-reader read-rs-string
  :traverse
  (nested-ast-remover
   else-if-traverser
   if-blocker
   decl-blocker
   renamer))

;;; Switches
(define-code-switch rs-reader-switch
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)))

(define-code-switches
  :code-reader rs-reader
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)
   ;; (set-macro-character #\{ #'left-brace-reader)
   ;; (set-macro-character #\} (get-macro-character #\) nil))
   ;; (set-dispatch-macro-character #\# #\: #'sharp-colon-reader)   
   ))

;;; Methods
(defmethod gen-reader ((self (eql :rs))) (function rs-reader))
(defmethod gen-reader-switch ((self (eql :rs))) (function rs-reader-switch))
(defmethod load-gen ((self (eql :rs))) 
  (init-gen :rs)
  (rs-reader))
(defmethod unload-gen ((self (eql :rs)))
  (init-gen nil)
  (cl-reader))
(defmethod gen-package ((self (eql :rs))) (find-package :syn/gen/rs/sym))
