;;; read.lisp --- CUDA Syntax Reader

;; 

;;; Code:
(in-package :syn/gen/cu)

;; Define a c-mera file reader with extra macro characters
(define-code-reader
  :file-reader   read-cu-file
  :string-reader read-cu-string
  :macro-character
  ((set-macro-character #\Space #'syn/gen/cpp::pre-process)
   (set-macro-character #\Tab #'syn/gen/cpp::pre-process)
   (set-macro-character #\Newline #'syn/gen/cpp::pre-process)
   (set-macro-character #\( #'syn/gen/cpp::pre-process-heads)
   (set-macro-character #\{ #'syn/gen/cpp::left-brace-reader)
   (set-macro-character #\} (get-macro-character #\) nil))
   (set-dispatch-macro-character #\# #\: #'syn/gen/cpp::sharp-colon-reader)))

;; Define a start-up function
(define-code-processor gen-cu
  :file-reader   read-cu-file
  :string-reader read-cu-string
  :traverse
  (nested-ast-remover
   else-if-traverser
   if-blocker
   decl-blocker
   renamer))

;; Define a save function
#+nil
(save-generator
 :name save 
 :start-function cuda-processor 
 :in-package :cmu-cuda)

;;; Define a reader switch with c++ pre-processing
(define-code-switch cu-reader-switch
  :macro-character
  ((set-macro-character #\Space #'syn/gen/cpp::pre-process)
   (set-macro-character #\Tab #'syn/gen/cpp::pre-process)
   (set-macro-character #\Newline #'syn/gen/cpp::pre-process)
   (set-macro-character #\( #'syn/gen/cpp::pre-process-heads)
   (set-macro-character #\{ #'syn/gen/cpp::left-brace-reader)
   (set-macro-character #\} (get-macro-character #\) nil))
   (set-dispatch-macro-character #\# #\: #'syn/gen/cpp::sharp-colon-reader)))

(define-code-switches
    :cl-reader cl-reader
    :code-reader cu-reader
  :macro-character
  ((set-macro-character #\Space #'syn/gen/cpp::pre-process)
   (set-macro-character #\Tab #'syn/gen/cpp::pre-process)
   (set-macro-character #\Newline #'syn/gen/cpp::pre-process)
   (set-macro-character #\( #'syn/gen/cpp::pre-process-heads)
   (set-macro-character #\{ #'syn/gen/cpp::left-brace-reader)
   (set-macro-character #\} (get-macro-character #\) nil))
   (set-dispatch-macro-character #\# #\: #'syn/gen/cpp::sharp-colon-reader)))
