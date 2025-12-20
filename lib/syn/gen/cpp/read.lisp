;;; read.lisp --- CPP Reader

;; 

;;; Code:
(in-package :syn/gen/cpp)

(defun split-reference (item)
  (let* ((name (symbol-name item))
         (len (length name)))
    `(syn/gen/cpp/sym::reference-type ,(dissect (intern (subseq name 0 (- len 1))) :quoty t))))

(defun dissect (item &key (quoty nil))
  "extended c pre processor"
  (if (symbolp item)
      (cond ((or (eql item 'syn/gen/cpp/sym::new[])
                 (eql item 'syn/gen/cpp/sym::delete[])
                 (eql item 'syn/gen/cpp/sym::operator[]))
             item)
            ((and (> (length (symbol-name item)) 1)
                  (not (eql (first (coerce (symbol-name item) 'list)) #\&))
                  (eql (first (reverse (coerce (symbol-name item) 'list))) #\&))
             (split-reference item))
            (t (syn/gen/c::dissect item :quoty quoty)))
      (syn/gen/c::dissect item :quoty quoty)))

;; copy of c reader
(defun pre-process (stream char)
  "Pre process symbols in stream and prepare actual node"
  (declare (ignore char))
  (let ((peek (peek-char nil stream nil nil nil)))
    ;; skip multiple whitespace and comments
    (if (not (or (eql peek #\))
                 (eql peek #\;)
                 (eql peek #\#)
                 (eql peek #\})
                 (eql peek #\{)
                 (eql peek #\Space)
                 (eql peek #\Newline)
                 (eql peek #\Tab)))
        (dissect (read stream nil nil nil))
        (values))))

;; copy of c reader
(defun pre-process-heads (stream char)
  "Pre process list heads and prepare nodes"
  (declare (ignore char))
  (let ((peek (peek-char nil stream nil nil nil))
        (list (read-delimited-list #\) stream t)))
    (let ((first (first list)))
      ;; stop at whitespace and comments
      (if (not (or (eql peek #\()
                   (eql peek #\))
                   (eql peek #\})
                   (eql peek #\{)
                   (eql peek #\;)
                   (eql peek #\#)
                   (eql peek #\Space)
                   (eql peek #\Newline)
                   (eql peek #\Tab)
                   (and (symbolp first)
                        (std::fboundp! first))))
          (append (list (dissect first)) (rest list))
          list))))


(defun sharp-colon-reader (stream c1 c2)
  (declare (ignore c1 c2))
  (flet ((valid-id-char (c)
           (not (or (char= #\( c)
              (char= #\) c)
              (char= #\} c)
              (char= #\{ c)
              (char= #\; c)
              (char= #\Space c)
              (char= #\Newline c)
              (char= #\Tab c)))))
    ;; accumulation target
    (let ((str (make-array 0 :element-type 'character
                             :fill-pointer 0
                             :adjustable t)))
      ;; read char-by-char, unread terminating char
      (loop for c = (read-char-no-hang stream)
            then    (read-char-no-hang stream)
            while   (valid-id-char c)
            do      (vector-push-extend c str)
            finally (unread-char c stream))
      ;; build fn-form by parsing the read string
      (let* ((raw-items 
               ;; collect namespaces and skip ":" and "::"
               (loop for s in (loop for i = 0 then (1+ j)
                                    as j = (position #\: str :start i)
                                    collect (subseq str i j)
                                    while j) 
                     if (string/= s "")
                     collect (dissect (cintern s) :quoty t)))
             ;; add a 'nil' if no namespace defined -> global scope
             (fixed-items (if (second raw-items)
                               raw-items
                               `(nil ,@raw-items))))
        `(syn/gen/cpp/sym::from-namespace ,@fixed-items)))))

(defun left-brace-reader (stream char)
  "Read cxx initializer list '{...}' and emit double list '((...))'"
  (declare (ignore char))
  (let ((init-list (read-delimited-list #\} stream t)))
    (let ((first (car init-list))
          (rest  (rest init-list)))
      (list (list (append (list (dissect first)) rest))))))

;; Define a c-mera file reader with extra macro characters
(define-code-reader
  :file-reader   read-cpp-file
  :string-reader read-cpp-string
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)
   (set-macro-character #\{ #'left-brace-reader)
   (set-macro-character #\} (get-macro-character #\) nil))
   (set-dispatch-macro-character #\# #\: #'sharp-colon-reader)))

;; Define a start-up function
(define-code-processor cpp-processor
  :file-reader   read-cpp-file
  :string-reader read-cpp-string
  :traverse
  (nested-ast-remover
   else-if-traverser
   if-blocker
   decl-blocker
   renamer
   virtualizer
   access-respecifier
   ))

;; Define a save function
;; (save-generator
;;  :name save 
;;  :start-function cxx-processor 
;;  :in-package :cmu-c++)

;;; Define a reader switch with c++ pre-processing
(define-code-switch switch-reader
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)
   (set-macro-character #\{ #'left-brace-reader)
   (set-macro-character #\} (get-macro-character #\) nil))
   (set-dispatch-macro-character #\# #\: #'sharp-colon-reader)))

(define-code-switches
    :cl-reader cl-reader
    :code-reader cpp-reader
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)
   (set-macro-character #\{ #'left-brace-reader)
   (set-macro-character #\} (get-macro-character #\) nil))
   (set-dispatch-macro-character #\# #\: #'sharp-colon-reader)))
