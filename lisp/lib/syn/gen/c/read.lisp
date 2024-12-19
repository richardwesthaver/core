;;; read.lisp --- Lisp Readers which return C AST Nodes

;; 

;;; Code:
(in-package :syn/gen/c)

(defun fix-case (parent child)
  "Fix case for dissected symbols.
   Required because of our inverted readtable."
  ;; check every single character's case
  (macrolet ((case-test (test string)
               `(eval `(and ,@(mapcar (lambda(x) (or (not (both-case-p x))
                                                     (,test x)))
                                      (coerce ,string 'list)))))
             (special-case (string)
               `(eval `(and ,@(mapcar (lambda(x) (not (both-case-p x)))
                                      (coerce ,string 'list))))))
    (let
        ;; parent upper
        ((pu (case-test upper-case-p parent))
         ;; parent lower
         (pl (case-test lower-case-p parent))
         ;; child upper
         (cu (case-test upper-case-p child))
         ;; child lower
         (cl (case-test lower-case-p child))
         ;; child special
         (cs (special-case child)))
      ;; adjust cases
      (let ((parent
              ;; fix parent case if root symbol had mixed case
              (cond
                ;; special cases
                ((and pu cs) (intern (string-upcase parent)))
                ((and pl cs) (intern (string-downcase parent)))
                ;; parend upper case
                ;; child lower or mixed case
                ((or (and pu cl) (and pu (not (or cu cl))))
                 (intern (string-downcase parent)))
                ;; parent lower case
                ;; child upper or mixed case
                ((or (and pl cu) (and pl (not (or cu cl))))
                 (intern (string-upcase parent)))
                ;; default
                (t (intern parent))))
            ;; fix child case if root symbol had mixed case
            (child
              (cond
                ;; child lower case
                ;; parent upper or mixed case
                ((or (and pu cl) (and (not (or pu pl)) cl))
                 (intern (string-upcase child)))
                ;; child upper case
                ;; parent loer or mixed case
                ((or (and pl cu) (and (not (or pu pl)) cu))
                 (intern (string-downcase child)))
                ;; default
                (t (intern child)))))
        (list parent child)))))

(defun read-float (item)
  "perace correct float print"
  (let* ((name (symbol-name item))
         (len (length name)))
    ;; Inspired by:  Bozhidar Batsov, batsov.com/articles/2011/04/30/parsing-numbers-from-string-in-lisp
    (with-input-from-string (in (subseq name 0 (- len 1)))
      `(float ,(read in)))))

(defun split-unary (item)
  "prepare ++i or the like to unary node cration: ++i => (prefix i ++)"
  (let* ((name (symbol-name item))
         (len (length name))
         (>2 (> len 2))
         (>1 (> len 1)))
    (if (not >1)
        item
        (let ((pos-inc (equalp (subseq name (- len 2) len) "++"))
              (pos-dec (equalp (subseq name (- len 2) len) "--"))
              (pre-inc (equalp (subseq name 0 2) "++"))
              (pre-dec (equalp (subseq name 0 2) "--"))
              (minus   (equalp (subseq name 0 1) "-"))
              (plus    (equalp (subseq name 0 1) "+"))
              (not     (equalp (subseq name 0 1) "!"))
              (not2    (equalp (subseq name 0 1) "~"))
              (ast     (equalp (subseq name (- len 1) len) "*")))

          (cond
            ((and pos-inc >2) `(postfix++ ,(dissect (intern (subseq name 0 (- len 2))) :quoty t)))
            ((and pos-dec >2) `(postfix-- ,(dissect (intern (subseq name 0 (- len 2))) :quoty t)))
            ((and ast >1)     `(postfix*  ,(dissect (intern (subseq name 0 (- len 1))) :quoty t)))
            ((and pre-inc >2) `(prefix++  ,(dissect (intern (subseq name 2 len)) :quoty t)))
            ((and pre-dec >2) `(prefix--  ,(dissect (intern (subseq name 2 len)) :quoty t)))
            ((and minus >1)   `(-  ,(dissect (intern (subseq name 1 len)) :quoty t)))
            ((and plus >1)    `(+  ,(dissect (intern (subseq name 1 len)) :quoty t)))
            ((and not >1)     `(!  ,(dissect (intern (subseq name 1 len)) :quoty t)))
            ((and not2 >1)    `(~  ,(dissect (intern (subseq name 1 len)) :quoty t)))
            (t item))))))

(defun split-addrof (name)
  "prepare addr-of node: &foo => (addr-of foo)"
  (let ((name (symbol-name name)))
    `(addr-of ,(dissect (intern (subseq name 1 (length name))) :quoty t))))

(defun split-targof (name)
  "prepare targ-of node: *foo => (targ-of foo)"
  (let ((name (symbol-name name)))
    `(targ-of ,(dissect (intern (subseq name 1 (length name))) :quoty t))))

(defun split-oref (name)
  "prepare oref node: foo.baz => (oref foo baz)"
  (let* ((name-string (symbol-name name))
         (pos (search "." name-string :from-end t))
         (names (fix-case (subseq name-string 0 pos) (subseq name-string (+ 1 pos)))))
    `(oref ,(dissect (first names) :quoty t)
                  ,(dissect (second names) :quoty t))))

(defun split-pref (name)
  "prepare pref node: a->b => (pref a b)"
  (let* ((name-string (symbol-name name))
         (pos (search "->" name-string :from-end t))
         (names (fix-case (subseq name-string 0 pos) (subseq name-string (+ 2 pos)))))
    (if (eql pos 0)
        name ;; function definition arrow, dont touch
        `(pref ,(dissect (first names) :quoty t)
                      ,(dissect (second names) :quoty t)))))

(defun split-aref (name)
  "make aref node: a[b][c] => (aref (aref a b) c)"
  (let* ((name-string (symbol-name name))
         (name-list (reverse (coerce name-string 'list))))
    (let ((pos 0)
          (counter 0)
          (names nil))

      ;; get position of matching '[ for last ']
      (loop for i in name-list do
        (progn
          (cond 
            ((eql i #\]) (incf counter))
            ((eql i #\[) (decf counter)))
          (incf pos)
          (when (eql  counter 0)
            (return))))
      (setf pos (- (length name-string) pos))
      (setf names (fix-case (subseq name-string 0 pos)
                            (subseq name-string (1+ pos) (1- (length name-string)))))
      (if (not (equal "" (symbol-name (second names))))
          ;; index not empty
          `(aref ,(dissect (first names) :quoty t)
                        ,(dissect (second names) :quoty t))
          ;; index empty
          `(aref ,(dissect (first names) :quoty t))))))

(defun pre-process (stream char)
  "Pre process symbols in STREAM."
  (declare (ignore char))
  (let ((peek (peek-char nil stream nil nil nil)))
    ;; stop at whitespace and comments
    (if (not (or (eql peek #\))
                 (eql peek #\;)
                 (eql peek #\#)
                 (eql peek #\Space)
                 (eql peek #\Newline)
                 (eql peek #\Tab)))
        (dissect (read stream nil nil nil))
        (values))))

(defun pre-process-heads (stream char)
  "Pre process list heads in STREAM."
  (declare (ignore char))
  (let ((peek (peek-char nil stream nil nil nil))
        (list (read-delimited-list #\) stream t)))
    (let ((first (first list)))
      ;; stop at whitespace and comments
      (if (not (or (eql peek #\()
                   (eql peek #\))
                   (eql peek #\;)
                   (eql peek #\#)
                   (eql peek #\Space)
                   (eql peek #\Newline)
                   (eql peek #\Tab)
                   (and (symbolp first)
                        (std/sym:fboundp! first))))
          (append (list (dissect first)) (rest list))
          list))))

;;; Needs further analysis
;;(defun comment-reader (stream char)
;;  "Rread lisp comments and emmit c-mera comments"
;;  (let ((peek (peek-char nil stream nil nil nil)))
;;    (if (not (eql peek #\;))
;;	`(comment ,(read-string stream #\Newline))
;;	(values))))

(defun dissect (form &key quoty)
  "starts the appropriate preprocessing for the given form"
  (cond
    ((symbolp form)
     (cond
       ((and (eql (first (coerce (symbol-name form) 'list)) #\")
             (eql (first (reverse (coerce (symbol-name form) 'list))) #\"))
        form)
       ((and (eql (first (coerce (symbol-name form) 'list)) #\<)
             (eql (first (reverse (coerce (symbol-name form) 'list))) #\>))
        form)
       ((and (eql (first (coerce (symbol-name form) 'list)) #\*)
             (eql (first (reverse (coerce (symbol-name form) 'list))) #\*))
        form)
       ;; check/(fix package 
       ((or (eql form '&optional)
            (eql form '&key)
            (eql form '&environment)
            (eql form '&body)
            (eql form '&rest))
        form)
       ((and (> (length (symbol-name form)) 1)
             (eql (first (coerce (symbol-name form) 'list)) #\&))
        (split-addrof form))
       ((and (> (length (symbol-name form)) 1)
             (eql (first (coerce (symbol-name form) 'list)) #\*)
             (not (eql (first (reverse (coerce (symbol-name form) 'list))) #\*)))
        (split-targof form))
       (t 
        (let* ((name-string (symbol-name form))
               (num-pos (position-if #'numberp (mapcar #'digit-char-p (coerce name-string 'list))))
               (f-pos (search "F" name-string :from-end t))
               (-pos  (search "-" name-string))
               (dot-pos2 (search "." name-string)) ;hack
               (dot-pos (search "." name-string :from-end t))
               (arrow-pos (search "->" name-string :from-end t))
               (bracket-pos (search "]" name-string :from-end t)))
          (labels ((pos-cond (a b c) (if a (and (if b (> a b) t) (if c (> a c) t)) nil)))
            (cond
              ((and (eql f-pos (- (length name-string) 1)) (or (eql num-pos 0)
                                                               (eql -pos 0)
                                                               (eql dot-pos2 0)))
               (read-float form))
              ((pos-cond dot-pos arrow-pos bracket-pos) (split-oref form))
              ((pos-cond arrow-pos dot-pos bracket-pos) (split-pref form))
              ((pos-cond bracket-pos arrow-pos dot-pos) (split-aref form))
              ((or (search "+" name-string)
                   (search "-" name-string)
                   (search "!" name-string)
                   (search "*" name-string)
                   (search "~" name-string))
               (split-unary form))
              (t (if (not (find-if-not #'digit-char-p (symbol-name form)))
                     (parse-integer (symbol-name form))
                     (if quoty
                         `(quoty ,form)
                         form)))))))))
    (t form)))

;;; Readers
(define-code-reader
  :file-reader read-gen-c-file
  :string-reader read-gen-c-string
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)))

;; Define a start-up function
(define-code-processor c-processor
  :file-reader   read-gen-c-file
  :string-reader read-gen-c-string
  :traverse
  (nested-ast-remover
   else-if-traverser
   if-blocker
   decl-blocker
   renamer))

;;; Switches
(define-code-switch switch-reader
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)))

(define-code-switches
  :cl-reader cl-reader
  :code-reader c-reader
  :macro-character
  ((set-macro-character #\Space #'pre-process)
   (set-macro-character #\Tab #'pre-process)
   (set-macro-character #\Newline #'pre-process)
   (set-macro-character #\( #'pre-process-heads)))

(defmethod gen-reader ((self (eql :c))) (function c-reader))
(defmethod gen-reader-switch ((self (eql :c))) (function switch-reader))

(defmethod load-gen ((self (eql :c))) 
  (init-gen :c)
  (c-reader))

(defmethod unload-gen ((self (eql :c)))
  (init-gen nil)
  (cl-reader))

(defmethod gen-symbol-package ((self (eql :c))) (find-package :syn/gen/c/sym))
