;;; macs.lisp --- HTML macros

;; WITH-HTML and friends

;;; Commentary:

;; see https://github.com/edicl/cl-who

;; also SPINNERET

;;; Code:
(in-package :dat/html)

;; (describe
;;  (dat/html:make-element (dat/html:make-document) "foo" nil))
;; (describe (dat/html:make-fragment (dat/html:make-document)))
(define-condition html-condition () ())
(deferror html-error (html-condition) ())

(defclass html-output-stream (wrapped-stream) ()
  (:default-initargs :stream (make-synonym-stream '*standard-output*)))

(defvar *html-output* (make-instance 'html-output-stream))
(defvar *html-lang* "en")
(defvar *html-charset* (string *default-encoding*))
(defvar *html-path* nil)
(defvar *html-prologue* "<!DOCTYPE html>"
  "A string which is printed as the first line of output when :PROLOGUE is T.")

(declaim ((or null (integer 0 8)) *html-indent*))
(defvar *html-indent* nil
  "Whether to insert line breaks and indent - when non-nil the value is assumed
to be a positive integer indicating the number of whitespace chars to insert
for indentation.")

(defvar *empty-attribute-syntax* nil
  "Set this to t to enable attribute minimization (also called
'boolean attributes', or 'empty attribute syntax' according to the w3
html standard). In XHTML attribute minimization is forbidden, and all
attributes must have a value. Thus in XHTML boolean attributes must be
defined as <input disabled='disabled' />. In HTML5 boolean attributes
can be defined as <input disabled>")

(defvar *html-mode* :xml
  ":SGML for \(SGML-)HTML, :XML \(default) for XHTML, :HTML5 for HTML5.")

(defvar *downcase-tokens-p* t
  "If NIL, a keyword symbol representing a tag or attribute name will
not be automatically converted to lowercase.  If T, the tag and
attribute name will be converted to lowercase only if it is in the
same case. This is useful when one needs to output case sensitive
XML.")

(defvar *attribute-quote-char* #\'
  "Quote character for attributes.")

(defvar *empty-tag-end* " />"
  "End of an empty tag.  Default is XML style.")

(defvar *html-no-indent-tags*
  '(:pre :textarea)
  "The list of HTML tags that should disable indentation inside them. The initial
value is a list containing only :PRE and :TEXTAREA.")

(defvar *html-empty-tags*
  '(:area
    :atop
    :audioscope
    :base
    :basefont
    :br
    :choose
    :col
    :command
    :embed
    :frame
    :hr
    :img
    :input
    :isindex
    :keygen
    :left
    :limittext
    :link
    :meta
    :nextid
    :of
    :over
    :param
    :range
    :right
    :source
    :spacer
    :spot
    :tab
    :track
    :wbr)
  "The list of HTML tags that should be output as empty tags.
See *HTML-EMPTY-TAG-AWARE-P*.")

(defvar *html-empty-tag-aware-p* t
  "Set this to NIL to if you want to use CL-WHO as a strict XML
generator.  Otherwise, CL-WHO will only write empty tags listed
in *HTML-EMPTY-TAGS* as <tag/> \(XHTML mode) or <tag> \(SGML
mode and HTML5 mode).  For all other tags, it will always generate
<tag></tag>.")

(defun n-spaces (n)
  "A string with N spaces - used by indentation."
  (make-array n :element-type 'base-char))

(defun escape-char-p (c)
  (or (some (lambda (x) (char= c x)) "<>&'\"")))

(declaim (inline escape-char))
(defun escape-char (char &key (test #'escape-char-p))
  (declare (optimize speed) (function test))
  "Returns an escaped version of the character CHAR if CHAR satisfies
the predicate TEST.  Always returns a string."
  (if (funcall test char)
    (case char
      (#\< "&lt;")
      (#\> "&gt;")
      (#\& "&amp;")
      (#\' "&#039;")
      (#\" "&quot;")
      (t (format nil (if (eq *html-mode* :xml) "&#x~x;" "&#~d;")
                 (char-code char))))
    (make-string 1 :initial-element char)))

(defun escape-string (string &key (test #'escape-char-p))
  (declare (optimize speed))
  "Escape all characters in STRING which pass TEST. This function is
not guaranteed to return a fresh string.  Note that you can pass NIL
for STRING which'll just be returned."
  (let ((first-pos (position-if test string))
        (format-string (if (eq *html-mode* :xml) "&#x~x;" "&#~d;")))
    (if (not first-pos)
      ;; nothing to do, just return STRING
      string
      (with-output-to-string (s)
        (loop with len = (length string)
              for old-pos = 0 then (1+ pos)
              for pos = first-pos
                  then (position-if test string :start old-pos)
              ;; now the characters from OLD-POS to (excluding) POS
              ;; don't have to be escaped while the next character has to
              for char = (and pos (char string pos))
              while pos
              do (write-sequence string s :start old-pos :end pos)
                 (case char
                   ((#\<)
                     (write-sequence "&lt;" s))
                   ((#\>)
                     (write-sequence "&gt;" s))
                   ((#\&)
                     (write-sequence "&amp;" s))
                   ((#\')
                     (write-sequence "&#039;" s))
                   ((#\")
                     (write-sequence "&quot;" s))
                   (otherwise
                     (format s format-string (char-code char))))
              while (< (1+ pos) len)
              finally (unless pos
                        (write-sequence string s :start old-pos)))))))

(defun minimal-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL functions to determine
whether CHAR must be escaped."
  (find char "<>&"))

(defun escape-char-minimal (char)
  "Escapes only #\<, #\>, and #\& characters."
  (escape-char char :test #'minimal-escape-char-p))

(defun escape-string-minimal (string)
  "Escapes only #\<, #\>, and #\& in STRING."
  (escape-string string :test #'minimal-escape-char-p))

(defun minimal-plus-quotes-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-MINIMAL-PLUS-QUOTES functions to
determine whether CHAR must be escaped."
  (find char "<>&'\""))

(defun escape-char-minimal-plus-quotes (char)
  "Like ESCAPE-CHAR-MINIMAL but also escapes quotes."
  (escape-char char :test #'minimal-plus-quotes-escape-char-p))

(defun escape-string-minimal-plus-quotes (string)
  "Like ESCAPE-STRING-MINIMAL but also escapes quotes."
  (escape-string string :test #'minimal-plus-quotes-escape-char-p))

(defun iso-8859-1-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (find char "<>&'\"")
      (> (char-code char) 255)))

(defun escape-char-iso-8859-1 (char)
  "Escapes characters that aren't defined in ISO-8859-9."
  (escape-char char :test #'iso-8859-1-escape-char-p))

(defun escape-string-iso-8859-1 (string)
  "Escapes all characters in STRING which aren't defined in ISO-8859-1."
  (escape-string string :test #'iso-8859-1-escape-char-p))

(defun non-7bit-ascii-escape-char-p (char)
  "Helper function for the ESCAPE-FOO-ISO-8859-1 functions to
determine whether CHAR must be escaped."
  (or (find char "<>&'\"")
      (> (char-code char) 127)))

(defun escape-char-all (char)
  "Escapes characters which aren't in the 7-bit ASCII character set."
  (escape-char char :test #'non-7bit-ascii-escape-char-p))

(defun escape-string-all (string)
  "Escapes all characters in STRING which aren't in the 7-bit ASCII
character set."
  (escape-string string :test #'non-7bit-ascii-escape-char-p))

(defun extract-declarations (forms)
  "Given a FORM, the declarations - if any - will be extracted
   from the head of the FORM, and will return two values the declarations,
   and the remaining of FORM"
  (loop with declarations
        for forms on forms
        for form = (first forms)
        while (and (consp form)
                   (eql (first form) 'cl:declare))
        do (push form declarations)
        finally (return (values (nreverse declarations) forms))))

(defun same-case-p (string)
  "Test if all characters of a string are in the same case."
  (or (every #'(lambda (c) (or (not (alpha-char-p c)) (lower-case-p c))) string)
      (every #'(lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) string)))

(defun maybe-downcase (symbol)
  (let ((string (string symbol)))
    (if (and *downcase-tokens-p* (same-case-p string))
        (string-downcase string)
        string)))

(defun html-mode ()
  "Return the current value of *HTML-MODE*."
  *html-mode*)

(defun (setf html-mode) (mode)
  "Set the *HTML-MODE* and corresponding special variables."
  (ecase mode
    ((:sgml)
     (setf *html-mode* :sgml
           *empty-attribute-syntax* t
           *empty-tag-end* ">"
           *html-prologue* "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">"))
    ((:xml)
     (setf *html-mode* :xml
           *empty-attribute-syntax* nil
           *empty-tag-end* " />"
           *html-prologue* "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))
    ((:html5)
     (setf *html-mode* :html5
           *empty-attribute-syntax* t
           *empty-tag-end* ">"
           *html-prologue* "<!DOCTYPE html>"))))

(defun process-tag (sexp body-fn)
  (declare (optimize speed space))
  "Returns a string list corresponding to the `HTML' \(in CL-WHO
syntax) in SEXP.  Uses the generic function CONVERT-TO-STRING-LIST
internally.  Utility function used by TREE-TO-TEMPLATE."
  (let (tag attr-list body)
    (cond
      ((keywordp sexp)
       (setq tag sexp))
      ((atom (first sexp))
       (setq tag (first sexp))
       ;; collect attribute/value pairs into ATTR-LIST and tag body (if
       ;; any) into BODY
       (loop for rest on (cdr sexp) by #'cddr
             if (keywordp (first rest))
               collect (cons (first rest) (second rest)) into attr
             else
               do (progn (setq attr-list attr)
                         (setq body rest)
                         (return))
             finally (setq attr-list attr)))
      ((listp (first sexp))
       (setq tag (first (first sexp)))
       (loop for rest on (cdr (first sexp)) by #'cddr
             if (keywordp (first rest))
               collect (cons (first rest) (second rest)) into attr
             finally (setq attr-list attr))
       (setq body (cdr sexp))))
    (convert-tag-to-string-list tag attr-list body body-fn)))

(defun convert-attributes (attr-list)
  "Helper function for CONVERT-TAG-TO-STRING-LIST which converts the
alist ATTR-LIST of attributes into a list of strings and/or Lisp
forms."
  (declare (optimize speed space))
  (loop with =var= = (gensym)
        for (orig-attr . val) in attr-list
        for attr = (maybe-downcase orig-attr)
        unless (null val) ;; no attribute at all if VAL is NIL
          if (constantp val)
            if (and *empty-attribute-syntax* (eq val t)) ; special case for SGML and HTML5
              nconc (list " " attr)
            else
              nconc (list " "
                          ;; name of attribute
                          attr
                          (format nil "=~C" *attribute-quote-char*)
                          ;; value of attribute
                          (cond ((eq val t)
                                 ;; VAL is T, use attribute's name
                                 attr)
                                (t
                                 ;; constant form, PRINC it -
                                 ;; EVAL is OK here because of CONSTANTP
                                 (format nil "~A" (eval val))))
                          (string *attribute-quote-char*))
            end
          else
            ;; do the same things as above but at runtime
            nconc (list `(let ((,=var= ,val))
                          (cond ((null ,=var=))
                                ((eq ,=var= t)
                                 ,(if *empty-attribute-syntax*
                                      `(fmt " ~A" ,attr)
                                      `(fmt " ~A=~C~A~C"
                                            ,attr
                                            *attribute-quote-char*
                                            ,attr
                                            *attribute-quote-char*)))
                                (t
                                 (fmt " ~A=~C~A~C"
                                      ,attr
                                      *attribute-quote-char*
                                      ,=var=
                                      *attribute-quote-char*)))))))

(defgeneric convert-tag-to-string-list (tag attr-list body body-fn)
  (:documentation "Used by PROCESS-TAG to convert `HTML' into a list
of strings.  TAG is a keyword symbol naming the outer tag, ATTR-LIST
is an alist of its attributes \(the car is the attribute's name as a
keyword, the cdr is its value), BODY is the tag's body, and BODY-FN is
a function which should be applied to BODY.  The function must return
a list of strings or Lisp forms."))

(defmethod convert-tag-to-string-list (tag attr-list body body-fn)
  "The standard method which is not specialized.  The idea is that you
can use EQL specializers on the first argument."
  (declare (optimize speed space))
  (let ((tag (maybe-downcase tag))
        (body-indent
          ;; increase *HTML-INDENT* by 2 for body -- or disable it
          (when (and *html-indent* (not (member tag *html-no-indent-tags* :test #'string-equal)))
            (+ 2 *html-indent*))))
    (declare ((integer 0 8) body-indent))
    (nconc
     (if *html-indent*
       ;; indent by *HTML-INDENT* spaces
       (list +newline+ (n-spaces *html-indent*)))
     ;; tag name
     (list "<" tag)
     ;; attributes
     (convert-attributes attr-list)
     ;; body
     (if body
         (append
          (list ">")
          ;; now hand over the tag's body to TREE-TO-TEMPLATE
          (let ((*html-indent* body-indent))
            (funcall (the function body-fn) body))
          (when body-indent
            ;; indentation
            (list +newline+ (n-spaces *html-indent*)))
          ;; closing tag
          (list "</" tag ">"))
         ;; no body, so no closing tag unless defined in *HTML-EMPTY-TAGS*
         (if (or (not *html-empty-tag-aware-p*)
                 (member tag *html-empty-tags* :test #'string-equal))
             (list *empty-tag-end*)
             (list ">" "</" tag ">"))))))

(defun tree-to-template (tree)
  "Transforms an HTML tree into an intermediate format - mainly a
flattened list of strings. Utility function used by TREE-TO-COMMANDS-AUX."
  (loop for element in tree
        if (or (keywordp element)
                 (and (listp element)
                      (keywordp (first element)))
                 (and (listp element)
                      (listp (first element))
                      (keywordp (first (first element)))))
        ;; the syntax for a tag - process it
        nconc (process-tag element #'tree-to-template)
        ;; list - insert as sexp
        else if (consp element)
        collect `(let ((*html-indent* ,*html-indent*))
                   nil ;; If the element is (declare ...) it
                       ;; won't be interpreted as a declaration and an
                       ;; appropriate error could be signaled
                   ,element)
        ;; something else - insert verbatim
        else
        collect element))

(defun string-list-to-string (string-list)
  (declare (optimize speed space))
  "Concatenates a list of strings to one string."
  ;; note that we can't use APPLY with CONCATENATE here because of
  ;; CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (dolist (string string-list)
      (incf total-size (length string)))
    (let ((result-string (make-string total-size
                                      #+:lispworks #+:lispworks
                                      :element-type 'lw:simple-char))
          (curr-pos 0))
      (dolist (string string-list)
        (replace result-string string :start1 curr-pos)
        (incf curr-pos (length string)))
      result-string)))

(defun conc (&rest string-list)
  "Concatenates all arguments which should be string into one string."
  (funcall #'string-list-to-string string-list))

(defun tree-to-commands (tree  &key (prologue *html-prologue*) (indent *html-indent*) (stream *html-output*))
  (declare (optimize speed space))
  (when (and indent
             (not (integerp indent)))
    (setq *html-indent* 0))
  (let ((in-string-p t)
        collector
        string-collector
        (template (tree-to-template tree)))
    (when prologue
      (push +newline+ template)
      (when (eq prologue t)
        (setq prologue *html-prologue*))
      (push prologue template))
    (flet ((emit-string-collector ()
             "Generate a WRITE-STRING statement for what is currently
in STRING-COLLECTOR."
             (list 'write-string
                   (string-list-to-string (nreverse string-collector))
                   stream)))
      (dolist (element template)
        (cond ((and in-string-p (stringp element))
               ;; this element is a string and the last one
               ;; also was (or this is the first element) -
               ;; collect into STRING-COLLECTOR
               (push element string-collector))
              ((stringp element)
               ;; the last one wasn't a string so we start
               ;; with an empty STRING-COLLECTOR
               (setq string-collector (list element)
                     in-string-p t))
              (string-collector
               ;; not a string but STRING-COLLECTOR isn't
               ;; empty so we have to emit the collected
               ;; strings first
               (push (emit-string-collector) collector)
               (setq in-string-p nil
                     string-collector '())
               (push element collector))
              (t
               ;; not a string and empty STRING-COLLECTOR
               (push element collector))))
      (if string-collector
        ;; finally empty STRING-COLLECTOR if
        ;; there's something in it
        (nreverse (cons (emit-string-collector)
                        collector))
        (nreverse collector)))))

(defmacro with-html ((out &key stream (indent *html-indent*) (prologue *html-prologue*))
                     &body body)
  "Transform the enclosed BODY consisting of HTML s-expressions
into Lisp code to write the corresponding HTML as strings to OUT - which
should either hold a stream or which'll be bound to STREAM if supplied."
  (multiple-value-bind (declarations forms) (extract-declarations body)
    `(let ((,out ,(or stream '(make-synonym-stream '*standard-output*))))
       ,@declarations
       (check-type ,out stream)
       (macrolet ((htm (&body body)
                    `(with-html (,',out nil :prologue nil :indent ,,indent)
                       ,@body))
                  (fmt (&rest args)
                    `(format ,',out ,@args))
                  (esc (thing)
                    (std:with-gensyms (result)
                      `(let ((,result ,thing))
                         (when ,result (write-string (escape-string ,result) ,',out)))))
                  (str (thing)
                    (std:with-gensyms (result)
                      `(let ((,result ,thing))
                         (when ,result (princ ,result ,',out))))))
         ,@(tree-to-commands forms :stream out :indent indent :prologue prologue)))))
