;;; lib/dat/xml.lisp --- XML Data Format

;; based on the re-implementation of https://github.com/rpgoldman/xmls

;; our nodes are called XML-NODE and inherit from OBJ/TREE:NODE.

;; XMLS:NODE-NAME == OBJ/TREE:NODE-KEY

;;; Code:
(in-package :dat/xml)

;;; XMLS

;;;-----------------------------------------------------------------------------
;;; GLOBAL SETTINGS
;;;-----------------------------------------------------------------------------
(defvar *strip-comments* t)
(defvar *compress-whitespace* t)
(defvar *discard-processing-instructions*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type vector *entities*))
  (defvar *entities*
    #(("lt;" #\<)
      ("gt;" #\>)
      ("amp;" #\&)
      ("apos;" #\')
      ("quot;" #\")))
  (defvar *whitespace* (remove-duplicates
                        '(#\Newline #\Space #\Tab #\Return #\Linefeed))))
(defvar *char-escapes*
  (let ((table (make-array 256 :element-type 'string :initial-element "")))
    (loop
     for code from 0 to 255
     for char = (code-char code)
     for entity = (first (find char *entities* :test #'char= :key #'second))
     do (setf (svref table code)
              (cond
                (entity
                 (concatenate 'string "&" entity))
                ((and (or (< code 32) (> code 126))
                      (not (= code 10))
                      (not (= code 9)))
                 (format nil "&#x~x;" code))
                (t
                 (format nil "~x" char))))
     finally (return table))
    table))

;;;---------------------------------------------------------------------------
;;; DYNAMIC VARIABLES
;;;---------------------------------------------------------------------------
(defvar *parser-stream* nil
  "The currently-being-parsed stream. Used so that we can appropriately track
the line number.")
(defvar *parser-line-number* nil)



;;;-----------------------------------------------------------------------------
;;; CONDITIONS
;;;-----------------------------------------------------------------------------
(define-condition xml-parse-error (error)
  ((line :initarg :line
         :initform nil
         :reader error-line))
  (:report (lambda (xpe stream)
             (format stream "XML-PARSE-ERROR~@[ at line ~d~]"
                     (error-line xpe)))))

(defmethod initialize-instance :after ((obj xml-parse-error) &key)
  (unless (slot-value obj 'line)
    (when *parser-line-number*
      (setf (slot-value obj 'line) *parser-line-number*))))

;;;-----------------------------------------------------------------------------
;;; NODE INTERFACE
;;;-----------------------------------------------------------------------------
(defstruct (xml-node (:constructor %make-xml-node))
  name
  ns
  attrs
  children)
  
(defun make-xml-node (&key name ns attrs child children)
  "Convenience function for creating a new xml node."
  (when (and child children)
    (error "Cannot specify both :child and :children for MAKE-NODE."))
  (let ((children (if child
                      (list child)
                    children)))
    (%make-xml-node :name name :ns ns
                :children children
                :attrs attrs)))

;;;---------------------------------------------------------------------------
;;; XML Processing Instruction
;;;---------------------------------------------------------------------------
(defstruct proc-inst
  (target "" :type string)
  (contents "" :type string)
  )


;;;-----------------------------------------------------------------------------
;;; UTILITY FUNCTIONS
;;;-----------------------------------------------------------------------------
(defun compress-whitespace (str)
  (if *compress-whitespace*
      (progn
        (setf str (string-trim *whitespace* str))
        (if (= 0 (length str))
            nil
            str))
      str))

(defun write-escaped (string stream)
  (write-string (escape-for-html string) stream))

(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#039;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

(defun make-extendable-string (&optional (size 10))
  "Creates an adjustable string with a fill pointer."
  (make-array size
              :element-type 'character
              :adjustable t
              :fill-pointer 0))

(defun push-string (c string)
  "Shorthand function for adding characters to an extendable string."
  (vector-push-extend c string))

(defun translate-raw-value (raw-value)
  "Helper function for xml generation."
  (etypecase raw-value
    (string raw-value)
    (symbol (symbol-name raw-value))
    (integer (format nil "~D" raw-value))
    (float (format nil "~G" raw-value))))

(defun generate-xml (e s indent)
  "Renders a lisp node tree to an xml string stream."
  (if (> indent 0) (incf indent))
  (etypecase e
    (xml-node
     (progn
       (dotimes (i (* 2 (- indent 2)))
         (write-char #\Space s))
       (format s "<~A~@[ xmlns=\"~A\"~]" (xml-node-name e) (xml-node-ns e))
       (loop for a in (xml-node-attrs e)
             do (progn
                  (write-char #\Space s)
                  (write-string (first a) s)
                  (write-char #\= s)
                  (write-char #\" s)
                  (write-escaped (translate-raw-value (second a)) s)
                  (write-char #\" s))))
     (if (null (xml-node-children e))
         (progn
           (write-string "/>" s)
           (if (> indent 0) (write-char #\Newline s)))
         (progn
           (write-char #\> s)
           (if (> indent 0) (write-char #\Newline s))
           (mapc #'(lambda (c) (generate-xml c s indent)) (xml-node-children e))
           (if (> indent 0)
               (progn
                 (dotimes (i (* 2 (- indent 2)))
                   (write-char #\Space s))))
           (format s "</~A>" (xml-node-name e))
           (if (> indent 0) (write-char #\Newline s)))))
    (number
     (generate-xml (translate-raw-value e) s indent))
    (symbol
     (generate-xml (translate-raw-value e) s indent))
    (string
     (progn
       (if (> indent 0)
           (progn
             (dotimes (i (* 2 (- indent 2)))
               (write-char #\Space s))))
       (write-escaped e s)
       (if (> indent 0) (write-char #\Newline s))))))

;;;-----------------------------------------------------------------------------
;;; PARSER STATE & LOOKAHEAD
;;;-----------------------------------------------------------------------------
(defstruct state
  "Represents parser state.  Passed among rules to avoid threading issues."
  (got-doctype nil)
  (lines 1 :type integer)
  nsstack
  stream)

(defun resolve-entity (ent)
  "Resolves the xml entity ENT to a character.  Numeric entities are
converted using CODE-CHAR, which only works in implementations that
internally encode strings in US-ASCII, ISO-8859-1 or UCS."
  (declare (type simple-string ent))
  (or (and (>= (length ent) 2)
           (char= (char ent 0) #\#)
           (code-char
            (if (char= (char ent 1) #\x)
                (parse-integer ent :start 2 :end (- (length ent) 1) :radix 16)
                (parse-integer ent :start 1 :end (- (length ent) 1)))))
      (second (find ent *entities* :test #'string= :key #'first))
      (error "Unable to resolve entity ~S" ent)))

(declaim (inline peek-stream))
(defun peek-stream (stream)
  "Looks one character ahead in the input stream.  Serves as a potential hook for
character translation."
  (peek-char nil stream nil))

(defun read-stream (stream)
  "Reads a character from the stream, translating entities as it goes."
  (let ((c (read-char stream nil)))
    (if (and c (not (char= c #\&)))
        c
        (loop with ent = (make-extendable-string 5)
              for char = (read-char stream)
              do (push-string char ent)
              until (char= char #\;)
              finally (return (resolve-entity (coerce ent 'simple-string)))))))

;;;---------------------------------------------------------------------------
;;; Shadow READ-CHAR and UNREAD-CHAR so we can count lines while we parse...
;;;---------------------------------------------------------------------------
(defun read-char (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (let ((eof-p nil))
    (let ((c
            (catch 'char-return
              (handler-bind
                  ((end-of-file
                     #'(lambda (e)
                         (declare (ignore e))
                         (unless eof-error-p
                           (setf eof-p t)
                           (throw 'char-return eof-value)))))
                (common-lisp:read-char stream t nil recursive-p)))))
    (when (and (eq stream *parser-stream*)
               (not eof-p)
               (char= c #\newline))
      (incf *parser-line-number*))
    c)))

(defun unread-char (char &optional (stream *standard-input*))
  (when (char= char #\newline)
    (decf *parser-line-number*))
  (common-lisp:unread-char char stream))
    
;;;END shadowing--------------------------------------------------------------

(define-symbol-macro next-char (peek-stream (state-stream s)))

(defmacro eat ()
  "Consumes one character from the input stream."
  `(read-char (state-stream s)))

(defmacro puke (char)
  "The opposite of EAT."
  `(unread-char ,char (state-stream s)))

(defmacro match (&rest matchers)
  "Attempts to match the next input character with one of the supplied matchers."
  `(let ((c (peek-stream (state-stream s))))
    (and c
     (or ,@(loop for m in matchers
                 collect (etypecase m
                           (standard-char `(char= ,m c))
                           (symbol `(,m c)))))
     ;; cheat here a little bit - eat entire char entity instead
     ;; of peeked char
     (read-stream (state-stream s)))))

(defmacro match-seq (&rest sequence)
  "Tries to match the supplied matchers in sequence with characters in the input stream."
  `(and ,@(loop for s in sequence
                collect `(match ,s))))

(defmacro match* (&rest sequence)
  "Matches any occurances of any of the supplied matchers."
  `(loop with data = (make-extendable-string 10)
    for c = (match ,@sequence)
    while c
    do (push-string c data)
    finally (return data)))

(defmacro match+ (&rest sequence)
  "Matches one or more occurances of any of the supplied matchers."
  `(and (peek ,@sequence)
    (match* ,@sequence)))

(defmacro peek (&rest matchers)
  "Looks ahead for an occurance of any of the supplied matchers."
  `(let ((c (peek-stream (state-stream s))))
    (or ,@(loop for m in matchers
                collect (etypecase m
                          (standard-char `(char= ,m c))
                          (symbol `(,m c)))))))

(defmacro must (&rest body)
  "Throws a parse error if the supplied forms do not succeed."
  `(or (progn ,@body)
    (error 'xml-parse-error)))

;;;-----------------------------------------------------------------------------
;;; PARSER INTERNAL FUNCTIONS
;;;-----------------------------------------------------------------------------
(defstruct element
  "Common return type of all rule functions."
  (type nil :type symbol)
  (val nil))

(defun resolve-namespace (elem env)
  "Maps the ns prefix to its associated url via the supplied ns env."
  (let ((ns (xml-node-ns elem)))
    (dolist (e env)
      (let ((nsurl (assoc ns e :test #'string=)))
        (and nsurl
             (setf (xml-node-ns elem) (cadr nsurl))
             (return ns))))))

;;;-----------------------------------------------------------------------------
;;; MATCH AND RULE BUILDING UTILITIES
;;;-----------------------------------------------------------------------------
(defmacro defmatch (name &rest body)
  "Match definition macro that provides a common lexical environment for matchers."
  `(defun ,name (c)
    ,@body))

(defmacro defrule (name &rest body)
  "Rule definition macro that provides a common lexical environment for rules."
  `(defun ,name (s)
    ,@body))

(defmacro matchfn (name)
  "Convenience macro for creating an anonymous function wrapper around a matcher macro."
  `(lambda (s) (match ,name)))

(defun none-or-more (s func)
  "Collects any matches of the supplied rule with the input stream."
  (declare (type function func))
  (let ((val (funcall func s)))
    (if val
        (multiple-value-bind (res nextval)
            (none-or-more s func)
          (values res (cons val nextval)))
        (values t nil))))

(defun one-or-more (s func)
  "Collects one or more matches of the supplied rule with the input stream."
  (declare (type function func))
  (let ((val (funcall func s)))
    (if val
        (multiple-value-bind (res nextval)
            (none-or-more s func)
          (declare (ignore res))
          (cons val nextval))
        nil)))

;;;-----------------------------------------------------------------------------
;;; MATCHERS
;;;-----------------------------------------------------------------------------
(defmatch digit ()
  (and c (digit-char-p c)))

(defmatch letter ()
  (and c (alpha-char-p c)))

;; Modified because *whitespace* is not defined at compile
;; time. [2004/08/31:rpg]
(defmatch ws-char ()
  (member c *whitespace*))
;;;  (case c
;;;    (#.*whitespace* t)
;;;    (t nil)))

(defmatch namechar ()
  (or
   (and c (alpha-char-p c))
   (and c (digit-char-p c))
   (case c
     ((#\. #\- #\_ #\:) t))))

(defmatch ncname-char ()
  (or
   (and c (alpha-char-p c))
   (and c (digit-char-p c))
   (case c
     ((#\. #\- #\_) t))))

(defmatch attr-text-dq ()
  (and c (not (member c (list #\< #\")))))

(defmatch attr-text-sq ()
  (and c (not (member c (list #\< #\')))))

(defmatch chardata ()
  (and c (not (char= c #\<))))

(defmatch comment-char ()
  (and c (not (eql c #\-))))

;;;-----------------------------------------------------------------------------
;;; RULES
;;;-----------------------------------------------------------------------------
(defrule ncname ()
  (and (peek letter #\_)
       (match+ ncname-char)))

(defrule qname ()
  (let (name suffix)
    (and
     (setf name (ncname s))
     (or
      (and
       (match #\:)
       (setf suffix (ncname s)))
      t))
    (values name suffix)))

(defrule attr-or-nsdecl ()
  (let (suffix name val)
    (and
     (setf (values name suffix) (qname s))
     (or
      (and
       (progn
         (match* ws-char)
         (match #\=))
       (or
        (and
         (progn
           (match* ws-char)
           (match #\"))
         (setf val (match* attr-text-dq))
         (match #\"))
        (and
         (progn
           (match* ws-char)
           (match #\'))
         (setf val (match* attr-text-sq))
         (match #\'))))
      t)
     (if (string= "xmlns" name)
	 (list 'nsdecl suffix val)
	 ;; If SUFFIX is true, then NAME is Prefix and SUFFIX is
	 ;; LocalPart.
	 (if suffix
	     (list 'attr suffix val :attr-ns name)
	     (list 'attr name val))))))

(defrule ws ()
  (and (match+ ws-char)
       (make-element :type 'whitespace :val nil)))

(defrule name ()
  (and
   (peek namechar #\_ #\:)
   (match* namechar)))

(defrule ws-attr-or-nsdecl ()
  (and
   (ws s)
   (attr-or-nsdecl s)))

(defrule start-tag ()
  (let (name suffix attrs nsdecls)
    (and
     (peek namechar)
     (setf (values name suffix) (qname s))
     (multiple-value-bind (res a)
         (none-or-more s #'ws-attr-or-nsdecl)
       (mapcar (lambda (x) (if (eq (car x) 'attr)
                               (push (cdr x) attrs)
                               (push (cdr x) nsdecls)))
               a)
       res)
     (or (ws s) t)
     (values
      (make-xml-node
       :name (or suffix name)
       :ns (and suffix name)
       :attrs attrs)
      nsdecls))))

(defrule end-tag ()
  (let (name suffix)
    (and
     (match #\/)
     (setf (values name suffix) (qname s))
     (or (ws s) t)
     (match #\>)
     (make-element :type 'end-tag :val (or suffix name)))))

(defrule comment ()
  (and
   (match-seq #\! #\- #\-)
   (progn
     (loop until (match-seq #\- #\- #\>)
           do (eat))
     t)
   (make-element :type 'comment)))

;;; For the CDATA matching of ]]> I by hand generated an NFA, and then
;;; determinized it (also by hand).  Then I did a simpler thing of just pushing
;;; ALL the data onto the data string, and truncating it when done.
(defrule comment-or-cdata ()
  (and
   (peek #\!)
   (must (or (comment s)
             (and
              (match-seq #\[ #\C #\D #\A #\T #\A #\[)
              (loop with data = (make-extendable-string 50)
                    with state = 0
                    for char = (eat)
                    do (push-string char data)
                    do (case state
                         (0
                          (case char
                            (#\]
                             (dbg :cdata "State 0 Match #\], go to state {0,1} = 4.")
                             (setf state 4))
                            (otherwise
                             (dbg :cdata "State 0 Non-], go to (remain in) state 0."))))
                         (4 ; {0, 1}
                          (case char
                            (#\]
                             (dbg :cdata "State 4 {0, 1}, match ], go to state {0,1,2} = 5")
                             (setf state 5))
                            (otherwise
                             (dbg :cdata "State 4 {0, 1}, Non-], go to state 0.")
                             (setf state 0))))
                         (5 ; {0, 1, 2}
                          (case char
                            (#\]
                             (dbg :cdata "State 5 {0, 1, 2}, match ], stay in state 5."))
                            (#\>
                             (dbg :cdata "State 5 {0, 1, 2}, match >, finish match and go to state 3.")
                             (setf state 3))
                            (otherwise
                             (dbg :cdata "State 5 {0, 1, 2}, find neither ] nor >; go to state 0.")
                             (setf state 0))))
                         )
                    until (eql state 3)
                    finally (return (make-element
                                     :type 'cdata
                                     :val (coerce
                                           ;; rip the ]]> off the end of the data and return it...
                                           (subseq data 0 (- (fill-pointer data) 3))
                                           'simple-string)))))))))


(declaim (ftype function element))     ; forward decl for content rule
(defrule content ()
  (if (match #\<)
      (must (or (comment-or-cdata s)
                (processing-instruction s)
                (element s)
                (end-tag s)))
      (or (let (content)
            (and (setf content (match+ chardata))
                 (make-element :type 'data :val (compress-whitespace content)))))))

(defrule element ()
  (let (elem children nsdecls end-name)
    (and
     ;; parse front end of tag
     (multiple-value-bind (e n)
         (start-tag s)
       (setf elem e)
       (setf nsdecls n)
       e)
     ;; resolve namespaces *before* parsing children
     (if nsdecls (push nsdecls (state-nsstack s)) t)
     (or (if (or nsdecls (state-nsstack s))
             (resolve-namespace elem (state-nsstack s)))
         t)
     ;; parse end-tag and children
     (or
      (match-seq #\/ #\>)
      (and
       (match #\>)
       (loop for c = (content s)
             while c
             do (etypecase c
                  (element (case (element-type c)
                             (end-tag
                              (return (setf end-name (element-val c))))
                             ;; processing instructions may be discarded
                             (pi
                              (unless *discard-processing-instructions*
                                (when (element-val c)
                                  (push (element-val c) children))))
                             (t (if (element-val c)
                                    (push (element-val c) children)))))))
       (string= (xml-node-name elem) end-name)))
     ;; package up new node
     (progn
       (setf (xml-node-children elem) (nreverse children))
       (make-element :type 'elem :val elem)))))

(defrule processing-instruction ()
  (let (name contents)
    (and
     (match #\?)
     (setf name (name s))
     (not (string= name "xml"))
     ;; contents of a processing instruction can be arbitrary stuff, as long
     ;; as it doesn't contain ?>...
     (setf contents (pi-contents s))
     ;; if we get here, we have eaten ?> off the input in the course of
     ;; processing PI-CONTENTS
     (make-element :type 'pi :val (make-proc-inst :target name :contents contents)))))

(defrule pi-contents ()
  (loop with data = (make-extendable-string 50)
         with state = 0
         for char = (eat)
         do (push-string char data)
         do (ecase state
              (0
               (case char
                 (#\?
                  (dbg :pi-contents "State 0 Match #\?, go to state 1.")
                  (setf state 1))
                 (otherwise
                  (dbg :pi-contents "State 0 ~c, go to (remain in) state 0." char))))
              (1
               (case char
                 (#\>
                  (dbg :pi-contents "State 1 Match #\>, done.")
                  (setf state 2))
                 (otherwise
                  (dbg :pi-contents "State 1, ~c, do not match #\>, return to 0." char)
                  (setf state 0)))))
         until (eql state 2)
         finally (return (coerce
                          ;; rip the ?> off the end of the data and return it...
                          (subseq data 0 (max 0 (- (fill-pointer data) 2)))
                          'simple-string))))

(defrule xmldecl ()
    (let (name contents)
    (and
     (match #\?)
     (setf name (name s))
     (string= name "xml")
     (setf contents (none-or-more s #'ws-attr-or-nsdecl))
     (match-seq #\? #\>)
     (make-element :type 'xmldecl :val contents))))

(defrule comment-or-doctype ()
  ;; skip dtd - bail out to comment if it's a comment
  ;; only match doctype once
  (and
   (peek #\!)
   (or (comment s)
       (and (not (state-got-doctype s))
            (must (match-seq #\D #\O #\C #\T #\Y #\P #\E))
            (loop with level = 1
                  do (case (eat)
                       (#\> (decf level))
                       (#\< (incf level)))
                  until (eq level 0)
                  finally (return t))
            (setf (state-got-doctype s) t)
            (make-element :type 'doctype)))))

(defrule misc ()
  (or
   (ws s)
   (and (match #\<) (must (or (processing-instruction s)
                              (comment-or-doctype s)
                              (element s))))))

(defrule document ()
  (let (elem)
    (if (match #\<)
        (must (or (xmldecl s)
                  (comment-or-doctype s)
                  (setf elem (element s)))))
    ;; NOTE: I don't understand this: it seems to parse arbitrary crap
    (unless elem
      (loop for c = (misc s)
            while c
            do (cond ((eql (element-type c) 'elem)
                      (return (setf elem c)))
                     ((and (eql (element-type c) 'pi)
                           (not *discard-processing-instructions*))
                      (return (setf elem c))))))
                       
    (and elem (element-val elem))))

;;;-----------------------------------------------------------------------------
;;; PUBLIC INTERFACE
;;;-----------------------------------------------------------------------------
(defun write-xml (e s &key (indent nil))
  "Renders a lisp node tree to an xml stream.  Indents if indent is non-nil."
  (if (null s)
      (toxml e :indent indent)
    (generate-xml e s (if indent 1 0))))

(defun write-prologue (xml-decl doctype s)
  "Render the leading <?xml ... ?> and <!DOCTYPE ... > tags to an xml stream."
  (format s "<?xml")
  (dolist (attrib xml-decl)
    (format s " ~A=\"~A\"" (car attrib) (cdr attrib)))
  (format s " ?>~%")
  (when doctype
    (format s "<!DOCTYPE ~A>~%" doctype)))

(defun write-prolog (xml-decl doctype s)
  (write-prologue xml-decl doctype s))

(defun toxml (e &key (indent nil))
  "Renders a lisp node tree to an xml string."
  (with-output-to-string (s)
    (write-xml e s :indent indent)))

(defun xml-parse (s &key (compress-whitespace t) (quash-errors t))
  "Parses the supplied stream or string into a lisp node tree."
  (let* ((*compress-whitespace* compress-whitespace)
         (*discard-processing-instructions* t)
         (stream
           (etypecase s
             (string (make-string-input-stream s))
             (stream s)))
         (*parser-stream* stream)
         (*parser-line-number* 1))
    (if quash-errors
        (handler-case
            (document (make-state :stream stream))
          (end-of-file () nil)
          (xml-parse-error () nil))
        (document (make-state :stream stream)))))

;;; XMLrep
(defun make-xmlrep (tag &key (representation-kind :node) namespace attribs children)
  (case representation-kind
    ((:list)
     (cond
       (namespace
        (list (list tag namespace) (list attribs) children))
       (t
        (list tag (list attribs) children))))
    ((:node)
     (make-xml-node :name tag :ns namespace :attrs attribs :children children))
    (otherwise
     (error "REPRESENTATION-KIND must be :LIST or :NODE, found ~s" representation-kind))))

(defgeneric xmlrep-add-child! (xmlrep child)
  (:method ((xmlrep xml-node) child)
    (setf (xml-node-children xmlrep)
          (append (xml-node-children xmlrep)
                  (list child))))
  (:method ((xmlrep cons) child)
    (setf (cddr xmlrep)
          (append (cddr xmlrep)
                  (list child)))))

(defgeneric xmlrep-tag (treenode)
  (:method ((treenode xml-node))
    (xml-node-name treenode))
  (:method ((treenode cons))
    (let ((tag-name (car treenode)))
      ;; detect the "namespaced" case
      (cond
        ((consp tag-name) (car tag-name))
        (t tag-name)))))

(defun xmlrep-tagmatch (tag treenode)
  ;;child nodes to XMLREPs could be strings or nodes
  (unless (stringp treenode)
    (string-equal tag (xmlrep-tag treenode))))

(defgeneric xmlrep-attribs (treenode)
  (:method ((treenode xml-node))
    (xml-node-attrs treenode))
  (:method ((treenode cons))
    (cadr treenode)))

(defgeneric (setf xmlrep-attribs) (attribs treenode)
  (:argument-precedence-order treenode attribs)
  (:method (attribs (treenode xml-node))
    (setf (xml-node-attrs treenode) attribs))
  (:method (attribs (treenode cons))
    (setf (cadr treenode) attribs)))

(defgeneric xmlrep-children (treenode)
  (:method ((treenode xml-node))
    (xml-node-children treenode))
  (:method ((treenode cons))
    (cddr treenode)))

(defgeneric (setf xmlrep-children) (children treenode)
  (:argument-precedence-order treenode children)
  (:method (children (treenode xml-node))
    (setf (xml-node-children treenode) children))
  (:method (children (treenode cons))
    (setf (cddr treenode) children)))

(defun xmlrep-string-child (treenode &optional (if-unfound :error))
  (let ((children (xmlrep-children treenode)))
    (if (and (eq (length children) 1) (typep (first children) 'string))
        (first children)
        (if (eq if-unfound :error)
            (error "Node does not have a single string child: ~a" treenode)
            if-unfound)
        )))

(defun xmlrep-integer-child (treenode)
  (parse-integer (xmlrep-string-child treenode)))

(defun xmlrep-find-child-tags (tag treenode)
  "Find all the children of TREENODE with TAG."
  (remove-if-not #'(lambda (child) (xmlrep-tagmatch tag child))
                 (xmlrep-children treenode)))

(defun xmlrep-find-child-tag (tag treenode
                                  &optional (if-unfound :error))
  "Find a single child of TREENODE with TAG.  Returns an error
if there is more or less than one such child."
  (let ((matches (xmlrep-find-child-tags tag treenode)))
    (case (length matches)
      (0 (if (eq if-unfound :error)
             (error "Couldn't find child tag ~A in ~A"
                tag treenode)
             if-unfound))
      (1 (first matches))
      (otherwise (error "Child tag ~A multiply defined in ~A"
                        tag treenode)))))

(defun xmlrep-attrib-value (attrib treenode
                            &optional (if-undefined :error))
  "Find the value of ATTRIB, a string, in TREENODE.
if there is no ATTRIB, will return the value of IF-UNDEFINED,
which defaults to :ERROR."
  (let ((found-attrib (find-attrib attrib treenode)))
    (cond (found-attrib
           (second found-attrib))
          ((eq if-undefined :error)
           (error "XML attribute ~S undefined in ~S"
                  attrib treenode))
          (t
           if-undefined))))

(defun find-attrib (attrib treenode)
  "Returns the attrib CELL (not the attrib value) from 
TREENODE, if found.  This cell will be a list of length 2,
the attrib name (a string) and its value."
  (find attrib (xmlrep-attribs treenode)
        :test #'string=
        :key #'car))
  
(defun (setf xmlrep-attrib-value) (value attrib treenode)
  ;; ideally, we would check this...
  (let ((old-val (xmlrep-attrib-value attrib treenode nil)))
    (if old-val
        (cond ((null value)
               ;; just delete this attribute...
               (setf (xmlrep-attribs treenode)
                     (remove attrib (xmlrep-attribs treenode)
                             :test #'string=
                             :key #'first))
               nil)
              (t (let ((cell (find-attrib attrib treenode)))
                   (setf (second cell) value)
                   value)))
        ;; no old value
        (cond ((null value)
               nil)                         ; no old value to delete
              (t
               (setf (xmlrep-attribs treenode)
                     (append (xmlrep-attribs treenode)
                             (list (list attrib value))))
               value)))))

(defun xmlrep-boolean-attrib-value (attrib treenode
                                    &optional (if-undefined :error))
  "Find the value of ATTRIB, a string, in TREENODE.
The value should be either \"true\" or \"false\".  The
function will return T or NIL, accordingly.  If there is no ATTRIB,
will return the value of IF-UNDEFINED, which defaults to :ERROR."
  (let ((val (xmlrep-attrib-value attrib treenode
                                  if-undefined)))
    (cond ((string-equal val "true")
           t)
          ((string-equal val "false") nil)
          (t (error "Not a boolean value, ~A for attribute ~A."
                    val attrib)))))

;;; XML extraction tool
(defun extract-path (key-list xml)
  "Extracts data from XML parse tree.  KEY-LIST is a path for descending down
named objects in the XML parse tree.  For each KEY-LIST element, XML subforms
are searched for a matching tag name.  Finally the whole last XML subform on the
path is normally returned if found; however the symbol * may be added at the end
of KEY-LIST to return list of all objects /enclosed/ by the last subform on
KEY-LIST. Also KEY-LIST may be dotted as explained below to return XML tag
attributes from the last subform on KEY-LIST.

XML is to have the forms as returned by PARSE-TO-LIST or PARSE:
        (tag-name (attributes-list) subform*),
        ((tag-name . name-space) (attributes-list) subform*), or
        #s(node :name tag-name
                :ns name-space
                :attrs attributes-list
                :children subform*)

The first element in KEY-LIST must match the top level form in XML.
Subsequently each element in the KEY-LIST is to match a subform.

An element of KEY-LIST may be a string atom.  In that case the first subform
with tag-name matching the string is matched.  An element of KEY-LIST may also
be a list of string atoms in this format:
        (tag-name (attribute-name attribute-value) ...)

The first subform with name matching TAG-NAME /and/ having attributes matching
attribute-names and attribute-values is matched.  Zero or more attribute/value
pairs may be given.

Normally the whole subform matching last element in KEY-LIST is returned.  The
symbol * can be the last element of KEY-LIST to return list of all subforms
enclosed by the last matched form.  Attributes of last matched subform may be
searched by ending KEY-LIST in dot notation, in which case the string after dot
matches an attribute name.  The two element list of attribute name and value is
returned. The symbol * may be used after dot to return the whole attribute list.

In the case where the search fails NIL is returned.  However it is possible that
the search partially succeeds down the key path.  Three values are returned
altogether and the 2nd and 3rd values give information about how much of
KEY-LIST was matched, and at what point in XML:
        (values RESULT  KEY-LIST-FRAGMENT  XML-FRAGMENT)

When RESULT is non-NIL, the others are NIL. When result is NIL however, the
others are:
        XML-FRAGMENT
          The last XML form that /did/ match in the key list.  It matches the first
          element of KEY-LIST-FRAGMENT.

        KEY-LIST-FRAGMENT
          The /remaining/ part of the KEY-LIST that did not succeed.  However the
          /first/ item on KEY-LIST-FRAGMENT matches the XML-FRAGMENT returned.  The
          failure is at the second item on KEY-LIST-FRAGMENT.

In the case of complete failure, where even the very first item on KEY-LIST does not
match the top XML form given, all three return values are NIL.  (It suffices to check
the first two return values.)"
  (labels ((attribs-match-p ( key-attribs-list xml-attribs-list )
             ;; search for (attr-name attr-value) pairs from KEY-ATTRIBS-LIST on
             ;; XML-ATTRIBS-LIST.  true if all key pairs found.
             (loop
                :with attribs-match-var := t
                :for attrib-key-pair  :in key-attribs-list
                :do
                  (setq attribs-match-var
                        (and attribs-match-var
                             (find attrib-key-pair xml-attribs-list :test #'equal)))
                :finally (return attribs-match-var)))

           (find-test ( key xml-form )
             ;; test whether the XML-FORM matches KEY
             (cond
               ;; just the XML tag name in key
               ;; XML name is simple string
               ((and (stringp key)
                     (stringp (xmlrep-tag xml-form)))
                (string-equal key (xmlrep-tag xml-form)))

               ;; key form (tag-name (attr-name attr-value) ...)
               ((and (find-test (car key) xml-form)
                     (attribs-match-p (cdr key) (xmlrep-attribs xml-form))))))

           (descend ( key-list xml-form )
             ;; recursive run down KEY-LIST.  If XML-FORM runs down to NIL before reaching
             ;; the end of KEY-LIST, it will be NIL at the end.  If not, what is
             ;; remaining of XML-FORM is the found item.
             (cond
               ;; KEY-LIST ends without dotted item, at the target XML form
               ((null (cdr key-list))
                (values xml-form nil nil))

               ;; dotted item at the end of KEY-LIST, search attribute list of target XML form
               ((atom (cdr key-list))
                (if (eq '* (cdr key-list))
                    (values (xmlrep-attribs xml-form) nil nil)
                    (find (cdr key-list)  (xmlrep-attribs xml-form)
                          :test (lambda (key item) (equal key (car item))))))

               ;; more tag names to match on KEY-LIST
               ('t
                (if (eq '* (cadr key-list))
                    (values (xmlrep-children xml-form) nil nil)
                    (let ((selected-xml-form (find (cadr key-list)  (xmlrep-children xml-form)
                                                   :test #'find-test)))
                      (if selected-xml-form
                          (descend (cdr key-list) selected-xml-form)

                          ;; no matching sub-form, indicate what part of KEY-LIST did not match
                          (values nil key-list xml-form))))))))

    ;; empty list, degenerate usage
    (when (null key-list)
      (error "KEY-LIST is empty."))

    ;; search down after initial match
    (if (find-test (car key-list) xml)
        (descend  key-list xml)
        (values nil nil nil))))

