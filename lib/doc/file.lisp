;;; lib/doc/file.lisp --- File Documentation

;; Support for inline documentation and comments in source-code files.

;;; Commentary:

;; NOTE 2024-04-05: sb-impl::read-comment takes an 'ignore' second
;; arg, but the return value is always ignored anyways?

;; As of [2024-04-04] we aren't considering any other files besides
;; Lisp source-code. We'll eventually open the door to other langs via
;; SYN, and finally integrate with other types of
;; documentation-specific files like READMEs.

;;;; Lisp Files

;; We determine the flavor of Lisp used in a source file by the file
;; extension. In reality we're only dealing with two flavors: Common
;; Lisp and Emacs Lisp.

;; The way we treat them at the read stage is almost identical. The
;; only difference being that Emacs Lisp does not support the inline
;; comment syntax.

;; In any case the idea is to do 'something' with comments instead of
;; getting rid of them at read-time.

;;;; Headers

;; Special consideration is given to source-code 'headers'. In our own
;; code, we use them often.

;; Source Headers have the form:

#|
;;; FILENAME --- SUMMARY

 ;; DESCRIPTION

 ;;; Commentary:

 ;; COMMENTARY

 ;;; Code:
 CODE
|#

;; Note the difference in comment characters used between the lines. Headings
;; start with 3, and the contents of those Headings start with 2. The first
;; heading/section is an 'anonymous' or 'meta' section that should be
;; considered required. All headings beneath it are 'named' sections. 'Code:'
;; is the only required named section and marks the end of a header. In the
;; example above, we may exclude the 'Commentary:' section or add additional
;; ones before and after it.

;;;; Headings

;; We define headings according to the Emacs notion of the term, as used in
;; outline-mode and org-mode. As mentioned, headings in source
;; files begin with a minimum of 3 comment characters. 

;; For each additional comment character in outline-mode, the nested 'level'
;; of the heading is increased and any non-header elements or header elements
;; with a level greater than the top-level are nested inside that heading.

;; 3 comment headings represent a level of 0. Any heading with a level
;; > 0 is a Subheading. For example, we are in a subheading named
;; 'Headings' of level 2, inside a subheading of level 1, inside a
;; heading named 'Commentary'.

;;;; Keyword Comments

;; Some comment blocks starts with a keyword followed by an optional timestamp
;; and colon - these are known as 'keyword comments' which are similar to
;; Org-mode inline tasks. The keyword is equivalent to a todo-state, the
;; timestamp the CREATED property, and the remainder of the first line after
;; the colon the title. Keyword comments with a timestamp should never be
;; considered permanent, and often denote the action which will result in its
;; removal.

;;;; Comment Blocks

;; The vertical bar comment syntax has special meaning when compiled with the
;; DOC reader active, which overloads the standard reader with additional
;; capabilities. Conceptually they are quite similar to Org source blocks. The
;; following comment block will be compiled as an ORGAN:ORG-ELEMENT and saved
;; for use in our API documentation generator:

#| org
- *foo*
- /bar/
- _baz_
- [[comp:][a link]]
|#

;; The parsing function is determined by the first line (in this case 'org'),
;; and the parser input is a string which starts at the next line and ends at
;; the matching comment block end sequence.

;; When using the core emacs distribution you may also activate
;; `ulang-minor-mode' to swith to `org-mode' highlighting and bindings while
;; point is inside an org comment block or an organ block '#&&#'

;;; Code:
(in-package :doc)

(deftempo :file-documentation
  "* <%@var name%>
:PROPERTIES:
:SUMMARY: <%@var summary%>
:LOCATION: <%@var location%>
:END:
<%@var description%>
<%@var info%>
<%@if commentary%>
<%@var commentary%>
<%@endif%><%@if outline%>
<%@var outline%>
<%@endif%>")

(defconstant +max-file-heading-level+ 8)
(defconstant +min-file-heading-level+ 3)

(defclass file-heading ()
  ((name :initarg :name :type string)
   (level :initform 0 :initarg :level :type (integer 0 #.+max-file-heading-level+))
   (description :initarg :description :type string))
  (:documentation "A generic file heading according to Emacs outline-mode."))

(defun heading-line-p (string)
  (uiop:string-prefix-p #.(make-string +min-file-heading-level+ :initial-element #\;) string))

(defun read-comment-line (stream)
  "Read a comment line from STREAM. Returns two values: the uncommented
string and a 'level' indicating how many comment characters were
stripped. Note that this level is NOT the same as the heading level."
  (let* ((level 0))
    (loop for c = (peek-char nil stream nil)
          while c
          until (not (char= c #\;))
          do (read-char stream nil)
          do (incf level))
    (values
     (when-let ((line (read-line stream nil))) (string-trim " " line))
     level)))

(defun read-file-heading (stream)
  (multiple-value-bind (name level) (read-comment-line stream)
    (make-instance 'file-heading :name name :level level :description "")))

(defun decomment (s) (string-left-trim "; " s))

(defclass file-headline (file-heading)
  ((summary :initarg :summary :type string)
   (opts :initform nil :initarg :opts :type list)))

(defun read-file-headline-description (stream)
  "Read a headline description returning a string and a second value indicating
the name of the next top-level headline or NIL."
  (let ((next) (description))
    (loop named desc
          for l = (read-line stream)
          while l
          if (heading-line-p l)
          do (progn (setf next (decomment l)) (return-from desc))
          else 
          do (unless (sequence:emptyp (trim l))
               (push (decomment l) description)
               (push (make-string 1 :initial-element #\newline) description)))
    (values 
     (when description (trim (apply #'concatenate 'string (nreverse description))))
     (unless (or (< (length next) 4) (string-equal "Code" (subseq next 0 4)))
       (string-right-trim ":" next)))))

(defun headline-values-p (string)
  (unless (> 5 (length string))
    (let ((found (search " --- " string)))
      (values (subseq string 0 found) (when found (subseq string (+ found 5)))))))

(defun split-headline-values (string)
  "Split the headline in STRING into individual values."
  (multiple-value-bind (name rest) (headline-values-p string)
    (when name
      (if rest
          (multiple-value-bind (summary opts) (headline-values-p rest)
            (values name summary opts))
          (values name nil nil)))))

(defun read-file-headline (stream &optional error)
  (handler-case      
      (let ((line (read-comment-line stream))) ;; throw out second value
        (multiple-value-bind (name summary opts) (split-headline-values line)
          (when name
            (multiple-value-bind (desc next) (read-file-headline-description stream)
              (values
               (make-instance 'file-headline
                 :name name
                 :summary summary
                 :opts opts
                 :level 0
                 :description desc)
               next)))))
    (end-of-file (c) (when error (error "failed to read file headline: ~A" c)))))

(defclass file-header ()
  ((headline :initarg :headline :type file-headline)
   (commentary :initarg :commentary :type file-heading))
  (:documentation "A source-file header object containing a FILE-HEADLINE and array of
optional top-level FILE-HEADINGs."))

(defun code-start-p (line)
  (string-prefix-p ";;; Code:" line))

(defun read-until-code-start (stream)
  (loop for l = (read-line stream nil)
        while l
        until (code-start-p l)
        finally (return (file-position stream))))

(defun read-file-header (path &optional (if-does-not-exist :error))
  "Read a FILE-HEADER from PATH.

File headers always appear at the very start of a file so the stream position
is always assumed to be 0.

Return two values: the file-header and the position of the first character
after the code start header (see CODE-START-P)."
  (with-open-file (f path :if-does-not-exist if-does-not-exist)
    (multiple-value-bind (hl next) (read-file-headline f)
      (when hl
        (let ((h (make-instance 'file-header :headline hl))
              (body-start))
          (when next
            (setf (slot-value h 'commentary) 
                  (make-instance 'file-heading 
                    :level 0 
                    :name next
                    :description
                    (trim
                     (apply 'concatenate 'string
                            (loop for l = (read-line f nil)
                                  while l
                                  until (and (code-start-p l)
                                             (setf body-start (file-position f)))
                                  unless (sequence:emptyp (trim l))
                                  collect (decomment l)
                                  collect (make-string 1 :initial-element #\newline)))))))
          (values h body-start))))))

(defun read-file-outline (path &optional start (if-does-not-exist :error))
  "Return a list of file-headings defined in PATH."
  (with-open-file (f path :if-does-not-exist if-does-not-exist)
    ;; calculate offset of first line after ';;; Code:'
    (file-position f (or start (read-until-code-start f)))
    (trace! "code starts at ~d" (file-position f))
    (loop for l = (read-line f nil)
          while l
          if (heading-line-p l)
          collect (with-input-from-string (s l) (read-file-heading s)))))

;; (read-file-outline "proto.lisp")
;; (defmacro define-file-heading (type slots))

(defclass file-documentation (file-component id)
  ((path :initarg :path :type pathname :accessor path)
   (header :initarg :header :type file-header)
   (outline :initarg :outline :type sequence)
   (links :initarg :links :type sequence))
  (:documentation "An object containing the header, outline, and relevant
  links in a source file. Note that this object only contains inline
  comments. Symbol documentation such as this one will not be captured in
  instances of this object. The ID slot of file-documentation is a hash of
  the full file contents."))

(defmethod initialize-instance :after ((self file-documentation) &key &allow-other-keys)
  (when (path self) (setf (id self) (cry:crc64-file (path self)))))

;; (defmethod print-object ((self file-documentation) stream)
;;   (print-unreadable-object (self stream :type t)
;;     (format stream "~A" (path self))))

(defmethod change-class ((self file-component) (new (eql 'file-documentation)) &key &allow-other-keys)
  (make-instance new :header (read-file-header (path self) nil)))

(defun file-documentation (path)
  "Return the FILE-DOCUMENTATION for PATH."
  (multiple-value-bind (header code-start) (read-file-header path nil)
    (make-instance 'file-documentation
      :path path
      :header header
      :outline (when code-start (read-file-outline path code-start))
      :name (pathname-name path)
      :type (pathname-type path))))

(definline file-header (doc) (slot-boundp! doc 'header))
(definline file-headline (doc) (when-let ((h (file-header doc))) (slot-boundp! h 'headline)))
(definline file-commentary (doc)
  (when-let* ((h (file-header doc))
              (c (slot-boundp! h 'commentary)))
    (slot-boundp! c 'description)))
(definline file-summary (doc) 
  (when-let ((h (file-header doc)))
    (slot-boundp! h 'summary)))
(definline file-description (doc) 
  (when-let ((h (file-header doc)))
    (slot-boundp! h 'description)))

(defmethod description ((self file-documentation)) (file-description self))
(defmethod summary ((self file-documentation)) (file-summary self))
(defmethod commentary ((self file-documentation)) (file-commentary self))

(defmethod publish ((self file-documentation) &key output)
  (with-slots (id name outline) self
    (let ((gen (execute-template (keywordicate (class-name (class-of self)))
                                 :env
                                 `(:name ,(name self) :id ,id
                                   :location ,(path self)
                                   :description ,(file-description self)
                                   :summary ,(file-summary self)
                                   :commentary ,(file-commentary self)
                                   ;; :tags ,(file-tag-string self)
                                   :outline ,outline))))
      (case output
        ('nil (values (org-parse (document-keyword self) gen) gen))
        (:string gen)
        (t (write-string gen output))))))

;; TODO 2026-06-21: (defmethod publish ((self file-heading) &key))
