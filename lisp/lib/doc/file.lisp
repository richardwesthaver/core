;;; lib/doc/file.lisp --- File Documentation

;; Support for inline documentation and comments in source-code files.

;;; Commentary:

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
;; comment syntax '#| some comment |#'.

;; In any case the idea is to do 'something' with comments instead of
;; getting rid of them at read-time.

;;;;; Headers

;; Special consideration is given to source-code 'header' blocks. In
;; our own code, we use them as much as possible but haven't been
;; using them to their full potential just yet.

;; You will find most code, including this file begins with a block of
;; the following form, where CAPS is variable input from the
;; developer:

#|
,;;; PATH --- SHORT-DESCRIPTION

;; LONG-DESCRIPTION

,;;; Commentary:

;; COMMENTARY

,;;; Code:
CODE
|#

;; Note the difference in comment characters used between the
;; lines. Headings start with 3, and the contents of those Headings
;; start with 2. The first heading/section is an 'anonymous' or 'meta'
;; section that should be considered required. All headings beneath it
;; are 'named' sections. 'Code:' is the only required named section,
;; so in the example above, we may exclude the 'Commentary:' section.

;;;;; Headings

;; We define headings according to the Emacs notion of the term, as
;; used in outline-mode and org-mode. As mentioned, headings in source
;; files begin with a minimum of 3 comment characters. For each
;; additional comment character, the nested 'level' of the heading is
;; increased and any non-header elements or header elements with a
;; level greater than the top-level are nested inside that heading.

;; 3 comment headings represent a level of 0. Any heading with a level
;; > 0 is a Subheading. For example, we are in a subheading named
;; 'Headings' of level 2, inside a subheading of level 1, inside a
;; heading named 'Commentary'.

;;; Code:
(in-package :doc)

;; asdf:source-file-type asdf:source-file-explicit-type
(defvar *source-file-types* '(common-lisp emacs-lisp rust sh nu))

(defconstant +max-file-heading-level+ 8)

(defclass file-heading ()
  ((name :initarg :name :type string)
   (level :initform 0 :initarg :level :type (integer 0 #.+max-file-heading-level+))))

(defclass file-headline (file-heading)
  ((summary :initarg :summary :type string)
   (opts :initform nil :initarg :opts :type list)
   (description :initarg :description :type string))) ;; should be comment-block etc

(defclass file-header ()
  ((headline :initarg :headline :type file-headline)
   (headings :initarg :headings :type (array file-heading)))
  (:documentation "A source-file header object containing a FILE-HEADLINE and array of
optional top-level FILE-HEADINGs."))

;; (defmacro define-file-heading (type slots))

(defclass file-documentation ()
  ((path :initarg :path :type pathname :accessor doc-path)
   (header :initarg :header :type file-header)
   (contents :initarg :contents :type sequence)
   (locations :initarg :locations :type sequence))
  (:documentation "An object containing the header, contents, and relevant
  locations of a source file. This object should be the result of a
  function like COMPILE-FILE-DOCUMENTATION. Note that this object only
  contains inline comments. Symbol documentation such as this one will
  not be captured in instances of this object."))

(defmethod print-object ((self file-documentation) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~A" (doc-path self))))

(defun file-documentation (path)
  "Return the FILE-DOCUMENTATION for PATH."
  (make-instance 'file-documentation
    :path path))

;; asdf:source-file
