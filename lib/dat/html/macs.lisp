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

(defmacro with-html ((out &optional (stream *html-output*)) &body body)
  `(let ((,out ,stream)) ,@body))
