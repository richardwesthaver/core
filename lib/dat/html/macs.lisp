;;; macs.lisp --- HTML macros

;; WITH-HTML-OUTPUT and friends

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

(defmacro with-html ((out &optional (stream *html-output*)) &body body)
  `(let ((,out ,stream)) ,@body))
