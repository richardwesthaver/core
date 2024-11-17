;;; html.lisp --- HTML Generator

;; HTML Generator a la CL-WHO or SPINNERET

;;; Commentary:

;;

;;; Code:
(in-package :web/html)

;; (describe 
;;  (dat/html:make-element (dat/html:make-document) "foo" nil))
;; (describe (dat/html:make-fragment (dat/html:make-document)))
(define-condition html-condition () ())
(deferror html-error (html-condition) ())

(defclass html-output-stream (wrapped-stream) ()
  (:default-initargs :stream (make-synonym-stream '*standard-output*)))

(defvar *html-output* (make-instance 'html-output-stream))
(defvar *html-lang* "en")
(defvar *html-charset* "UTF-8")
(defvar *html-path* nil)

(defmacro with-html ((out &optional (stream *html-output*)) &body body)
  `(let ((,out ,stream)) ,@body))

