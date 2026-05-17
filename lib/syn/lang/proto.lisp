;;; proto.lisp --- SYN/LANG Protocol

;; 

;;; Code:
(in-package :syn/lang)

;;; Vars
(defvar *langs* (list :c :rs :js :py))
(sb-ext:define-load-time-global *lang* nil)

;;; Conditions
(defcondition lang-condition () ()
              (:handler))

(deferror lang-error (lang-condition) () (:reporter t))
(defwarning lang-warning (lang-condition) () (:reporter t))

;;; Config
(defconfig lang-config (ast id)
  (flags
   tools
   features))

;;; Types
(defun langp (self)
  (or 
   (typep self 'lang-config)
   (memq self *langs*)))

(deftype lang () `(satisfies langp))

;;; Protocol
(defgeneric lang (self))
(defgeneric (setf lang) (new self))

;;; Macros
(defmacro deflang (name supers slots &rest options)
  `(defclass ,name ,(or supers '(lang)) ,slots ,@options))

(defmacro with-lang (lang &body body)
  `(with-ts-lang syn/lang:*lang* ,lang
     ,@body))

;;; Utils
(defun lang-stats (lang)
  (with-ts-lang lang l
    `(:symbols ,(ts-language-symbol-count l)
      :fields ,(ts-language-field-count l))))
