;;; theme.lisp --- Color Themes

;; Color Theme Protocol

;;; Commentary:

;; loosely based on Anathema by contrapunctus, a CL theme library for CLIM
;; applications.

;; Styles loosely correspond to Emacs 'faces', will perhaps serve as a basis
;; for a 'presentation' class.

;;; Code:
(in-package :obj/color)

(defvar *theme* nil
  "The currently active theme.")

(defvar *theme-table* (make-hash-table)
  "A global table containing a mapping of names to themes.")

;;; Style
(defkernel style () 
  ((attributes :initarg :attributes :initform nil :accessor style-attributes)))

(defgeneric style (self))
(defgeneric (setf style) (new self))
(defmacro defstyle (name direct-superclasses direct-slots &rest opts)
  `(defkernel ,name ,(or direct-superclasses '(style)) ,direct-slots ,@opts))
;; (defmacro with-style ((medium style &rest opts) &body body))

;;; Theme
(defgeneric theme (self)
  (:documentation "Return the theme associated with SELF."))
(defgeneric (setf theme) (new self)
  (:documentation "Set the theme associated with SELF."))

(defclass theme ()
  ((palette :initarg :palette :accessor palette :initform *palette*)
   (style :initarg :style :accessor style :initform (make-hash-table)
          :documentation "A map of names to styles.")))

(defmacro deftheme (name direct-superclasses direct-slots &rest opts)
  `(defclass ,name ,(or direct-superclasses '(theme)) ,direct-slots ,@opts))

(defun find-theme (name)
  (gethash name *theme-table*))

(defun load-theme (name)
  (setf *theme* (find-theme name)))

;; (defun apply-theme (name obj &rest args))
;; (defun apply-style (style a &rest b))
