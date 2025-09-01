;;; theme.lisp --- Color Themes

;; Color Theme Protocol

;;; Commentary:

;; loosely based on Anathema by contrapunctus, a CL theme library for CLIM
;; applications

;; Styles loosely correspond to Emacs 'faces', will perhaps serve as a basis
;; for a 'presentation' class.

;;; Code:
(in-package :obj/color)

(defvar *theme* nil
  "The currently active theme.")

;;; Style
(defkernel style () 
  ((attributes :initarg :attributes :initform nil)))

(defgeneric style (self))
(defgeneric (setf style) (new self))
(defmacro define-style (name direct-superclasses direct-slots &rest opts))
;; (defmacro with-style ((medium style &rest opts) &body body))
;; (defun apply-style (style object &rest args))
;;; Theme
(defgeneric theme (self)
  (:documentation "Return the theme associated with SELF."))
(defgeneric (setf theme) (new self)
  (:documentation "Set the theme associated with SELF."))

(defclass theme ()
  ((palette :initarg :palette :accessor palette :initform *palette*)
   (style :initarg :style :accessor style :initform (make-hash-table)
          :documentation "A map of names to styles.")))

(defmacro define-theme (name direct-superclasses direct-slots &rest opts))

;; (defun load-theme (name))
;; (defun find-theme (name))
;; (defun apply-theme (name))
;; (defun apply-style (style a &rest b))
