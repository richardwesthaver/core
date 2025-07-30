;;; theme.lisp --- Color Themes

;; Color Theme Protocol

;;; Commentary:

;; loosely based on Anathema by contrapunctus, a CL theme library for CLIM
;; applications

;; Styles loosely correspond to Emacs 'faces'.

;;; Code:
(in-package :obj/color)

(defvar *theme* nil)

;;; Style
(defclass style () ())
(defgeneric style (self))
(defgeneric (setf style) (new self))
(defmacro define-style (name direct-superclasses direct-slots &rest opts))
;; (defmacro with-style ((medium style &rest opts) &body body))
;;; Theme
(defclass theme (style) ())
(defgeneric theme (self))
(defgeneric (setf theme) (new self))
(defmacro define-theme (name direct-superclasses direct-slots &rest opts))

