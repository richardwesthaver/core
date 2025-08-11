;;; syn/forth/pkg.lisp --- Experimental support for Forth

;; experimental

;;; Commentary:

;; This module is at the very bottom of our language tower and is intended to
;; support the Forth family of languages.

;; Forth is a very simple language traditionally based directly on
;; asm. Recently I have been interested in some other concatenative languages
;; such as Factor and Joy but what has really piqued my interest is some
;; recent-past Forths which target microcontrollers.

;; My first programming experience was with Arduino and I've bought several
;; ATMegas, Unos, and other random devices throughout the years but have never
;; felt comfortable writing complete programs for them. I didn't find learning
;; Arduino C fun and gave up in pursuit of more general-purpose langs. I've
;; always had this idea of coming back to tiny embedded devices and making
;; them useful, but none of the tools available have kept my attention long
;; enough to get anything done.

;; I'm committed to Lisp and Linux for most traditional personal-compute
;; machines like desktops, laptops, and phones - but for microcontrollers
;; there is little need for such heavy machinery and levels of abstraction. As
;; I see it, Forth is a perfect fit for communicating with these machines.
;; When all you get is a sprinkling of flash storage there's no room for an
;; OS, fancy syntax, objects, types.. there is no room for a Lisp. 

;; ref: https://www.flashforth.com/

;; ref: http://krue.net/avrforth/

;; ref: https://forth-standard.org/

;;; Code:
(defpackage :syn/gen/forth
  (:nicknames :syn/forth)
  (:use :cl :std :syn/lang :ast :syn/gen)
  (:export
   :forth-reader
   :forth-syntax
   :read-forth-string
   :read-forth-file))

(defpackage syn/gen/forth/swap)

(in-package :syn/forth)

(defvar *forth-backend*
  (append *cl-symbols*
          '()))

(export *forth-backend*)

(defparameter *forth-symbols* nil)

(defparameter *forth-exports*
  (append *forth-symbols* *cl-symbols*))

(defparameter *forth-swap* *forth-symbols*)

(pkg:defpackage* :syn/gen/forth/sym
    (:shadow-symbols *forth-swap* :export-symbols *forth-exports*)
  (:nicknames :forth)
  (:use :cl)
  (:import-from :syn/forth :forth-reader :read-forth-string :read-forth-file))

(defclass forth-stack () ())

(defclass forth-data-stack (forth-stack) ())

(defclass forth-control-stack (forth-stack) ())

(defclass forth-return-stack (forth-stack) ())

(defclass forth-program (ast) 
  ((env)))

;; @ !
