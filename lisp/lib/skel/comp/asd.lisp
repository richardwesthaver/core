;;; lib/skel/comp/asd.lisp --- ASDF System Definition Compiler

;; ASDF/PARSE-DEFSYSTEM may come in handy for testing.

;; The problem with ASD files is that they're read-only afaik - eg
;; there's no 'write' methods implemented on ASD:SYSTEM objects. This
;; makes it a bit tedious because we obviously want to transform
;; SK-LISP-SYSTEM objects directly to SYSTEM, but also need to be able
;; to write them out as discrete files - for portability. Probably
;; will end up violating all that is DRY and holy.

;;; Code:
(in-package :skel/comp)

;; (describe (asdf:find-system :skel))

(defclass sk-lisp-system (skel sk-meta) 
  (build-pathname entry-point defsystem-depends-on depends-on weakly-depends-on in-order-to if-feature))

(defclass sk-lisp-component (skel)
  (type value))

(defmethod sk-compile ((self sk-lisp-system) stream &key &allow-other-keys))

(defmethod sk-write-file ((self sk-lisp-system) &key path))

(defmethod sk-read-file ((self sk-lisp-system) path))
