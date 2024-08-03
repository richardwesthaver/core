;;; print.lisp --- Skel Printer

;; SK-PRINT

;;; Commentary:

;; SK-PRINT is the top-level interface, and dispatches on all sorts of SKEL
;; objects. The output is different than the PRINT-OBJECT methods, which are
;; implemented in the SKEL/CORE/OBJ package.

;; SK-PRINT is the 'external print' representation, which is structured, akin
;; to PPRINT - while PRINT-OBJECT is the 'internal print' and unstructured
;; representation.

;; All printer parameters are dynamic and dispatch occurs in the same manner
;; as the standard Lisp Printer. Additional parameters may be provided in the
;; future.

;;; Code:
(in-package :skel/core/print)

;; sb-pretty::*standard-pprint-dispatch-table*
;; *readtable*

(sb-ext:defglobal *sk-print-dispatch-table* (sb-pretty::make-pprint-dispatch-table #() nil nil))

(defmethod sk-print ((self skel))
  (pprint (cons (keywordicate (class-name (class-of self))) (format-sxhash (obj/id:id self)))))
