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

(defmethod sk-print ((self skel) &key (stream t) (id t) exclude &allow-other-keys)
  (with-standard-io-syntax
    (let ((sk-name (keywordicate (string-left-trim "SK-" (class-name (class-of self))))))
      (if id
        (format stream "~S ~A~%" 
                sk-name 
                (format-sxhash (obj/id:id self)))
        (format stream "~S~%" sk-name)))
    (mapcar
     (lambda (slot)
       (let ((name (sb-mop:slot-definition-name slot)))
         (when (slot-boundp self name)
           (when-let ((val (slot-value self name)))
             (typecase val
               (sequence (unless (sequence:emptyp val) (format stream ":~A ~A~%" name val)))
               (hash-table (unless (zerop (hash-table-count val))
                             (format stream ":~A~%" name)
                             (pprint-tabular stream (hash-table-alist val) nil nil 2)
                             (terpri stream)))
               (t (format stream ":~A ~A~%" name val)))))))
     (remove-if (lambda (x) (member x exclude)) (sb-mop:class-direct-slots (class-of self))))
    self))
