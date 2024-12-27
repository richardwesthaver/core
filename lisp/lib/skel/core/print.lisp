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

(defmethod sk-print ((self skel) &key (stream t) (id t) exclude (case :downcase) &allow-other-keys)
  (let ((name (skel/core/obj::sk-slot-name self (when (eql :downcase case))))
        (*print-case* case))
    (if id
        (format stream "~S ~A~%" 
                name 
                (format-sxhash (obj/id:id self)))
        (format stream "~S~%" name)))
  (mapcar
   (lambda (slot)
     (let ((name (sb-mop:slot-definition-name slot)))
       (when (slot-boundp self name)
         (when-let ((val (slot-value self name))
                    (name (if (eql :downcase case) (string-downcase name) name)))
           (typecase val
             (sequence (unless (sequence:emptyp val) (format stream ":~A ~A~%" name val)))
             (hash-table (unless (zerop (hash-table-count val))
                           (format stream ":~A~%" name)
                           (pprint-tabular stream (hash-table-alist val) nil nil 2)
                           (terpri stream)))
             (t (format stream ":~A ~A~%" name val)))))))
   (remove-if (lambda (x) (member (keywordicate (sb-mop:slot-definition-name x)) exclude))
              (sb-mop:class-direct-slots (class-of self))))
  self)

(defmethod sk-print ((self t) &key (stream t) (pretty t) (case :downcase))
  (write self :stream stream :pretty pretty :case case)
  (terpri stream))
