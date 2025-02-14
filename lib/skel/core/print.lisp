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

(defun sk-coerce-name (name &optional (case :downcase))
  (if (eql :downcase case) (string-downcase name) (string-upcase name)))

(defun sk-print-slot (slot self &key (stream *standard-output*) (limit 8) (case :downcase))
     (let ((name (sb-mop:slot-definition-name slot)))
       (when (slot-boundp self name)
         (when-let ((val (slot-value self name))
                    (name (sk-coerce-name name case)))
           (typecase val
             (string (format stream ":~A ~A~%" name val))
             (cons (unless (sequence:emptyp val) (format stream ":~A ~A~%" name val)))
             (vector (unless (sequence:emptyp val)
                       (format stream ":~A~%[" name)
                       (pprint-tabular stream (coerce (take limit val) 'list) nil)
                       (force-output stream)
                       (when (> (length val) limit) (format stream " ..."))
                       (format stream "]~%")))
             (hash-table (unless (zerop (hash-table-count val))
                           (format stream ":~A~%{" name)
                           (pprint-tabular 
                            stream 
                            (mapcar (lambda (x) (setf (car x) (sk-coerce-name (car x) case)) x)
                                    (coerce (take limit (hash-table-alist val)) 'list))
                            nil)
                           (force-output stream)
                           (when (> (hash-table-count val) limit) (format stream " ..."))
                           (format stream "}~%")))
             (t (format stream ":~A ~A~%" name val)))))))

(defmethod sk-print ((self skel) &key (stream *standard-output*) (id t) exclude (case :downcase) direct (limit 8) &allow-other-keys)
  (let ((name (skel/core/obj::sk-slot-name self (when (eql :downcase case))))
        (*print-case* case))
    (if id
        (format stream "~S ~A~%" 
                name 
                (format-sxhash (obj/id:id self)))
        (format stream "~S~%" name)))
  (mapcar
   (lambda (slot) (sk-print-slot slot self :stream stream :limit limit :case case))
   (remove-if (lambda (x) (member (keywordicate (sb-mop:slot-definition-name x)) exclude))
              (if direct
                  (sb-mop:class-direct-slots (class-of self))
                  (sb-mop:class-slots (class-of self)))))
  self)
