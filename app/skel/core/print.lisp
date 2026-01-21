;;; print.lisp --- Skel Printer

;; SK-PRINT

;;; Commentary:

;; SK-PRINT is the top-level interface, and dispatches on all sorts of SKEL
;; objects. The output is different than the PRINT-OBJECT methods, which are
;; implemented in the SKEL/CORE package.

;; SK-PRINT is the 'external print' representation, which is structured, akin
;; to PPRINT - while PRINT-OBJECT is the 'internal print' and unstructured
;; representation.

;; All printer parameters are dynamic and dispatch occurs in the same manner
;; as the standard Lisp Printer. Additional parameters may be provided in the
;; future.

;;; Code:
(in-package :skel/core)
(declaim (optimize (speed 3)))
;; sb-pretty::*standard-pprint-dispatch-table*
;; *readtable*

(declaim (inline sk-coerce-name sk-coerce-sequence))

(defun sk-coerce-name (name &optional (case :downcase))
  (if (eql :downcase case) (string-downcase name) (string-upcase name)))

(defun sk-coerce-sequence (seq &optional limit)
  (coerce
   (if limit
       (take limit seq)
       seq)
   'list))

(defun sk-print-slot (slot self &key (stream *standard-output*) (limit 8) (case :downcase))
  (declare (stream stream) (skel self))
     (let ((name (sb-mop:slot-definition-name slot))
           (*print-case* case))
       (when (slot-boundp self name)
         (let ((val (slot-value self name))
               (name (sk-coerce-name name case)))
           (typecase val
             (string (format stream ":~A ~A~%" name val))
             (cons (unless (sequence:emptyp val) (format stream ":~A ~A~%" name val)))
             (vector (unless (sequence:emptyp val)
                       (format stream ":~A [" name)
                       (pprint-tabular stream (sk-coerce-sequence val limit) nil nil 2)
                       (force-output stream)
                       (if (and limit (> #2=(length val) #1=(the positive-fixnum limit)))
                           (format stream " ...~d]~%" (- #2# limit))
                           (format stream "]~%"))))
             (hash-table (unless (zerop (hash-table-count val))
                           (format stream ":~A {" name)
                           (pprint-tabular stream (sk-coerce-sequence (hash-table-alist val) limit)
                                           nil nil 2)
		           (if (and limit (> #4=(hash-table-count val) #3=(the positive-fixnum limit)))
			       (format stream " ...~d}~%" (- #4# limit))
			       (format stream "}~%"))))
             (t (format stream ":~A ~A~%" name val)))))))

(defmethod sk-print ((self skel) &key (stream *standard-output*) (id t) exclude (case :downcase) direct (limit 8) &allow-other-keys)
  (declare (stream stream) (positive-fixnum limit))
  (let ((name (skel/core::sk-slot-name self (when (eql :downcase case))))
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

(defmethod sk-print ((self t) &key (stream *standard-output*))
  (println self stream))
