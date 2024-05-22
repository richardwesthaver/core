;;; std/macs/collecting.lisp --- Collecting Macros

;; ported from CL-UTILITIES

;;; Code:
(in-package :std/macs)

;; This should only be called inside of COLLECTING macros, but we
;; define it here to provide an informative error message and to make
;; it easier for SLIME (et al.) to get documentation for the COLLECT
;; function when it's used in the COLLECTING macro.
(defun collect (thing)
  "Collect THING in the context established by the COLLECTING macro"
  (error "Can't collect ~S outside the context of the COLLECTING macro"
         thing))

(defmacro collecting (&body body)
  "Collect things into a list forwards. Within the body of this macro,
the COLLECT function will collect its argument into the list returned
by COLLECTING."
  (with-gensyms (collector tail)
    `(let (,collector ,tail)
      (labels ((collect (thing)
                 (if ,collector
                     (setf (cdr ,tail)
                           (setf ,tail (list thing)))
                     (setf ,collector
                           (setf ,tail (list thing))))))
        ,@body)
      ,collector)))

(defmacro with-collectors ((&rest collectors) &body body)
  "Collect some things into lists forwards. The names in COLLECTORS
are defined as local functions which each collect into a separate
list.  Returns as many values as there are collectors, in the order
they were given."
  (%with-collectors-check-collectors collectors)
  (let ((gensyms-alist (%with-collectors-gensyms-alist collectors)))
    `(let ,(loop for collector in collectors
                 for tail = (cdr (assoc collector gensyms-alist))
                 nconc (list collector tail))
      (labels ,(loop for collector in collectors
                     for tail = (cdr (assoc collector gensyms-alist))
                     collect `(,collector (thing)
                               (if ,collector
                                   (setf (cdr ,tail)
                                         (setf ,tail (list thing)))
                                   (setf ,collector
                                         (setf ,tail (list thing))))))
        ,@body)
      (values ,@collectors))))

(defun %with-collectors-check-collectors (collectors)
  "Check that all of the COLLECTORS are symbols. If not, raise an error."
  (let ((bad-collector (find-if-not #'symbolp collectors)))
    (when bad-collector
      (error 'type-error
             :datum bad-collector
             :expected-type 'symbol))))

(defun %with-collectors-gensyms-alist (collectors)
  "Return an alist mapping the symbols in COLLECTORS to gensyms"
  (mapcar #'cons collectors
          (mapcar (compose #'gensym
                           #'(lambda (x)
                               (format nil "~A-TAIL-" x)))
                  collectors)))
