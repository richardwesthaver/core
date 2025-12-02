;;; print.lisp --- Tensor Printer

;; 

;;; Code:
(in-package :obj/tensor)

;; Routines for printing tensors/matrices nicely.

(defparameter *tensor-print-parameters* `(10 5 0)
  "
0: Maximum number of elements in any particular argument to print.
Set this to T to print all the elements.

1: Maximum number of arguments of the tensor to print.
Set this to T to print all the arguments.

2: Determines how many spaces will be printed before each row
of a matrix (default 0)
")

(defgeneric print-element (x element stream)
  (:documentation "
  Syntax
  ======
  (PRINT-ELEMENT tensor element stream)

  Purpose
  =======
  This generic function is specialized to TENSOR to
  print ELEMENT to STREAM.  Called by PRINT-TENSOR/MATRIX
  to format a tensor into the STREAM."))

(defun print-tensor (tensor stream)
  (letv* ((rank (order tensor)) (dims (dimensions tensor))
          (two-print-calls 0)
          ((print-max-len print-max-args print-indent) *tensor-print-parameters*))
    (labels ((two-print (tensor subs)
               (let* ((maxw (make-array (if (eq print-max-len t) (aref dims 1) (1+ print-max-len)) :initial-element 0))
                      (strs (loop for i from 0 below (aref dims 0)
                                  if (or (eq print-max-len t) (< i print-max-len))
                                  collect (loop for j from 0 below (aref dims 1)
                                                with cprints = nil
                                                if (or (eq print-max-len t) (< j print-max-len))
                                                do (let ((str (with-output-to-string (str)
                                                                (print-element tensor 
                                                                               (apply #'ref 
                                                                                      (list* tensor i j subs)) 
                                                                               str))))
                                                     (push str cprints)
                                                     (setf (aref maxw j) (max (aref maxw j) (length str))))
                                                else 
                                                do (let ((str (with-output-to-string (str) (format str "..."))))
                                                     (push str cprints)
                                                     (setf (aref maxw j) (max (aref maxw j) (length str))))
                                                finally (return (nreverse cprints)))
                                  into rprints
                                  else do (return rprints)
                                  finally (return rprints))))
                 (loop for row in strs
                       do (format stream (format nil "~~~AT" print-indent))
                       do (loop for cref in row
                                for j from 0
                                do (format stream (replace (make-string (+ (aref maxw j) 4) :initial-element #\Space) cref :start1 (if (char= (aref cref 0) #\-) 0 1))))
                       do (format stream "~%"))
                 (unless (or (eq print-max-len t) (< (aref dims 0) print-max-len))
                   (format stream (format nil "~~~AT.~~%~~~:*~AT:~~%" print-indent)))))
             (rec-print (tensor idx subs)
               (if (>= idx 2)
                   (dotimes (i (aref dims idx) t)
                     (unless (rec-print tensor (1- idx) (append `(,i) subs))
                       (return nil)))
                   (progn
                     (if (or (eq print-max-args t) (< two-print-calls print-max-args))
                         (progn
                           (format stream "~A~%" (append '(\: \:) subs))
                           (two-print tensor subs)
                           (format stream "~%")
                           (incf two-print-calls)
                           t)
                         (progn
                           (format stream "~A~%" (make-list rank :initial-element '\:))
                           (format stream (format nil "~~~AT..~~%~~~AT::~~%" print-indent print-indent))
                           nil))))))
      (case rank
        (1
         (format stream (format nil "~~~AT" print-indent))
         (dotimes (i (aref dims 0))
           (if (or (eq print-max-len t) (< i print-max-len))
               (progn
                 (print-element tensor (ref tensor i) stream)
                 (format stream "~,4T"))
               (progn
                 (format stream "...")
                 (return nil))))
         (format stream "~%"))
        (2
         (two-print tensor nil))
        (t
         (rec-print tensor (1- (order tensor)) nil))))))

(defmethod print-element ((x tensor) element stream)
  (cond
    ((floatp element) (format stream "~,4,-2,,,,'Eg" element))
    ((complexp element)
     (let ((realpart (cl:realpart element))
           (imagpart (cl:imagpart element)))
       (if (not (zerop imagpart))
           (format stream "~,4,-2,,,,'Eg ~a ~,4,-2,,,,'Egi"  realpart (if (>= imagpart 0) #\+ #\-) (abs imagpart))
           (format stream "~,4,-2,,,,'Eg" realpart))))
    (t (format stream "~a" element))))

#+nil
(defmethod print-object ((tensor tensor) stream)
  (if (typep tensor 'dense-tensor)
      (print-unreadable-object (tensor stream :type t)
        (format stream (concatenate 'string "~A" (if (slot-value tensor 'parent) "~,4T:DISPLACED" "")) (dimensions tensor))
        (when (> (total-size tensor) 0)
          (format stream "~%")
          (print-tensor tensor stream)))
      (print-unreadable-object (tensor stream :type t)
        (format stream "~A, size: ~A/~A" (dimensions tensor) (total-size tensor) (store-size tensor)))))

(defmethod print-object ((g graph-accessor) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~A, size: ~A/~A" (dimensions g) (aref (fence g) (1- (length (fence g)))) (length (δ-i g)))))

