;;; print.lisp --- Tensor Printer

;; 

;;; Code:
(in-package :obj/tensor)
;;; PRINT
;; FIX 2025-08-16: 
(defun print-tensor (tensor stream)
  (let ((rank (rank tensor))
        (dims (dimensions tensor))
        (two-print-calls 0))
    (labels ((two-print (tensor subs)
               (let ((strs) (cprints)
                     (maxw (make-array (if (eq *print-tensor-max-len* t) (aref dims 1) (1+ *print-tensor-max-len*)) :initial-element 0)))
                 (setq strs
                       (loop for i from 0 below (aref dims 0)
                             if (or (eq *print-tensor-max-len* t) (< i *print-tensor-max-len*))
                             collect (loop for j from 0 below (aref dims 1)
                                           if (or (eq *print-tensor-max-len* t) (< j *print-tensor-max-len*))
                                           do 
                                              (let ((str (with-output-to-string (str)
                                                           (print-element tensor (ref tensor (append `(,i ,j) subs)) str))))
                                                (push str cprints)
                                                (setf (aref maxw j) (max (aref maxw j) (length str))))
                                           else do
                                              (let ((str (with-output-to-string (str) (format str "..."))))
                                                (push str cprints)
                                                (setf (aref maxw j) (max (aref maxw j) (length str)))
                                                (return cprints))
                                           finally (return cprints))
                             into rprints
                             finally (return rprints)))
                 (loop for row in strs
                       do (format stream (format nil "~~~AT" *print-tensor-indent*))
                       do 
                          (loop for cref in row
                                with j = 0
                                do (format stream (replace (make-string (+ (aref maxw j) 4) :initial-element #\Space) cref :start1 (if (char= (aref cref 0) #\-) 0 1)))
                                do (incf j))
                       do (format stream "~%"))
                 (unless (or (eq *print-tensor-max-len* t) (< (aref dims 0) *print-tensor-max-len*))
                   (format stream (format nil "~~~AT.~~%~~~:*~AT:~~%" *print-tensor-indent*)))))
             (rec-print (tensor idx subs)
               (if (>= idx 2)
                   (dotimes (i (aref dims idx) t)
                     (unless (rec-print tensor (1- idx) (append `(,i) subs))
                       (return nil)))
                   (progn
                     (if (or (eq *print-tensor-max-args* t) (< two-print-calls *print-tensor-max-args*))
                         (progn
                           (format stream "~A~%" (append '(\: \:) subs))
                           (two-print tensor subs)
                           (format stream "~%")
                           (incf two-print-calls)
                           t)
                         (progn
                           (format stream "~A~%" (make-list rank :initial-element '\:))
                           (format stream (format nil "~~~AT..~~%~~~AT::~~%" *print-tensor-indent* *print-tensor-indent*))
                           nil))))))
      (case rank
        (1
         (format stream (format nil "~~~AT" *print-tensor-indent*))
         (dotimes (i (aref dims 0))
           (if (or (eq *print-tensor-max-len* t) (< i *print-tensor-max-len*))
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
         (rec-print tensor (1- (rank tensor)) nil))))))

(defmethod print-object ((tensor standard-tensor) stream)
  (print-unreadable-object (tensor stream :type t)
    (let ((dims (dimensions tensor)))
      ;; (if ;; (and (slot-value tensor 'parent-tensor) dims)
      ;; dims
      ;; (format stream "~A~,4T:DISPLACED" dims)
      (format stream "~A" dims)
      ;; )
      (when (> (size tensor) 0)
        (format stream "~%")
        (print-tensor tensor stream)))))

(defmethod print-object ((tensor sparse-tensor) stream)
  (declare (optimize (safety 0) (debug 1)))
  (print-unreadable-object (tensor stream :type t)
    (format stream
            (concatenate 'string
                         "~A, store-size: ~A"
                         (if (slot-value tensor 'parent-tensor) ",4T:DISPLACED" ""))
            (dimensions tensor) (store-size tensor))))
