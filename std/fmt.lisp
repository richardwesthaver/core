;;; std/fmt.lisp --- printer and format utils

;;; Code:
(in-package :std/fmt)

(defun iprintln (x &optional (n 2) stream)
  (println (format nil "~A~A" (make-string n :initial-element #\Space) x) stream))

(defun printer-status ()
  (format t ";;           *print-array* = ~a~%" *print-array*)
  (format t ";;            *print-base* = ~a~%" *print-base*)
  (format t ";;            *print-case* = ~a~%" *print-case*)
  (format t ";;          *print-circle* = ~a~%" *print-circle*)
  (format t ";;          *print-escape* = ~a~%" *print-escape*)
  (format t ";;          *print-gensym* = ~a~%" *print-gensym*)
  (format t ";;          *print-length* = ~a~%" *print-length*)
  (format t ";;           *print-level* = ~a~%" *print-level*)
  (format t ";;           *print-lines* = ~a~%" *print-lines*)
  (format t ";;     *print-miser-width* = ~a~%" *print-miser-width*)
  (format t ";; *print-pprint-dispatch* = ~a~%" *print-pprint-dispatch*)
  (format t ";;          *print-pretty* = ~a~%" *print-pretty*)
  (format t ";;           *print-radix* = ~a~%" *print-radix*)
  (format t ";;        *print-readably* = ~a~%" *print-readably*)
  (format t ";;    *print-right-margin* = ~a~%" *print-right-margin*))

(defun fmt-row (data &optional stream)
  (format stream "| ~{~A~^ | ~} |~%" data))

(defun format-sxhash (code &optional stream)
  "Turn the fixnum value CODE into a human-friendly string. CODE should
be produced by `sxhash'."
  (let (r)
    (dotimes (i 8 r)
      (push (ldb (byte 8 (* i 8)) code) r))
    (format
     stream
     "~{~A~^-~}"
     (mapcar
      (lambda (x) (format nil "~{~(~2,'0x~)~}" x))
      (group r 2)))))

;;; Trees

;; from https://gist.github.com/WetHat/9682b8f70f0241c37cd5d732784d1577

;; Example:

;; (let ((tree '(A B1 B2 (B3 C1) C2)))
;;     ; enumerate all layout options and draw the tree for each one.
;;     (dolist (layout '(:up :centered :down))
;;         (format t "Layout = :~A~%" layout)
;;         (fmt-tree t tree :layout layout)))

;; Layout = :UP
;;  ╭─ C2
;;  │   ╭─ C1
;;  ├─ B3
;;  ├─ B2
;;  ├─ B1
;;  A
;; Layout = :CENTERED
;;  ╭─ B2
;;  ├─ B1
;;  A
;;  ├─ B3
;;  │   ╰─ C1
;;  ╰─ C2
;; Layout = :DOWN
;;  A
;;  ├─ B1
;;  ├─ B2
;;  ├─ B3
;;  │   ╰─ C1
;;  ╰─ C2

;;                       Unicode    plain ASCII representation
(defvar *space*      "    ")
(defvar *upper-knee* " ╭─ ") ; " .- "
(defvar *pipe*       " │  ") ; " |  "
(defvar *tee*        " ├─ ") ; " +- "
(defvar *lower-knee* " ╰─ ") ; " '- "

(defun format-tree-segments (node &key (layout :centered)
                                       (node-formatter #'write-to-string))
  (unless node
    (return-from format-tree-segments nil)) ; nothing to do here
  (setq node (ensure-cons node))
  (flet ((prefix-node-strings (child-node &key layout node-formatter
                                               (upper-connector *pipe*)
                                               (root-connector  *tee*)
                                               (lower-connector *pipe*))
           "A local utility to add connectors to a string representation
                 of a tree segment to connect it to other tree segments."
           (multiple-value-bind (u r l)
               (format-tree-segments child-node
                                     :layout         layout
                                     :node-formatter node-formatter)
                                        ; prefix tree segment with connector glyphs to connect it to
                                        ; other segments.
             (nconc
              (mapcar
               (lambda (str) (concatenate 'string upper-connector str))
               u)
              (list (concatenate 'string root-connector r))
              (mapcar
               (lambda (str) (concatenate 'string lower-connector str))
               l)))))
    (let* ((children (rest node))
           (pivot (case layout ; the split point of the list of children
                    (:up   (length children)) ; split at top
                    (:down 0)                 ; split at bottom
                    (otherwise (round (/ (length children) 2))))) ; bisect
           (upper-children (reverse (subseq children 0 pivot))) ; above root
           (lower-children (subseq children pivot))) ; nodes below root
      (values ; compile multiple value return of upper-children root lower children
       (when upper-children
         (loop with top = (prefix-node-strings (first upper-children)
                                               :layout layout
                                               :node-formatter node-formatter
                                               :upper-connector *space*
                                               :root-connector  *upper-knee*) ; top node has special connectors
               for child-node in (rest upper-children)
               nconc (prefix-node-strings child-node
                                          :layout layout
                                          :node-formatter node-formatter)
               into strlist
               finally (return (nconc top strlist))))
       (let ((root-name (funcall node-formatter (car node)))) ; root node
         (if (= 1 (length root-name))
             (concatenate 'string " " root-name) ; at least 2 chars needed
                                        ;else
             root-name))
       (when lower-children
         (loop for (head . tail) on lower-children
               while tail ; omit the last child
               nconc (prefix-node-strings head
                                          :layout layout
                                          :node-formatter node-formatter)
               into strlist
               finally (return
                         (nconc
                          strlist
                                        ; bottom node has special connectors
                          (prefix-node-strings head
                                               :layout layout
                                               :node-formatter  node-formatter
                                               :root-connector  *lower-knee*
                                               :lower-connector *space*)))))))))

(defun fmt-tree (stream root &key 
			     (plist nil)
			     (layout :centered)
                             (node-formatter #'write-to-string))
  (multiple-value-bind (u r l)
      (format-tree-segments (if plist (cons (car root) (group (cdr root) 2)) root)
                            :layout layout
                            :node-formatter node-formatter)
    (format stream "~{~A~%~}" (nconc u (list r) l))))

(defun human-readable-size (number)
  (check-type number integer)
  (loop for size in '(80 70 60 50 40 30 20 10)
        and unit in '("YB" "ZB" "EB" "PB" "TB" "GB" "MB" "KB")
        when (> (ash number (- size)) 0)
        do (return-from human-readable-size
             (format nil "~,2F ~A"
                     (float (/ number (ash 1 size)))
                     unit))))

;;; MOP
(defun describe-slot (name value &optional (max-slot-name-length 30) (stream t))
  (format stream "~%~VA = ~A" max-slot-name-length name (sb-impl::prin1-to-line value)))

;; FROM: sb-impl describe
(defun %describe-object (object stream)
  (let* ((class (class-of object))
	 (slotds (sb-mop:class-slots class))
	 (max-slot-name-length 30)
	 (plist nil))
    ;; Figure out a good width for the slot-name column.
    (flet ((adjust-slot-name-length (name)
	     (setf max-slot-name-length
		   (max max-slot-name-length (length (symbol-name name))))))
      (dolist (slotd slotds)
	(adjust-slot-name-length (sb-mop:slot-definition-name slotd))
	(push slotd (getf plist (sb-mop:slot-definition-allocation slotd)))))
    ;; Now that we know the width, we can print.
      (sb-int:doplist (allocation slots) plist
	(dolist (slotd (nreverse slots))
	  (describe-slot
	   (sb-mop:slot-definition-name slotd)
	   (sb-pcl::slot-value-for-printing object (sb-mop:slot-definition-name slotd)))))
    (unless slotds
      (format stream "~@:_No slots."))))

(defun print-slots (object &optional (stream t))
  (let ((*print-right-margin* (or *print-right-margin* 72))
	(*print-circle* t)
	(sb-impl::*print-circle-not-shared* t)
	(*print-pretty* t)
	(sb-impl::*suppress-print-errors*
	  (if (subtypep 'serious-condition sb-impl::*suppress-print-errors*)
	      sb-impl::*suppress-print-errors*
	      'serious-condition)))
    (%describe-object object stream)))

(defun format-slots (stream &rest slots)
  (let ((*print-right-margin* (or *print-right-margin* 72))
	(*print-circle* t)
	(sb-impl::*print-circle-not-shared* t)
	(*print-pretty* t)
	(sb-impl::*suppress-print-errors*
	  (if (subtypep 'serious-condition sb-impl::*suppress-print-errors*)
	      sb-impl::*suppress-print-errors*
	      'serious-condition)))
    (sb-int:doplist (k v) (print slots)
      (describe-slot (string k) v 30 stream))
    (force-output stream)))

;;; Box
;; TODO 2025-04-04: 
;; APL Box Formatting (BQN/Dyalog/J)
