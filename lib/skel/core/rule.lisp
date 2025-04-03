;;; rule.lisp --- Skel Rule Objects

;; 

;;; Code:
(in-package :skel/core/obj)

;;; Rule
(defstruct (sk-rule (:constructor %make-sk-rule (target source recipe)))
"Maps a SOURCE to a corresponding TARGET
via the special form stored in RECIPE."
  (target "" :type string)
  (source nil :type list)
  (recipe nil :type list))

(declaim (inline make-sk-rule))
(defun make-sk-rule (target &optional source recipe)
  (%make-sk-rule
   (etypecase target 
     (string target)
     (symbol (string-downcase target)))
   source
   (multiple-value-bind (form _ doc) (parse-body recipe :documentation t)
     ;; TODO 2025-02-25: figure out where to put the docstring - hash,compare,cache
     (declare (ignore _ doc))
     form)))

(defmethod sk-new ((self (eql :rule)) &rest args)
  (declare (ignore self))
  (apply #'sk-new 'sk-rule args))

(defmethod id ((self sk-rule))
  (sxhash (list (sk-rule-target self) (sk-rule-source self))))

(defmethod write-ast ((self sk-rule) stream &key (pretty t) (case :downcase) &allow-other-keys)
  (write `(,(sk-rule-target self) ,(sk-rule-source self) ,@(sk-rule-recipe self)) :stream stream :pretty pretty :case case :readably t :array t :escape t))

(defmethod print-object ((self sk-rule) stream)
  (print-unreadable-object (self stream)
    (format stream "~A ~A" (sk-class-name self t) (sk-rule-target self))
    (when-let ((source (sk-rule-source self)))
      (format stream " ~A" (mapcar 'string-downcase source)))))

;; Note that SK-RUN directly on a rule currently does NOT touch the sources.
(defmethod sk-run ((self sk-rule))
  (with-slots (recipe) self
    (mapcar (lambda (x)
	      (etypecase x
		((or symbol function) (funcall x :output t))
		(t (funcall (sb-cltl2:enclose `(lambda () ,x))))))
	    recipe)))

(defmethod sk-write ((self sk-rule) stream)
  (with-slots (target source recipe) self
    (write-string (sk-rule-target target) stream) ;; target isn't typep SK-OBJECT
    (write (sk-rule-source self) :stream stream)
    (write (sk-rule-recipe self) :stream stream)))

(defun sk-make (obj &rest rules)
  (if rules
      (mapc
       (lambda (r) 
	 (when-let ((rule (sk-find r obj)))
	   (sk-run-with-sources obj rule)))
       rules)
      (unless (sequence:emptyp (sk-rules obj))
	(let ((rule (aref (sk-rules obj) 0)))
	  (if (sk-rule-source rule)
	      (sk-make obj rule)
	      (sk-run rule))))))

(defun sk-run-with-sources (obj rule)
  (declare (sk-rule rule))
  (when-let ((sources (and rule (sk-rule-source rule))))
    (mapcar
     (lambda (src)
       (if-let* ((sr (sk-find src obj)))
		;; TODO: check if we need to rerun sources
		(sk-make obj sr)
		(error "unhandled source: ~A for rule ~A" src rule)))
     sources))
  (sk-run rule))
