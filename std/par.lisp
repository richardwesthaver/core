;;; par.lisp --- Parallelized versions of Common Lisp functions

;; Based on LPARALLEL cognates

;;; Commentary:

;; ref: https://github.com/lmj/lparallel

;;; Code:
(in-package :std/par)

;; TODO 2025-04-27: 
;;; plet
(defmacro msetq (vars form)
  (if (= 1 (length vars))
      `(setq ,(first vars) ,form)
      `(multiple-value-setq ,vars ,form)))

(defun client-vars (binding-data)
  (reduce #'append binding-data :key #'car))

(defun temp-vars (binding-data)
  (reduce #'append binding-data :key #'cadr))

(defun primary-temp-vars (binding-data)
  (loop for (nil temp-vars nil) in binding-data
        collect (first temp-vars)))

(defmacro with-temp-bindings (here-binding-datum spawn-binding-data &body body)
  `(let (,@(temp-vars (list here-binding-datum))
         ,@(loop for var in (temp-vars spawn-binding-data)
                 collect `(,var std/async::+no-result+)))
     ,@body))

(defmacro with-client-bindings (binding-data null-bindings &body body)
  `(let (,@null-bindings
         ,@(mapcar #'list
                   (client-vars binding-data)
                   (temp-vars binding-data)))
     ,@body))

(defmacro spawn (kernel temp-vars form)
  (check-type kernel symbol)
  `(std/thread::submit-raw-work
    (std/thread::work-lambda
      ;; task handler already established
      (unwind-protect (msetq ,temp-vars (std/thread::with-work-context ,form))
        (locally (declare (optimize (speed 3) (safety 0)))
          (std/thread::update-limiter-count (the thread-pool ,kernel) 1))
       (values))
    ,kernel)))

(defmacro spawn-tasks (kernel spawn-binding-data)
  (check-type kernel symbol)
  `(progn
     ,@(loop for (nil temp-vars form) in spawn-binding-data
             collect `(spawn ,kernel ,temp-vars ,form))))

(defmacro exec-task (here-binding-datum)
  (destructuring-bind (client-vars temp-vars form) here-binding-datum
    (declare (ignore client-vars))
    `(msetq ,temp-vars ,form)))

(defmacro sync (kernel spawn-binding-data)
  (check-type kernel symbol)
  ;; reverse to check last spawn first
  (let ((temp-vars (reverse (temp-vars spawn-binding-data))))
    `(locally (declare (optimize (speed 3) (safety 3)))
       (loop with worker = *worker*
             while (or ,@(loop for temp-var in temp-vars
                               collect `(eq ,temp-var std/async::+no-result+)))
             do #+lparallel.with-green-threads (thread-yield)
                (steal-work (the kernel ,kernel) worker)))))

(defmacro scan-for-errors (binding-data)
  ;; a wrapped error would only appear as the primary return value
  `(locally (declare (optimize (speed 3) (safety 3)))
     ,@(loop for temp-var in (primary-temp-vars binding-data)
             collect `(when (typep ,temp-var 'wrapped-error)
                        (unwrap-result ,temp-var)))))

(defun make-temp-var (var)
  (gensym (symbol-name var)))

(defun make-binding-datum (mv-binding)
  (destructuring-bind (vars form) mv-binding
    `(,vars ,(mapcar #'make-temp-var vars) ,form)))

(defun make-binding-data (bindings)
  (multiple-value-bind (mv-bindings null-bindings) (parse-bindings bindings)
    (values (mapcar #'make-binding-datum mv-bindings)
            null-bindings)))

(defmacro %%%%plet (kernel bindings body)
  (multiple-value-bind (binding-data null-bindings) (make-binding-data bindings)
    (destructuring-bind
          (here-binding-datum &rest spawn-binding-data) binding-data
      `(with-temp-bindings ,here-binding-datum ,spawn-binding-data
         (spawn-tasks ,kernel ,spawn-binding-data)
         (exec-task ,here-binding-datum)
         (sync ,kernel ,spawn-binding-data)
         (scan-for-errors ,spawn-binding-data)
         (with-client-bindings ,binding-data ,null-bindings
           ,@body)))))

(defmacro with-lock-predicates (&key lock predicate1 predicate2
                                succeed/lock succeed/no-lock fail)
  (with-gensyms (top fail-tag)
    `(block ,top
       (tagbody
          (when ,predicate1
            (with-spin-lock-held (,lock)
              (if ,predicate2
                  ,succeed/lock
                  (go ,fail-tag)))
            (return-from ,top ,succeed/no-lock))
        ,fail-tag
          (return-from ,top ,fail)))))

(defun parse-bindings (bindings)
  (let ((mv-bindings nil)
        (null-bindings nil))
    (dolist (binding bindings)
      (etypecase binding
        (cons (if (= 1 (length binding))
                  (dolist (var (std/list:ensure-list (first binding)))
                    (push var null-bindings))
                  (destructuring-bind (var-or-vars form) binding
                    (push `(,(std/list:ensure-list var-or-vars) ,form)
                          mv-bindings))))
        (symbol (push binding null-bindings))))
    (values (reverse mv-bindings)
            (reverse null-bindings))))

(defmacro %%%plet (kernel predicate spawn-count bindings body)
  ;; Putting the body code into a shared dynamic-extent function
  ;; caused some slowdown, so reluctantly duplicate the body.
  `(with-lock-predicates
       :lock            (limiter-lock (the kernel ,kernel))
       :predicate1      ,predicate
       :predicate2      (accept-task-p ,kernel)
       :succeed/lock    (update-limiter-count/no-lock ,kernel ,(- spawn-count))
       :succeed/no-lock (%%%%plet ,kernel ,bindings ,body)
       :fail            (slet ,bindings ,@body)))

(defmacro %%plet (kernel predicate bindings body)
  (let ((spawn-count (- (length (parse-bindings bindings)) 1)))
    (if (plusp spawn-count)
        `(%%%plet ,kernel ,predicate ,spawn-count ,bindings ,body)
        `(slet ,bindings ,@body))))

(defmacro %plet (kernel bindings &body body)
  `(%%plet ,kernel
           (accept-task-p ,kernel)
           ,bindings
           ,body))

(defmacro %plet-if (kernel predicate bindings &body body)
  `(%%plet ,kernel
           (and (accept-task-p ,kernel) ,predicate)
           ,bindings
           ,body))

;;; Utils
(defun zip-vector (seqs)
  "Return a vector containing zipped SEQS."
  (apply #'map 'vector #'list seqs))

(defun find-min-length (seqs)
  "Find and return the sequence of minimum length in SEQS."
  (reduce #'min seqs :key #'length))

(defun subsize (seq size start end)
  "Return the length of a subseq of SEQ with given SIZE, erroring if (START
. END) is a bad range."
  (let ((ret (- (or end size) start)))
    (when (or (minusp ret) (> ret size))
      (error "Bad range for seq ~A: :start ~A :end ~A"
             seq start end))
    ret))

;;; Subdivide
(defun find-num-parts (size parts-hint)
  (multiple-value-bind (quo rem) (floor size parts-hint)
    (values (if (zerop quo) rem parts-hint) quo rem)))

(defmacro with-parts (seq-size parts-hint &body body)
  (with-gensyms (quo rem index num-parts part-offset part-size)
    `(multiple-value-bind
           (,num-parts ,quo ,rem) (find-num-parts ,seq-size ,parts-hint)
       (declare (fixnum ,num-parts ,quo ,rem))
       (let ((,index 0)
             (,part-offset 0)
             (,part-size 0))
         (declare (fixnum ,index ,part-offset ,part-size))
         (flet ((next-part ()
                  (when (< ,index ,num-parts)
                    (unless (zerop ,index)
                      (incf ,part-offset ,part-size))
                    (setf ,part-size (if (< ,index ,rem) (1+ ,quo) ,quo))
                    (incf ,index)))
                (part-size   () ,part-size)
                (part-offset () ,part-offset)
                (num-parts   () ,num-parts))
           (declare (inline part-size part-offset num-parts)
                    (ignorable #'part-size #'part-offset #'num-parts))
           ,@body)))))

(defun subdivide-array (array size parts-hint)
  (with-parts size parts-hint
    (map-into (make-array (num-parts))
              (lambda ()
                (next-part)
                (make-array (part-size)
                            :displaced-to array
                            :displaced-index-offset (part-offset)
                            :element-type (array-element-type array))))))

(defun subdivide-list (list size parts-hint)
  (with-parts size parts-hint
    (loop with p = list
          while (next-part)
          collect p
          do (setf p (nthcdr (part-size) p)))))

(defun subdivide-list/slice (list size parts-hint)
  (with-parts size parts-hint
    (loop with p = list
          while (next-part)
          collect p into firsts
          collect (prog1 (setf p (nthcdr (1- (part-size)) p))
                    (setf p (prog1 (cdr p) (setf (cdr p) nil)))) into lasts
          finally (return (values firsts
                                  (lambda ()
                                    ;; stitch it back together
                                    (loop for last  in lasts
                                          for first in (cdr firsts)
                                          do (setf (cdr last) first)
                                          finally (setf (cdr last) p))))))))

(defun make-parts (result size parts-hint &key slicep)
  (if (listp result)
      (funcall (if slicep #'subdivide-list/slice #'subdivide-list)
               result size parts-hint)
      (subdivide-array result size parts-hint)))

(defun make-result-parts (result size parts-hint)
  "Subdivide the result sequence. For a list, delineate boundaries by slicing."
  (make-parts result size parts-hint :slicep t))

(defun make-input-parts (sequences size parts-hint)
  "Subdivide and interleave sequences for parallel mapping."
  (zip-vector (mapcar (lambda (seq) (make-parts seq size parts-hint))
                      sequences)))

;;; Reduce
(defmacro with-preduce-context (size parts &body body)
  (with-gensyms (results)
    `(with-parts ,size ,parts
       (let ((,results (make-array (num-parts))))
         (with-submit-indexed (num-parts) ,results
           ,@body
           (receive-indexed))))))

;;;; defpun
(defmacro defun/wrapper (wrapper-name impl-name lambda-list &body body)
  (with-gensyms (args kernel)
    (multiple-value-bind (wrapper-lambda-list expansion)
        (if (intersection lambda-list lambda-list-keywords)
            (values `(&rest ,args)
                    ``(apply (function ,',impl-name) ,,kernel ,',args))
            (values lambda-list
                    ``(,',impl-name ,,kernel ,@',lambda-list)))
      `(defun ,wrapper-name ,wrapper-lambda-list
         (macrolet ((call-impl (,kernel) ,expansion))
           ,@body)))))

(defun call-with-toplevel-handler (fn)
  (declare (optimize (speed 3) (safety 3)))
  (declare (type function fn))
  (let* ((results (multiple-value-list (std/thread::call-with-work-handler fn)))
         (first (first results)))
    (when (typep first 'std/condition:wrapped-error)
      (std/thread::unwrap-result first))
    (values-list results)))

(defun call-inside-worker (pool fn)
  (declare (optimize (speed 3) (safety 3)))
  (declare (type function fn))
  (let ((channel (make-instance 'channel :pool pool)))
    (std/thread::submit-work channel (lambda () (multiple-value-list (funcall fn))))
    (values-list (std/thread::receive-result channel))))

(defun call-impl-fn (pool impl)
  (declare (optimize (speed 3) (safety 3)))
  (declare (type function impl))
  (if (or std/thread::*worker* (boundp '*kernel*))
      (call-with-toplevel-handler impl)
      (call-inside-worker pool impl)))

(declaim (inline unsplice))
(defun unsplice (form)
  (if form (list form) nil))

(defvar *registration-lock* (make-mutex :name "registration"))

(defconstant +checked-key+ 'checked-key)
(defconstant +unchecked-key+ 'unchecked-key)

(defvar *registered-names* nil)

(defun symbolicate/package (package &rest string-designators)
  "Concatenate `string-designators' then intern the result into `package'."
  (let ((*package* (find-package package)))
    (apply #'symbolicate string-designators)))

(defun symbolicate/no-intern (&rest string-designators)
  "Concatenate `string-designators' then make-symbol the result."
  (format-symbol nil "~{~a~}" string-designators))

(defun unchecked-name (name)
  ;; We could intern this into a private package and maintain an alist
  ;; of (public . private) package pairs, but that seems
  ;; over-engineered. Anonymous packages don't exist anyway.
  (sb-int:symbolicate (package-name (symbol-package name)) '#:%%%%.defpun. name))

(defun register-name (name)
  (pushnew name *registered-names*))

(defun register-fn (name)
  (setf (get name +checked-key+) (symbol-function name))
  (setf (get name +unchecked-key+) (symbol-function (unchecked-name name))))

(defun registered-fn-p (name)
  (get name +checked-key+))

(defun valid-registered-fn-p (name)
  (and (fboundp name)
       (eq (symbol-function name)
           (get name +checked-key+))
       (fboundp (unchecked-name name))
       (eq (symbol-function (unchecked-name name))
           (get name +unchecked-key+))))

;;; a name may be registered without having a corresponding function
(defun valid-registered-name-p (name)
  (and (symbol-package name)
       (or (not (registered-fn-p name))
           (valid-registered-fn-p name))))

(defun delete-stale-registrations ()
  (setf *registered-names*
        (remove-if-not #'valid-registered-name-p *registered-names*)))

(defun registered-macrolets (kernel)
  (loop for name in *registered-names*
        collect `(,name (&rest args)
                   `(,',(unchecked-name name) ,',kernel ,@args))))

(defmacro declaim-defpun (&rest names)
  "See `defpun'."
  ;; This is used outside of the defpun macro.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (with-mutex (*registration-lock*)
       ,@(loop for name in names
               collect `(register-name ',name)))))

(defun delete-registered-names (names)
  ;; This is used outside of the defpun macro.
  (with-mutex (*registration-lock*)
    (setf *registered-names* (set-difference *registered-names* names))))

(defmacro with-parsed-body ((body declares &optional docstring) &body own-body)
  "Pop docstring and declarations off `body' and assign them to the
variables `docstring' and `declares' respectively. If `docstring' is
not present then no docstring is parsed."
  (if docstring
      `(multiple-value-bind (,body ,declares ,docstring)
           (sb-int:parse-body ,body t)
         ,@own-body)
      `(multiple-value-bind (,body ,declares) (parse-body ,body)
         ,@own-body)))

(defmacro define-defpun (defpun doc defun &rest types)
  `(defmacro ,defpun (name lambda-list ,@types &body body)
     ,doc
     (with-parsed-body (body declares docstring)
       (with-mutex (*registration-lock*)
         ;; these two calls may affect the registered macrolets in the
         ;; return form below
         (delete-stale-registrations)
         (register-name name)
         (with-gensyms (kernel)
           `(progn
              (,',defun ,(unchecked-name name) (,kernel ,@lambda-list)
                  ,,@(unsplice (when types ``(kernel ,@,(first types))))
                  ,,@(unsplice (when types (second types)))
                ,@declares
                (declare (ignorable ,kernel))
                (macrolet ((plet (bindings &body body)
                             `(%plet ,',kernel ,bindings ,@body))
                           (plet-if (predicate bindings &body body)
                             `(%plet-if ,',kernel ,predicate ,bindings ,@body))
                           ,@(registered-macrolets kernel))
                  ,@body))
              (defun/wrapper ,name ,(unchecked-name name) ,lambda-list
                ,@(unsplice docstring)
                (let ((,kernel (check-kernel)))
                  (call-impl-fn ,kernel (lambda () (call-impl ,kernel)))))
              (eval-when (:load-toplevel :execute)
                (with-mutex (*registration-lock*)
                  (register-fn ',name)))
              ',name))))))

(define-defpun defpun
  "`defpun' defines a function which is specially geared for
fine-grained parallelism. If you have many small tasks which bog down
the system, `defpun' may help.

The syntax of `defpun' matches that of `defun'. The difference is that
`plet' and `plet-if' take on new meaning inside `defpun'. The symbols
in the binding positions of `plet' and `plet-if' should be viewed as
lazily evaluated immutable references.

Inside a `defpun' form the name of the function being defined is a
macrolet, as are the names of other functions which were defined by
`defpun'. Thus using #' on them is an error. Calls to functions
defined by `defpun' entail more overhead when the caller lies outside
a `defpun' form.

A `defpun' function must exist before it is referenced inside another
`defpun' function. If this is not possible--for example if func1 and
func2 reference each other--then use `declaim-defpun' to specify
intent:

    (declaim-defpun func1 func2)
"
  defun)

(define-defpun defpun*
  "Typed version of DEFPUN.

ARG-TYPES is an unevaluated list of argument types.

RETURN-TYPE is an unevaluated form of the return type, possibly indicating
multiple values as in (values fixnum float).

(As a technical point, if RETURN-TYPE contains no lambda list keywords then
the return type given to ftype will be additionally constrained to match the
number of return values specified.)"
  defun*
  arg-types
  return-type)
