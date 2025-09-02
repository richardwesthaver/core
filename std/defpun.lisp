;;; defpun.lisp --- Parallel Defuns

;; Based on LPARALLEL cognates

;;; Commentary:

;; This file defines the DEFPUN and DEFPTYPED macros. These macros work just
;; like DEFUN with the addition of PLET and PLET-IF forms which should be
;; viewed as 'lazily evaluated immutable references'.

;; ref: https://github.com/lmj/lparallel

;;; Code:
(in-package :std/async)

;;; Utils
(eval-always
  (definline unsplice (form)
    (if form (list form) nil))

  (defun item-predicate (item test test-not)
    (when (and test test-not)
      (error "Both :TEST and :TEST-NOT options given."))
    (when test-not
      (setf test (complement (std/curry:ensure-function test-not)))
      (setf test-not nil))
    (if test
        (let ((test (std/curry:ensure-function test)))
          (lambda (x)
            (funcall test item x)))
        (typecase item
          ((or number character)
           (lambda (x)
             (declare (optimize (speed 3) (safety 0)))
             (eql item x)))
          (otherwise
           (lambda (x)
             (declare (optimize (speed 3) (safety 0)))
             (eq item x)))))))


(defmacro dosequence ((var sequence &optional return) &body body)
  (with-gensyms (body-fn)
    `(block nil
       (flet ((,body-fn (,var) ,@body))
         (declare (dynamic-extent #',body-fn))
         (map nil #',body-fn ,sequence)
         ,@(unsplice (when return
                       `(let ((,var nil))
                          (declare (ignorable ,var))
                          ,return)))))))

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

(defmacro spawn (pool temp-vars form)
  (check-type pool symbol)
  `(std/thread::submit-raw-work
    (std/thread::work-lambda
      ;; work handler already established
      (unwind-protect (msetq ,temp-vars (std/thread::with-work-context ,form))
        (locally (declare (optimize (speed 3) (safety 0)))
          (std/thread::update-limiter-count (the thread-pool ,pool) 1))
       (values))
    ,pool)))

(defmacro spawn-work (pool spawn-binding-data)
  (check-type pool symbol)
  `(progn
     ,@(loop for (nil temp-vars form) in spawn-binding-data
             collect `(spawn ,pool ,temp-vars ,form))))

(defmacro exec-work (here-binding-datum)
  (destructuring-bind (client-vars temp-vars form) here-binding-datum
    (declare (ignore client-vars))
    `(msetq ,temp-vars ,form)))

(defmacro sync (pool spawn-binding-data)
  (check-type pool symbol)
  ;; reverse to check last spawn first
  (let ((temp-vars (reverse (temp-vars spawn-binding-data))))
    `(locally (declare (optimize (speed 3) (safety 3)))
       (loop with worker = *worker*
             while (or ,@(loop for temp-var in temp-vars
                               collect `(eq ,temp-var std/async::+no-result+)))
             do (thread-yield)
                (std/thread::steal-work* (the thread-pool ,pool) worker)))))

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

(defmacro %%%%plet (pool bindings body)
  (multiple-value-bind (binding-data null-bindings) (make-binding-data bindings)
    (destructuring-bind
          (here-binding-datum &rest spawn-binding-data) binding-data
      `(with-temp-bindings ,here-binding-datum ,spawn-binding-data
         (spawn-work ,pool ,spawn-binding-data)
         (exec-work ,here-binding-datum)
         (sync ,pool ,spawn-binding-data)
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

(defmacro %%%plet (pool predicate spawn-count bindings body)
  ;; Putting the body code into a shared dynamic-extent function
  ;; caused some slowdown, so reluctantly duplicate the body.
  `(with-lock-predicates
     :lock            (limiter-lock (the thread-pool ,pool))
     :predicate1      ,predicate
     :predicate2      (alive ,pool)
     :succeed/lock    (update-limiter-count/no-lock ,pool ,(- spawn-count))
     :succeed/no-lock (%%%%plet ,pool ,bindings ,body)
     :fail            (slet ,bindings ,@body)))

(defmacro %%plet (pool predicate bindings body)
  (let ((spawn-count (- (length (parse-bindings bindings)) 1)))
    (if (plusp spawn-count)
        `(%%%plet ,pool ,predicate ,spawn-count ,bindings ,body)
        `(slet ,bindings ,@body))))

(defmacro %plet (pool bindings &body body)
  `(%%plet ,pool
           (alive ,pool)
           ,bindings
           ,body))

(defmacro %plet-if (pool predicate bindings &body body)
  `(%%plet ,pool
           (and (alive ,pool) ,predicate)
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

(defun preduce-partial/vector (function sequence start size parts
                               &rest keyword-args)
  (declare (dynamic-extent keyword-args))
  (with-preduce-context size parts
    (loop for result-index from 0
          while (next-part)
          do (apply #'submit-indexed
                    result-index
                    #'reduce
                    function
                    sequence
                    :start (+ start (part-offset))
                    :end   (+ start (part-offset) (part-size))
                    keyword-args))))

(defun preduce-partial/list (function sequence start size parts
                             &rest keyword-args)
  (declare (dynamic-extent keyword-args))
  (with-preduce-context size parts
    (loop with subseq = (nthcdr start sequence)
          for result-index from 0
          while (next-part)
          do (apply #'submit-indexed
                    result-index
                    #'reduce
                    function
                    subseq
                    :end (part-size)
                    keyword-args)
             (setf subseq (nthcdr (part-size) subseq)))))

(defun %preduce-partial (function sequence start size parts
                         &rest keyword-args)
  (declare (dynamic-extent keyword-args))
  (etypecase sequence
    (vector (apply #'preduce-partial/vector
                   function sequence start size parts keyword-args))
    (list  (apply #'preduce-partial/list
                  function sequence start size parts keyword-args))))

(defun get-parts-hint (parts-hint)
  (cond (parts-hint
         (check-type parts-hint (integer 1 #.most-positive-fixnum))
         parts-hint)
        (t
         (worker-count*))))

(defmacro pop-plist (list)
  `(loop while (keywordp (first ,list))
         collect (pop ,list)
         collect (pop ,list)))

(defun %parse-options (args)
  (destructuring-bind (&key size parts) (pop-plist args)
    (values args size parts)))

(defun parse-options (args)
  (multiple-value-bind (seqs size parts) (%parse-options args)
    (unless seqs
      (error "Input sequence(s) for parallelization not found."))
    (unless size
      (setf size (find-min-length seqs)))
    (setf parts (get-parts-hint parts))
    (values seqs size parts)))

(defmacro with-parsed-options ((args size parts) &body body)
  `(multiple-value-bind (,args ,size ,parts) (parse-options ,args)
     ,@body))

(defun preduce/common (function sequence subsize
                       &key
                       key
                       from-end
                       (start 0)
                       end
                       (initial-value nil initial-value-given-p)
                       parts
                       recurse
                       partial)
  (declare (ignore end))
  (cond ((zerop subsize)
         (when partial
           (error "PREDUCE-PARTIAL given zero-length sequence"))
         (if initial-value-given-p
             initial-value
             (funcall function)))
        (t
         (let* ((parts-hint (get-parts-hint parts))
                (results    (apply #'%preduce-partial
                                   function sequence start subsize parts-hint
                                   :key key
                                   :from-end from-end
                                   (when initial-value-given-p
                                     (list :initial-value initial-value)))))
           (if partial
               results
               (let ((new-size (length results)))
                 (if (and recurse (>= new-size 4))
                     (apply #'preduce/common
                            function
                            results
                            new-size
                            :from-end from-end
                            :parts (min parts-hint (floor new-size 2))
                            :recurse recurse
                            (when initial-value-given-p
                              (list :initial-value initial-value)))
                     (reduce function results))))))))

(defun preduce (function sequence &rest args
                &key key from-end (start 0) end initial-value parts recurse)
  "Parallel version of `reduce'.

`preduce' subdivides the input sequence into `parts' number of parts
and, in parallel, calls `reduce' on each part. The partial results are
then reduced again, either by `reduce' (the default) or, if `recurse'
is non-nil, by `preduce'.

`parts' defaults to (kernel-worker-count).

`key' is thrown out while reducing the partial results. It applies to
the first pass only.

`start' and `end' have the same meaning as in `reduce'.

`from-end' means \"from the end of each part\".

`initial-value' means \"initial value of each part\"."
  (declare (ignore key from-end initial-value parts recurse))
  (declare (dynamic-extent args))
  (typecase sequence
    ((or vector list)
     (apply #'preduce/common
            function
            sequence
            (subsize sequence (length sequence) start end)
            args))
    (otherwise
     (apply #'reduce
            function
            sequence
            (std/list:remove-from-plist args :parts :recurse)))))

(defun preduce-partial (function sequence &rest args
                        &key key from-end (start 0) end initial-value parts)
  "Like `preduce' but only does a single reducing pass.

The length of `sequence' must not be zero.

Returns the partial results as a vector."
  (declare (ignore key from-end initial-value parts))
  (declare (dynamic-extent args))
  (apply #'preduce/common
         function
         sequence
         (subsize sequence (length sequence) start end)
         :partial t
         args))

;;;; pmap
(defun pmap-into/parts (map-into result-seq fn seqs size parts-hint)
  (let ((input-parts (make-input-parts seqs size parts-hint)))
    (multiple-value-bind (result-parts stitch)
        (make-result-parts result-seq size parts-hint)
      (unwind-protect
           (with-submit-counted
             (map nil
                  (lambda (result-part subseqs)
                    (submit-counted 'apply map-into result-part fn subseqs))
                  result-parts
                  input-parts)
             (receive-counted))
        (when stitch (funcall stitch))))))

(defun map-nil (&rest args)
  (declare (dynamic-extent args))
  (apply #'map nil args))

(defun maplist-into (result-list fn &rest lists)
  "A variation of map-into."
  (let ((fn (std/curry:ensure-function fn)))
    (apply #'mapl
           (lambda (result &rest args)
             ;; This is an inner loop.
             (declare (dynamic-extent args))
             (setf (car result) (apply fn args)))
           result-list
           lists)
    result-list))

(defun map-iterate (map size fn seqs)
  "A variation of (map nil ...)/mapc/mapl with size constrained.
Without a result to delineate sublist boundaries, we must enforce them
manually."
  (check-type size (integer 0))
  (let ((fn (std/curry:ensure-function fn))
        (index 0))
    (apply map
           (lambda (&rest args)
             ;; This is an inner loop.
             (declare (dynamic-extent args))
             (when (eql index size)
               (return-from map-iterate nil))
             (apply fn args)
             (incf index))
           seqs)))

(defun pmap-into/powder/array (result-seq fn seqs size)
  "When a sequence of size N is divided into N parts, it becomes powder."
  (with-submit-indexed size result-seq
    (let ((index 0))
      (map-iterate #'map-nil
                   size
                   (lambda (&rest args)
                     (declare (dynamic-extent args))
                     (apply #'submit-indexed index fn args)
                     (incf index))
                   seqs))
    (receive-indexed)))

(defun pmap-into/powder/list (map result-seq fn seqs size)
  (let ((result result-seq))
    (with-submit-counted
      (map-iterate map
                   size
                   (lambda (&rest args)
                     (submit-counted (let ((result result))
                                       (lambda ()
                                         (setf (car result) (apply fn args)))))
                     (setf result (cdr result)))
                   seqs)
      (receive-counted))))

(defun pmap-into/powder (map-into result-seq fn seqs size)
  (etypecase result-seq
    (array (pmap-into/powder/array result-seq fn seqs size))
    (list  (let ((map (if (eq map-into #'maplist-into) #'mapl #'map-nil)))
             (pmap-into/powder/list map result-seq fn seqs size)))))

(defun pmap-into/parsed (map-into result-seq fn seqs size parts-hint)
  (when (plusp size)
    (if (eql size (find-num-parts size parts-hint))
        (pmap-into/powder map-into result-seq fn seqs size)
        (pmap-into/parts  map-into result-seq fn seqs size parts-hint)))
  result-seq)

(defun pmap-into/unparsed (map-into result-seq fn seqs)
  (multiple-value-bind (seqs size parts-hint) (%parse-options seqs)
    (let* ((fn (std/curry:ensure-function fn))
           (initial-fill-pointer (and (arrayp result-seq)
                                      (array-has-fill-pointer-p result-seq)
                                      (fill-pointer result-seq)))
           (parts-hint (get-parts-hint parts-hint))
           (size (or size
                     (let ((limit (if initial-fill-pointer
                                      (array-total-size result-seq)
                                      (length result-seq))))
                       (if seqs
                           (min limit (find-min-length seqs))
                           limit)))))
      (flet ((main ()
               (if seqs
                   (pmap-into/parsed map-into
                                     result-seq fn seqs size parts-hint)
                   (pmap-into/parsed map-into
                                     result-seq
                                     (lambda (x)
                                       (declare (ignore x))
                                       (funcall fn))
                                     (list result-seq)
                                     size
                                     parts-hint))))
        (declare (dynamic-extent #'main))
        (if initial-fill-pointer
            (std/condition:unwind-protect-case ()
             (progn (setf (fill-pointer result-seq) size)
                    (main))
             (:abort (setf (fill-pointer result-seq) initial-fill-pointer)))
            (main))))))

(defun pmap-into (result-sequence function &rest sequences)
  "Parallel version of `map-into'. Keyword arguments `parts' and
`size' are also accepted (see `pmap')."
  (typecase result-sequence
    ((or array list)
     (pmap-into/unparsed #'map-into result-sequence function sequences))
    (t
     (apply #'map-into result-sequence function sequences)))
  result-sequence)

(defun pmap-iterate/parts (map fn seqs size parts-hint)
  (let ((input-parts (make-input-parts seqs size parts-hint)))
    (with-submit-counted
      (with-parts size parts-hint
        (dosequence (subseqs input-parts)
          (next-part)
          (submit-counted 'map-iterate map (part-size) fn subseqs)))
      (receive-counted))))

(defun pmap-iterate/powder (map fn seqs size)
  (with-submit-counted
    (map-iterate map
                 size
                 (lambda (&rest args)
                   (declare (dynamic-extent args))
                   (apply #'submit-counted fn args))
                 seqs)
    (receive-counted)))

(defun pmap-iterate (map fn seqs size parts-hint)
  (if (eql size (find-num-parts size parts-hint))
      (pmap-iterate/powder map fn seqs size)
      (pmap-iterate/parts  map fn seqs size parts-hint))
  nil)

(defun pmap/parsed (result-type function sequences size parts-hint)
  (if result-type
      (pmap-into/parsed #'map-into
                        (make-sequence result-type size)
                        function
                        sequences
                        size
                        parts-hint)
      ;; (pmap nil ...)
      (pmap-iterate #'map-nil function sequences size parts-hint)))

(defun pmap/unparsed (result-type function sequences)
  (with-parsed-options (sequences size parts-hint)
    (pmap/parsed result-type function sequences size parts-hint)))

(defun pmap (result-type function &rest sequences)
  "Parallel version of `map'. Keyword arguments `parts' and `size' are
also accepted.

The `parts' option divides each sequence into `parts' number of parts.
Default is (kernel-worker-count).

The `size' option limits the number of elements mapped to `size'. When
given, no `length' calls are made on the sequence(s) passed.

Warning: `size' must be less than or equal to the length of the
smallest sequence passed. It is unspecified what happens when that
condition is not met."
  (pmap/unparsed result-type function sequences))

(defun pmapcar (function &rest sequences)
  "Parallel version of `mapcar'. Keyword arguments `parts' and `size'
are also accepted (see `pmap').

Unlike `mapcar', `pmapcar' also accepts vectors."
  (pmap/unparsed 'list function sequences))

(defun pmaplist-into (result-list function &rest lists)
  "Like `pmaplist' but results are stored in `result-list'. Keyword
arguments `parts' and `size' are also accepted (see `pmap')."
  (pmap-into/unparsed #'maplist-into result-list function lists))

(defun pmaplist (function &rest lists)
  "Parallel version of `maplist'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (with-parsed-options (lists size parts-hint)
    (pmap-into/parsed
     #'maplist-into (make-list size) function lists size parts-hint)))

(defun pmapl (function &rest lists)
  "Parallel version of `mapl'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (with-parsed-options (lists size parts-hint)
    (pmap-iterate #'mapl function lists size parts-hint)
    (first lists)))

(defun pmapc (function &rest lists)
  "Parallel version of `mapc'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (with-parsed-options (lists size parts-hint)
    (pmap-iterate #'mapc function lists size parts-hint)
    (first lists)))

(defun pmapcan (function &rest lists)
  "Parallel version of `mapcan'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (declare (dynamic-extent lists))
  (apply #'nconc (apply #'pmapcar function lists)))

(defun pmapcon (function &rest lists)
  "Parallel version of `mapcon'. Keyword arguments `parts' and `size'
are also accepted (see `pmap')."
  (declare (dynamic-extent lists))
  (apply #'nconc (apply #'pmaplist function lists)))

(defun pmap-reduce (map-function reduce-function sequence
                    &rest args
                    &key start end initial-value parts recurse)
  "Equivalent to (preduce reduce-function sequence :key map-function ...)."
  (declare (ignore start end initial-value parts recurse))
  (declare (dynamic-extent args))
  (apply #'preduce reduce-function sequence :key map-function args))

;;; pquantifier
(defun pquantifier (quantifier predicate sequences bail)
  (with-parsed-options (sequences size parts-hint)
    (let ((input-parts (make-input-parts sequences size parts-hint)))
      (submit-with-cancel
        (dosequence (subseqs input-parts)
          (submit-cancelable 'apply quantifier predicate subseqs))
        (receive-cancelables (lambda (result)
                               (when (eq bail (when result t))
                                 (return-from pquantifier result)))))))
  (not bail))

(defun pevery (predicate &rest sequences)
  "Parallel version of `every'. Calls to `predicate' are done in
parallel, though not necessarily at the same time. Behavior is
otherwise indistinguishable from `every'.

Keyword arguments `parts' and `size' are also accepted (see `pmap')."
  (pquantifier #'every (std/curry:ensure-function predicate) sequences nil))

(defun psome (predicate &rest sequences)
  "Parallel version of `some'. Calls to `predicate' are done in
parallel, though not necessarily at the same time. Behavior is
otherwise indistinguishable from `some' except that any non-nil
predicate comparison result may be returned.

Keyword arguments `parts' and `size' are also accepted (see `pmap')."
  (pquantifier #'some (std/curry:ensure-function predicate) sequences t))

(defun pnotevery (predicate &rest sequences)
  "Parallel version of `notevery'. Calls to `predicate' are done in
parallel, though not necessarily at the same time. Behavior is
otherwise indistinguishable from `notevery'.

Keyword arguments `parts' and `size' are also accepted (see `pmap')."
  (declare (dynamic-extent sequences))
  (not (apply #'pevery predicate sequences)))

(defun pnotany (predicate &rest sequences)
  "Parallel version of `notany'. Calls to `predicate' are done in
parallel, though not necessarily at the same time. Behavior is
otherwise indistinguishable from `notany'.

Keyword arguments `parts' and `size' are also accepted (see `pmap')."
  (declare (dynamic-extent sequences))
  (not (apply #'psome predicate sequences)))

;;; pandor
(defmacro with-forms-submitted (forms &body body)
  `(submit-with-cancel
     ,@(loop for form in forms
             collect `(submit-cancelable (lambda () ,form)))
     ,@body))

(defmacro pand (&rest forms)
  "Parallel version of `and'. Forms in `forms' may be executed in
parallel, though not necessarily at the same time. If all forms
evaluate to true, then the result of any form may be returned."
  (with-gensyms (done result next-result)
    `(block ,done
       (with-forms-submitted ,forms
         (let ((,result nil))
           (receive-cancelables ,next-result
             (unless (setf ,result ,next-result)
               (return-from ,done nil)))
           ,result)))))

(defmacro por (&rest forms)
  "Parallel version of `or'. Forms in `forms' may be executed in
parallel, though not necessarily at the same time. Any form which
evaluates to non-nil may be returned."
  (with-gensyms (done result)
    `(block ,done
       (with-forms-submitted ,forms
         (receive-cancelables ,result
           (when ,result
             (return-from ,done ,result)))
         nil))))

(defun pcount-if (predicate sequence &key from-end (start 0) end key parts)
  "Parallel version of `count-if'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (let ((subsize (subsize sequence (length sequence) start end)))
    (if (zerop subsize)
        0
        (let ((predicate (std/curry:ensure-function predicate)))
          (flet ((maybe-inc (acc x)
                   (declare (fixnum acc))
                   (if (funcall predicate x)
                       (the fixnum (1+ acc))
                       acc)))
            (declare (ftype (function (fixnum t) fixnum) maybe-inc))
            (reduce #'+ (preduce/common #'maybe-inc
                                        sequence
                                        subsize
                                        :initial-value 0
                                        :from-end from-end
                                        :start start
                                        :key key
                                        :parts parts
                                        :partial t)))))))

(defun pcount-if-not (predicate sequence
                      &rest args
                      &key from-end start end key parts)
  "Parallel version of `count-if-not'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args)
           (ignore from-end start end key parts))
  (apply #'pcount-if (complement (std/curry:ensure-function predicate)) sequence args))

(defun pcount (item sequence
               &key from-end (start 0) end key test test-not parts)
  "Parallel version of `count'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (pcount-if (item-predicate item test test-not) sequence
             :from-end from-end :start start :end end :key key :parts parts))

(defun %pdotimes (size parts fn)
  (check-type size fixnum)
  (when (plusp size)
    (let ((fn (std/curry:ensure-function fn)))
      (flet ((compute-part (part-offset part-size)
               (declare (type fixnum part-offset part-size))
               (let ((index part-offset)
                     (end (+ part-offset part-size)))
                 (declare (type fixnum index end))
                 (loop while (< index end)
                       do (funcall fn index)
                          (incf index)))))
        (let ((parts (get-parts-hint parts))
              (channel (make-channel)))
          (with-parts size parts
            (loop while (next-part)
                  do (submit-work channel #'compute-part
                                  (part-offset) (part-size)))
            (std/seq:repeat (num-parts)
              (receive-result channel))))))))

(defonce pdotimes ((var &once count &optional result parts)
                         &body body)
;;   "Parallel version of `dotimes'.

;; The `parts' option divides the integer range into `parts' number of
;; parts. Default is (kernel-worker-count).

;; Unlike `dotimes', `pdotimes' does not define an implicit block named
;; nil."
  (with-parsed-body (body declares)
    `(progn
       (%pdotimes ,count ,parts (lambda (,var)
                                  ,@declares
                                  (tagbody ,@body)))
       (let ((,var (max ,count 0)))
         (declare (ignorable ,var))
         ,result))))

;;; pfind
(defmacro with-pfind-context (sequence start end parts &body body)
  (with-gensyms (top result)
    `(block ,top
       (with-parts
           (subsize ,sequence (length ,sequence) ,start ,end)
           (get-parts-hint ,parts)
         (submit-with-cancel
           ,@body
           (receive-cancelables 
            (lambda (,result)
              (when ,result
                (return-from ,top ,result))))
           nil)))))

(defun pfind-if/vector (predicate sequence
                        &key from-end (start 0) end key parts)
  (with-pfind-context sequence start end parts
    (loop with index = start
          while (next-part)
          do (submit-cancelable #'find-if
                                predicate
                                sequence
                                :from-end from-end
                                :start index
                                :end (+ index (part-size))
                                :key key)
             (incf index (part-size)))))

(defun pfind-if/list (predicate sequence
                      &key from-end (start 0) end key parts)
  (with-pfind-context sequence start end parts
    (loop with sublist = (nthcdr start sequence)
          while (next-part)
          do (submit-cancelable #'find-if
                                predicate
                                sublist
                                :from-end from-end
                                :end (part-size)
                                :key key)
             (setf sublist (nthcdr (part-size) sublist)))))

(defun pfind-if (predicate sequence
                 &rest args
                 &key from-end start end key parts)
  "Parallel version of `pfind-if'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args)
           (ignore from-end start end key parts))
  (let ((predicate (std/curry:ensure-function predicate)))
    (typecase sequence
      (vector    (apply #'pfind-if/vector predicate sequence args))
      (list      (apply #'pfind-if/list   predicate sequence args))
      (otherwise (apply #'find-if predicate sequence
                        (std/list:remove-from-plist args :parts))))))

(defun pfind-if-not (predicate sequence
                     &rest args
                     &key from-end start end key parts)
  "Parallel version of `pfind-if-not'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args)
           (ignore from-end start end key parts))
  (apply #'pfind-if (complement (std/curry:ensure-function predicate)) sequence args))

(defun pfind (item sequence
              &rest args
              &key from-end test test-not start end key parts)
  "Parallel version of `pfind'.

The `parts' option divides `sequence' into `parts' number of parts.
Default is (kernel-worker-count)."
  (declare (dynamic-extent args)
           (ignore from-end start end key parts))
  (apply #'pfind-if
         (item-predicate item test test-not)
         sequence
         (std/list:remove-from-plist args :test :test-not)))

;;;; defpun
(defmacro defun/wrapper (wrapper-name impl-name lambda-list &body body)
  (with-gensyms (args pool)
    (multiple-value-bind (wrapper-lambda-list expansion)
        (if (intersection lambda-list lambda-list-keywords)
            (values `(&rest ,args)
                    ``(apply (function ,',impl-name) ,,pool ,',args))
            (values lambda-list
                    ``(,',impl-name ,,pool ,@',lambda-list)))
      `(defun ,wrapper-name ,wrapper-lambda-list
         (macrolet ((call-impl (,pool) ,expansion))
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
  (if (or *worker* (boundp '*thread-pool*))
      (call-with-toplevel-handler impl)
      (call-inside-worker pool impl)))

(defvar *defpun-registration-lock* (make-mutex :name "defpun"))

(defconstant +checked-key+ 'checked-key)
(defconstant +unchecked-key+ 'unchecked-key)

(defvar *defpun-names* nil)

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
  (pushnew name *defpun-names*))

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
  (setf *defpun-names*
        (remove-if-not #'valid-registered-name-p *defpun-names*)))

(defun registered-macrolets (pool)
  (loop for name in *defpun-names*
        collect `(,name (&rest args)
                   `(,',(unchecked-name name) ,',pool ,@args))))

(defmacro declaim-defpun (&rest names)
  "See `defpun'."
  ;; This is used outside of the defpun macro.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (with-mutex (*defpun-registration-lock*)
       ,@(loop for name in names
               collect `(register-name ',name)))))

(defun delete-registered-names (names)
  ;; This is used outside of the defpun macro.
  (with-mutex (*defpun-registration-lock*)
    (setf *defpun-names* (set-difference *defpun-names* names))))

(defmacro with-parsed-body ((body declares &optional docstring) &body own-body)
  "Pop docstring and declarations off `body' and assign them to the
variables `docstring' and `declares' respectively. If `docstring' is
not present then no docstring is parsed."
  (if docstring
      `(multiple-value-bind (,body ,declares ,docstring)
           (sb-int:parse-body ,body t)
         ,@own-body)
      `(multiple-value-bind (,body ,declares) (sb-int:parse-body ,body t)
         ,@own-body)))

(defmacro define-defpun (defpun doc defun &rest types)
  `(defmacro ,defpun (name lambda-list ,@types &body body)
     ,doc
     (with-parsed-body (body declares docstring)
       (with-mutex (*defpun-registration-lock*)
         ;; these two calls may affect the registered macrolets in the
         ;; return form below
         (delete-stale-registrations)
         (register-name name)
         (with-gensyms (pool)
           `(progn
              (,',defun ,(unchecked-name name) (,pool ,@lambda-list)
                  ,,@(unsplice (when types ``(pool ,@,(first types))))
                  ,,@(unsplice (when types (second types)))
                ,@declares
                (declare (ignorable ,pool))
                (macrolet ((plet (bindings &body body)
                             `(%plet ,',pool ,bindings ,@body))
                           (plet-if (predicate bindings &body body)
                             `(%plet-if ,',pool ,predicate ,bindings ,@body))
                           ,@(registered-macrolets pool))
                  ,@body))
              (defun/wrapper ,name ,(unchecked-name name) ,lambda-list
                ,@(unsplice docstring)
                (let ((,pool (check-thread-pool)))
                  (call-impl-fn ,pool (lambda () (call-impl ,pool)))))
              (eval-when (:load-toplevel :execute)
                (with-mutex (*defpun-registration-lock*)
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

;; FIX 2025-08-18: 
(define-defpun defptyped
  "Typed version of DEFPUN.

ARG-TYPES is an unevaluated list of argument types.

RETURN-TYPE is an unevaluated form of the return type, possibly indicating
multiple values as in (values fixnum float).

(As a technical point, if RETURN-TYPE contains no lambda list keywords then
the return type given to ftype will be additionally constrained to match the
number of return values specified.)"
  deftyped
  arg-types
  return-type)
