;;; loop.lisp --- Tensor Loop Utils

;; 

;;; Commentary:

;; The MATLISP src is mostly undocumented, but as it turns out the FOR-MOD
;; clause is not possible in SB-LOOP::LOOP without significant
;; hackery. Instead, we observe that every FOR-MOD clause depends on a
;; WITH-ITERATOR value such as :STRIDE or :MINOR - these are what we will
;; translate into Loop Paths (FOR X BEING THE IDX OF Y WITH-ITERATOR ((:STRIDE (())))).

;;; Code:
(in-package :obj/tensor)

;;The scheme for this iterator was obtained from FEMLISP.
(defmacro mod-update ((idx init dims &key order uplo) &rest body)
  "IDX is an index which is updated from INIT based on DIMS.

ORDER is one of :ROW-MAJOR or :COL-MAJOR.
UPLO is one of :UL :L :LO :U :UO."
  (let* ((uplo (or uplo :ul))
         (order (or order (case uplo ((:u :uo) :col-major) ((:l :lo) :row-major)) *default-stride-ordering*)))
    (assert (null (remove-if #'(lambda (x) (member (first x) '(:update :reset))) body)) nil)
    (using-gensyms (decl (idx init dims) (count))
      `(let (,@decl)
         (declare (type index-store-vector ,idx ,dims))
         (loop for ,count of-type index-type
                  ,@(ecase order
                      (:row-major `(:from (1- (length ,idx)) :downto 0))
                      (:col-major `(:from 0 :below (length ,idx))))
               do
                  (if ,(recursive-append
                        (ecase uplo
                          (:ul nil)
                          (:l `(or (and (> ,count 0) (= (aref ,idx ,count) (aref ,idx (1- ,count))))))
                          (:lo `(or (and (> ,count 0) (= (aref ,idx ,count) (1- (aref ,idx (1- ,count)))))))
                          (:u `(or (and (< ,count (1- (length ,idx))) (= (aref ,idx ,count) (aref ,idx (1+ ,count))))))
                          (:uo `(or (and (< ,count (1- (length ,idx))) (= (aref ,idx ,count) (1- (aref ,idx (1+ ,count))))))))
                        `(= (1+ (aref ,idx ,count)) (aref ,dims ,count)))
                      (progn
                        ,@(mapcar 
                           #'(lambda (reset) `(let (,@(zip (second reset) (list count idx init dims))) ,@(cddr reset)))
                           (remove-if-not #'(lambda (x) (eql (first x) :reset)) body))
                        (setf (aref ,idx ,count) (aref ,init ,count)))
                      (progn
                        ,@(mapcar 
                           #'(lambda (update) 
                               `(let (,@(zip (second update) (list count idx init dims))) ,@(cddr update)))
                           (remove-if-not #'(lambda (x) (eql (first x) :update)) body))
                        (incf (aref ,idx ,count))
                        (return t))))))))

;;; FOR X BEING THE Y OF Z
;; TODO: docs - return value spec
(defgeneric for-index-iterator (clause-name init dims args))
;; (loop for y being the idx in X below (dims) using (iterator order ul) ...)
(defun loop-index-iteration-path (variable data-type prep-phrases)
  (declare (ignore data-type))
  (mumble "variable: ~A" variable)
  (mumble "prep: ~A" prep-phrases)
  (destructuring-bind (initial dimensions &optional iterator %uplo %order) prep-phrases
    (binding-gensyms (gm)
      (let ((iterable (when iterator
                        ;; strides = (cadadr iterator)
                        (for-index-iterator (keywordicate (caadr iterator))
                                            (gm init)
                                            (gm dims)
                                            (cadadr iterator))))
            (init (cadr initial))
            (uplo (cadr %uplo))
            (order (cadr %order)))
        ;; TODO (print (sb-loop::loop-named-var :stride))
        ;; wrappers come AFTER bindings
        (when iterable
          (push `(let* ,(first iterable)
                   ,@(second iterable))
                (sb-loop::wrappers sb-loop::*loop*)))
        (let ((var (if (numberp init)
                       `(t.store-allocator index-store-vector (length (coerce ,(cadr dimensions) 'index-store-vector)) :initial-element ,init)
                       `(coerce ,init 'index-store-vector))))
          ;; TODO types
          `(((,(gm dims) (coerce ,(cadr dimensions) 'index-store-vector) index-store-vector)
             (,variable ,(copy-seq var) index-store-vector)
             (,(gm init) ,var index-store-vector)
             (,(gm %init))
             ;; iterator bindings
             ;; ,@(print (mapcar (lambda (x) (list (car x))) (first iterable)))
             )
            ((assert (ziprm (= length) (,(gm init) ,(gm dims)))))
            () ;pre-test
            ()
            ;; (,@(print (first iterable))) ; psteps
            (not (if ,(gm %init)
                     (with-optimization (:speed 3 :safety 0) ;post-test
                       (mod-update (,variable
                                    ,(gm init) 
                                    ,(gm dims) 
                                    :order ,order 
                                    :uplo ,uplo)
                                   ,@(cddr iterable)))
                     (setf ,(gm %init) t)))
            ())))))) ;post-steps

(sb-loop::add-loop-path '(idx index) 'loop-index-iteration-path *loop-ansi-universe*
                        :preposition-groups '((:from :below) (:with-iterator :with-iter) (:uplo) (:order))
                        :inclusive-permitted nil)

;; (defmethod sequence:make-sequence-iterator ((self tensor)))

(defmethod for-index-iterator ((clause-name (eql :stride)) init dims strides)
  (binding-gensyms (gm gf)
    (list 
     (mapcan #'(lambda (x)
                 `((,(gf (first x)) ,(second x))
                   (,(first x) (+ (the fixnum ,(or (third x) 0))
                                  (loop :for ,(gm i) :of-type index-type :from 0 :below (length ,init)
                                        :summing (the index-type (* (aref ,(gf (first x)) ,(gm i)) (aref ,init ,(gm i)))) :of-type index-type)))))
             strides)
     `((declare (index-store-vector ,@(mapcar #'(lambda (x) (gf (first x))) strides))
                (index-type ,@(mapcar #'car strides))
                (ignorable ,@(mapcar #'car strides)))
       (assert (ziprm (= length) (,dims ,@(mapcar #'(lambda (x) (gf (first x))) strides)))))
     `(:update (,(gm count) ,(gm idx) ,(gm init) ,(gm dims))
               (declare (ignore ,(gm idx) ,(gm init) ,(gm dims)))
               ,@(mapcar #'(lambda (x) `(incf ,(first x) (aref ,(gf (first x)) ,(gm count)))) strides))
     `(:reset (,(gm count) ,(gm idx) ,(gm init) ,(gm dims))
              (declare (ignore ,(gm dims)))
              ,@(mapcar #'(lambda (x) `(decf ,(first x) (the index-type (* (aref ,(gf (first x)) ,(gm count)) (- (aref ,(gm idx) ,(gm count)) (aref ,(gm init) ,(gm count))))))) strides)))))

(defmethod for-index-iterator ((clause-name (eql :general)) init dims body)
  body)

;;; OFFSET-REF
(defmacro offset-ref (decl &rest body)
  (let ((stack (mapcar #'(lambda (x) (declare (ignorable x)) (list (gensym "sto") (gensym))) decl)))

    `(lety (,@(mapcar #'(lambda (x s)
                          (declare (sb-ext:muffle-conditions style-warning))
                          (letv* (((ref offset tensor &key type) x))
                            `(,(second s) ,tensor ,@(when type `(:type ,type)))))
                      decl stack))
       (lety (,@(mapcar #'(lambda (x s) 
                            (declare (sb-ext:muffle-conditions style-warning))
                            (letv* (((ref offset tensor &key type) x))
                              `(,(first s) (store ,(second s)) ,@(when type `(:type ,(store-type type))))))
                        decl stack))
         (symbol-macrolet (,@(mapcar #'(lambda (x s)
                                         (declare (sb-ext:muffle-conditions style-warning))
                                         (letv* (((ref offset tensor &key type) x))
                                           `(,ref ,(if type
                                                       `(the ,(field-type type) (t/store-ref ,type ,(first s) ,offset))
                                                       `(store-ref ,(second s) ,(first s))))))
                                     decl stack))
           ,@body)))))

;;; DOREFS
(defmacro dorefs ((idx dims &key (order *default-stride-ordering* loop-ordering-p) (uplo :ul)) (&rest ref-decls) &rest body)
  (let* ((tsyms (zipsym (mapcar #'second ref-decls)))
         (rsyms (mapcar #'car ref-decls))
         (types (mapcar #'(lambda (x) (destructuring-bind (ref ten &key type) x
                                        (declare (ignore ref ten))
                                        type))
                        ref-decls))
         (ssyms (mapcar #'(lambda (x y) (declare (ignorable x)) (when y `(,(gensym) (slot-value ,(car x) 'store)))) tsyms types))
         (osyms (mapcar #'(lambda (y) (when y (gensym))) types)))
    (using-gensyms (decl (dims) (lst))
      `(lety* (,@decl
                  ,@(mapcar #'(lambda (x y) (if y (append x `(:type ,y)) x)) tsyms types))
         ;; (declare (type index-store-vector ,dims))
         (lety ((,lst (make-list (length ,dims) :initial-element 0))
                ,@(remove-if #'null (mapcar #'(lambda (x y) (when y (append x `(:type ,(store-type y))))) ssyms types)))
           (loop for ,idx being the idx
                 from ,(case uplo
                         (:uo `(append (make-list (1- (length ,dims)) :initial-element 0) (list 1)))
                         (:lo `(append (list 1) (make-list (1- (length ,dims)) :initial-element 0)))
                         (t 0))
                 below ,dims 
                 with-iterator 
                    (:stride (,@(remove-if #'null 
                                           (mapcar 
                                            #'(lambda (of ten typ) 
                                                (when typ `(,of (strides ,(car ten)) 
                                                                (head ,(car ten)))))
                                            osyms tsyms types))))
                    ,@(when loop-ordering-p `(order ,order))
                 uplo ,uplo
                 do (copy-vector-to-list ,idx ,lst)
                 do (symbol-macrolet 
                        (,@(mapcar #'(lambda (ref sto ten of typ) 
                                       (list ref 
                                             (if typ
                                                 `(the ,(field-type typ) 
                                                       (t.store-ref 
                                                        ,typ 
                                                        (the ,(store-type typ) ,(car sto)) 
                                                        ,of))
                                                 `(apply #'ref (list* ,(car ten) ,lst)))))
                                   rsyms ssyms tsyms osyms types))
                      ,@body)))))))
