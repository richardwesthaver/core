;;; loop.lisp --- Tensor Loop Utils

;; 

;;; Commentary:

;;; Code:
(in-package :obj/tensor)

;;The scheme for this iterator was obtained from FEMLISP.
(defmacro mod-update ((idx init dims &key order uplo) &rest body)
  (let* ((uplo (or uplo :ul))
         (order (or order (case uplo ((:u :uo) :col-major) ((:l :lo) :row-major)) *default-stride-ordering*)))
    (assert (null (remove-if #'(lambda (x) (member (first x) '(:update :reset))) body)) nil 'invalid-arguments)
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
                        ,@(mapcar #'(lambda (reset) `(let (,@(zip (second reset) (list count idx init dims))) ,@(cddr reset))) (remove-if-not #'(lambda (x) (eql (first x) :reset)) body))
                        (setf (aref ,idx ,count) (aref ,init ,count)))
                      (progn
                        ,@(mapcar #'(lambda (update) `(let (,@(zip (second update) (list count idx init dims))) ,@(cddr update))) (remove-if-not #'(lambda (x) (eql (first x) :update)) body))
                        (incf (aref ,idx ,count))
                        (return t))))))))

;;; FOR X BEING THE MOD OF Y
;; (loop for y being the idx in X below (dims) using (iterator order ul) ...)
(defun loop-mod-iteration-path (variable data-type prep-phrases &key (index 0) dimensions uplo stride minor)
  (cond ((or (cdr prep-phrases) (not (member (caar prep-phrases) '(:in :of :across))))
         (sb-loop::loop-error "Invalid prepositions!"))
        ((null prep-phrases)
         (sb-loop::loop-error "missing OF, ACROSS, or IN in ~S iteration path")))
  (let ((idx-var (gensym "IDX"))
        (dim-var (gensym "DIM"))
        (variable (or variable (gensym "VAR")))
        (ini-var (gensym "INIT")))
    (push `(let ((,ini-var ,(print (cadar prep-phrases))))) (sb-loop::wrappers sb-loop::*loop*))
    `(((,variable nil ,data-type) (,idx-var ,index) (,dim-var ,dimensions))
      ()
      ()
      ()
      t
      #+nil
      (not (multiple-value-setq (,(sb-loop::loop-when-it-var)
                                 ,variable)
             (,next-fn)))
      ())))

(sb-loop::add-loop-path '(index idx) 'loop-mod-iteration-path *loop-ansi-universe*
                        :preposition-groups '((:of :in :across))
                        :inclusive-permitted nil)

#+nil
(defgeneric for-mod-iterator (clause-name init dims args))
#+nil
(defmacro-clause (FOR-MOD idx FROM initial BELOW dimensions &optional WITH-ITERATOR updates LOOP-ORDER order UPLO ul)
  (check-type idx symbol)
  (binding-gensyms (gm gf)
    (let ((iterables (mapcar #'(lambda (x) (for-mod-iterator (first x) (gm init) (gm dims) (second x))) updates)))
      `(progn
         (with ,(gm dims) = (coerce ,dimensions 'index-store-vector))
         (with ,(gm init) = (let ((,(gm idx) ,initial))
                              (if (numberp ,(gm idx))
                                  (t/store-allocator index-store-vector (length ,(gm dims)) :initial-element ,(gm idx))
                                  (coerce ,(gm idx) 'index-store-vector))))
         (with ,idx = (copy-seq ,(gm init)))
         (declare (type index-store-vector ,(gm dims) ,idx ,(gm init)))
         (initially (assert (ziprm (= length) (,(gm init) ,(gm dims)))))
         ,@(mapcan #'first iterables)
         (after-each
          (unless
              (with-optimization (:speed 3 :safety 0) (mod-update (,idx ,(gm init) ,(gm dims) :order ,order :uplo ,ul) ,@(mapcan #'cdr iterables)))
            (finish)))))))

#+nil
(closer-mop:defmethod for-mod-iterator ((clause-name (eql :stride)) init dims strides)
  (binding-gensyms (gm gf)
    (list `(,@(mapcan #'(lambda (x)
                          `((with ,(gf (first x)) = ,(second x))
                            (with ,(first x) = (+ ,(or (third x) 0)
                                                  (loop :for ,(gm i) :of-type index-type :from 0 :below (length ,init)
                                                     :summing (the index-type (* (aref ,(gf (first x)) ,(gm i)) (aref ,init ,(gm i)))) :of-type index-type)))))
                      strides)
              (initially (assert (ziprm (= length) (,dims ,@(mapcar #'(lambda (x) (gf (first x))) strides)))))
              (declare (type index-store-vector ,@(mapcar #'(lambda (x) (gf (first x))) strides))
                       (type index-type ,@(mapcar #'car strides))))
          `(:update (,(gm count) ,(gm idx) ,(gm init) ,(gm dims))
                    (declare (ignore ,(gm idx) ,(gm init) ,(gm dims)))
                    ,@(mapcar #'(lambda (x) `(incf ,(first x) (aref ,(gf (first x)) ,(gm count)))) strides))
          `(:reset (,(gm count) ,(gm idx) ,(gm init) ,(gm dims))
                   (declare (ignore ,(gm dims)))
                   ,@(mapcar #'(lambda (x) `(decf ,(first x) (the index-type (* (aref ,(gf (first x)) ,(gm count)) (- (aref ,(gm idx) ,(gm count)) (aref ,(gm init) ,(gm count))))))) strides)))))

#+nil
(closer-mop:defmethod for-mod-iterator ((clause-name (eql :general)) init dims body)
  body)
#+nil
(defmacro offset-ref (decl &rest body)
  (let ((stack (mapcar #'(lambda (x) (list (gensym "sto") (gensym))) decl)))
    `(let-typed (,@(mapcar #'(lambda (x s)
                               (letv* (((ref offset tensor &key type) x))
                                 `(,(second s) ,tensor ,@(when type `(:type ,type)))))
                           decl stack))
       (let-typed (,@(mapcar #'(lambda (x s) (letv* (((ref offset tensor &key type) x))
                                               `(,(first s) (store ,(second s)) ,@(when type `(:type ,(store-type type))))))
                             decl stack))
         (symbol-macrolet (,@(mapcar #'(lambda (x s)
                                         (letv* (((ref offset tensor &key type) x))
                                           `(,ref ,(if type
                                                       `(the ,(field-type type) (t/store-ref ,type ,(first s) ,offset))
                                                       `(store-ref ,(second s) ,(first s))))))
                                     decl stack))
           ,@body)))))

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
(defmacro dorefs ((idx dims &key (loop-order *default-stride-ordering* loop-ordering-p) (uplo? :ul)) (&rest ref-decls) &rest body)
  (let* ((tsyms (zipsym (mapcar #'second ref-decls)))
         (rsyms (mapcar #'car ref-decls))
         (types (mapcar #'(lambda (x) (destructuring-bind (ref ten &key type) x
                                        (declare (ignore ref ten))
                                        type))
                        ref-decls))
         (ssyms (mapcar #'(lambda (x y) (when y `(,(gensym) (slot-value ,(car x) 'store)))) tsyms types))
         (osyms (mapcar #'(lambda (y) (when y (gensym))) types)))
    (using-gensyms (decl (dims) (lst))
      `(lety (,@decl
                 ,@(mapcar #'(lambda (x y) (if y (append x `(:type ,y)) x)) tsyms types))
         (declare (type index-store-vector ,dims))
         (lety ((,lst (make-list (length ,dims) :initial-element 0))
                ,@(remove-if #'null (mapcar #'(lambda (x y) (when y (append x `(:type ,(store-type y))))) ssyms types)))
           (iter (for-mod ,idx from ,(case uplo?
                                       (:uo `(append (make-list (1- (length ,dims)) :initial-element 0) (list 1)))
                                       (:lo `(append (list 1) (make-list (1- (length ,dims)) :initial-element 0)))
                                       (t 0))
                          below ,dims 
                          with-iterator ((:stride (,@(remove-if #'null 
                                                                (mapcar 
                                                                 #'(lambda (of ten typ) 
                                                                     (when typ `(,of (strides ,(car ten)) 
                                                                                     (head ,(car ten)))))
                                                                 osyms tsyms types)))))
                          ,@(when loop-ordering-p `(loop-order ,loop-order)) uplo ,uplo?)
                 (lvec->list! ,idx ,lst)
                 (symbol-macrolet (,@(mapcar #'(lambda (ref sto ten of typ) (list ref (if typ
                                                                                          `(the ,(field-type typ) (t/store-ref ,typ (the ,(store-type typ) ,(car sto)) ,of))
                                                                                          `(apply #'ref (list* ,(car ten) ,lst)))))
                                             rsyms ssyms tsyms osyms types))
                   ,@body)))))))
