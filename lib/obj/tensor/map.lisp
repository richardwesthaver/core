;;; map.lisp --- Tensor Mapping Functions

;; 

;;; Code:
(in-package :obj/tensor)

(eval-always
  (defgeneric mapsor! (func x y)
    (:documentation
     "
    Syntax
    ======
    (MAPSOR! func x y)

    Purpose
    =======
    Applies the function element-wise on x, and sets the corresponding
    elements in y to the value returned by the function.

    Example
    =======
    > (mapsor! #'(lambda (idx x y)
                  (if (= (car idx) (cadr idx))
                      (sin x)
                      y))
       (randn '(2 2)) (zeros '(2 2)))
    #<REAL-TENSOR #(2 2)
    -9.78972E-2  0.0000
     0.0000     -.39243
    >
    >
")
    (:method :before ((func function) (x tensor) (y tensor))
      (assert (with-optimization (:speed 3 :safety 0) 
                (vector-eq (dimensions x) (dimensions y))) 
              nil 'tensor-dimension-mismatch))
    (:generic-function-class tensor-method-generator)))

(define-tensor-method mapsor! ((func function) (x dense-tensor :x) (y dense-tensor :y))
  `(dorefs (idx (dimensions x))
     ((ref-x x :type ,(cl :x))
      (ref-y y :type ,(cl :y)))
     (setf ref-y (funcall func idx ref-x ref-y)))
  'y)

(define-tensor-method mapsor! ((func function) (x null) (y dense-tensor :y))
  `(dorefs (idx (dimensions y))
     ((ref-y y :type ,(cl :y)))
     (setf ref-y (funcall func idx ref-y)))
  'y)

(definline mapsor (func x &optional output-type)
  (let ((ret (zeros (dimensions x) (or output-type (class-of x)))))
    (mapsor! #'(lambda (idx x y) (declare (ignore idx y)) (funcall func x)) x ret)))

(defmacro map-tensor! (type (x tensor) &body body)
  (declare (type symbol x))
  (using-gensyms (decl (tensor) (idx ref))
    `(let (,@decl)
       (declare (type ,type ,tensor))
       (with-optimization (:speed 3 :safety 0)
         (dorefs (,idx (dimensions ,tensor))
             ((,ref ,tensor :type ,type))
             (setf ,ref (lety ((,x ,ref :type ,(field-type type))) ,@body))))
       ,tensor)))

(defun check-dims (axlst tensors)
  (let ((axlst (if (numberp axlst) (make-list (length tensors) :initial-element axlst) axlst)))
    (loop for x in tensors
          for axis in axlst
          with dims = nil
          with strides = nil
          with slices = nil
          do (cond
               ((typep x 'dense-tensor)
                (lety ((xdims (dimensions x) :type index-store-vector))
                  (assert (< axis (order x)) nil 'tensor-dimension-mismatch)
                  (if (null dims)
                      (setf dims (aref xdims (mod axis (order x))))
                      (setf dims (min (aref xdims (mod axis (order x))) dims))))
                (push (aref (strides x) (mod axis (order x))) strides)
                (push (slice~ x axis 0 (if (> (order x) 1) nil t)) slices))
               ((eq x nil)
                (push nil strides)
                (push nil slices))
               (t (error 'invalid-arguments)))
          finally (return (values dims (nreverse strides) slices)))))

(defun mapslice (axis func tensor &rest more-tensors)
  (letv* ((d.axis strides slices (check-dims axis (cons tensor more-tensors))))
    (loop :for i :from 0 :below d.axis
          :collect (prog1 (apply func (mapcar #'tensor-copy slices))
                     (when (< i (1- d.axis))
                       (loop :for slc :in slices
                             :for std :in strides
                             :do (when slc (incf (slot-value slc 'head) std))))))))

(defun mapslice~ (axis func tensor &rest more-tensors)
  (letv* ((d.axis strides slices (check-dims axis (cons tensor more-tensors))))
   (loop :for i :from 0 :below d.axis
      :collect (prog1 (apply func slices)
                 (when (< i (1- d.axis))
                   (loop :for slc :in slices
                      :for std :in strides
                      :do (when slc (incf (slot-value slc 'head) std))))))))

(defun mapslicec~ (axis func tensor &rest more-tensors)
  (letv* ((d.axis strides slices (check-dims axis (cons tensor more-tensors))))
    (loop :for i :from 0 :below d.axis
       :do (prog1 (apply func slices)
             (when (< i (1- d.axis))
               (loop :for slc :in slices
                  :for std :in strides
                  :do (when slc (incf (slot-value slc 'head) std)))))))
  (values-list (cons tensor more-tensors)))
;;

(defmacro tensor-foldl (type func ten init &key (init-type (field-type type)) (key nil))
  (using-gensyms (decl (ten init))
    (with-gensyms (sto idx of funcsym keysym)
    `(let* (,@decl
            ,@(unless (symbolp func)
                `((,funcsym ,func)))
            ,@(unless (symbolp key)
                `((,keysym ,key)))
            (,sto (store ,ten)))
       (declare (type ,type ,ten)
                ,@(unless (symbolp func) `((type function ,funcsym)))
                ,@(unless (symbolp key) `((type function ,keysym)))
                (type ,(store-type type) ,sto)
                ,@(when init-type
                        `((type ,init-type ,init))))
       (with-optimization (:speed 3 :safety 0)
         (loop for ,idx being the idx from 0 below (dimensions ,ten) 
               with-iterator ((:stride ((,of (strides ,ten) (head ,ten)))))
               do (setf ,init (,@(if (symbolp func)
                                     `(,func)
                                     `(funcall ,funcsym))
                               ,init 
                               ,(recursive-append
                                 (when key
                                   (if (symbolp key)
                                       `(,key)
                                       `(funcall ,keysym)))
                                 `(t/store-ref ,type ,sto ,of))))))
       ,init))))

(defmacro with-peeky! (((&rest tensors) &optional (step 1)) &rest body)
  (let ((ts (zipsym tensors)))
    (with-gensyms (e.step s)
      `(let (,@ts
             (,e.step ,step))
         (unless (and
                  ,@(mapcar #'(lambda (x) `(when-let ((,s (gethash 'slice-increment (attributes ,x))))
                                             (incf (slot-value ,x 'head) (* ,e.step ,s))))
                            (mapcar #'car ts)))
           (error 'tensor-error :message "Can't find slice-increment in tensor's attributes"))
         (prog1 (progn ,@body)
           (unless (and
                    ,@(mapcar #'(lambda (x) `(when-let ((,s (gethash 'slice-increment (attributes ,x))))
                                               (decf (slot-value ,x 'head) (* ,e.step ,s))))
                              (mapcar #'car ts)))
             (error 'tensor-error :message "Can't find slice-increment in tensor's attributes")))))))

(definline peek-tensor! (x &optional (step 1))
  (if-let ((s (gethash 'slice-increment (memos x))))
    (progn (incf (slot-value x 'head) (* step s)) x)
    (error 'tensor-error :message "Can't find slice-increment in tensor's attributes" :tensor x)))

(definline peek-tensor~ (x &optional (step 1))
  (if-let ((s (gethash 'slice-increment (memos x))))
    (let ((ret (subtensor~ x nil)))
      (incf (slot-value ret 'head) (* step s)) ret)
    (error 'tensor-error :message "Can't find slice-increment in tensor's attributes" :tensor x)))

;; FOR x BEING THE SLICE OF y ALONG axis (FROM BELOW TO DOWNTO) (WITH-INDEX) (BY)
;; TODO
(defun loop-slice-iteration-path (xa data-type prep-phrases)
  (declare (ignore data-type))
  (destructuring-bind (%x %axis &optional start oend cend dend index step) prep-phrases
    (let ((x (cadr %x))
          (axis (cadr %axis)))
    ;; (when (or (and oend cend) (and dend (or cend oend))) (error "Use only one of BELOW TO DOWNTO."))
    (when (setq xa (ensure-list xa))
      (binding-gensyms (hy hyf)
        (let ((n (length xa)))
          (push 
           `(let ((,(hy axis) ,(hy axis)))
              (setf ,@(mapcan #'(lambda (x) `(,x (caar ,(hy axis)) ,(hy axis) (cdr ,(hy axis)))) xa)))
           (sb-loop::wrappers sb-loop::*loop*))
          (push
           `(let ((,(hy x) (if (listp ,(hy x))
                               (progn (assert (= (length ,(hy x)) ,n) nil 'invalid-arguments) ,(hy x))
                               (make-list ,n :initial-element ,(hy x)))))
             (loop for ,(hy xi) in ,(hy x)
                      for ,(hy as) on ,(hy axis)
                   do (etypecase ,(hy xi)
                        (null (setf (car ,(hy as)) nil))
                        (dense-tensor
                         (let* ((,(hy ai) (modproj (car ,(hy as)) (order ,(hy xi))))
                                (,(hy xi) ,(if (or start oend cend dend)
                                               `(let ((,(hy dimi) (dimensions ,(hy xi) (the index-type ,(hy ai))))
                                                      (,(hy slice) (make-list (order ,(hy xi)) :initial-element '(nil nil))))
                                                  (declare (ignorable ,(hy dimi)))
                                                  (setf (nth ,(hy ai) ,(hy slice))
                                                        ,(cond
                                                           (dend `(list* ,(and start (hy start)) (- (modproj ,(and dend (hy dend)) ,(hy dimi)) ,(hy dimi) 1) -1))
                                                           (oend `(list ,(and start (hy start)) ,(and oend (hy oend))))
                                                           (cend `(list ,(and start (hy start)) (1+ (modproj ,(and cend (hy cend)) ,(hy dimi)))))
                                                           (t `(list ,(and start (hy start)) nil))))
                                                  (subtensor~ ,(hy xi) ,(hy slice)))
                                               (hy xi))))
                           (if ,(hy xi)
                               (progn
                                 (when (or (< ,(hy dim) 0) (> ,(hy dim) (dimensions ,(hy xi) (the index-type ,(hy ai)))))
                                   (setf ,(hy dim) (dimensions ,(hy xi) (the index-type ,(hy ai)))))
                                 (setf (car ,(hy as))
                                       (cons (let ((,(hy xs) (slice~ ,(hy xi) ,(hy ai))))
                                               (setf (gethash 'slice-increment (memos ,(hy xs))) (strides ,(hy xi) ,(hy ai)))
                                               ,(hy xs))
                                             (strides ,(hy xi) ,(hy ai)))))
                               (setf ,(hy dim) 0
                                     (car ,(hy as)) (cons nil nil))))))))
           (sb-loop::wrappers sb-loop::*loop*))
          `(((,(hy x) ,x)
             (,(hy dim) -1)
            ;; ,@(mapcan #'(lambda (x y) (when x `((with ,(hyf y) = ,x) (declare (type index-type ,(hyf y))))))
            ;;          (list start oend cend dend step)
            ;;          '(start oend cend dend step))             
             (,(hy axis) (let ((,(hy axis) ,axis))
                           (if (listp ,(hy axis))
                               (assert (= (length ,(hy axis)) ,n) nil 'invalid-arguments)
                               (setq ,(hy axis) (make-list ,n :initial-element ,(hy axis))))
                           ,(hy axis)))
             ,@(mapcan #'(lambda (x) `((,x nil))) xa))
            ()
            ;; (repeat ,(if step `(floor ,(hy dim) ,(hy step)) (hy dim)))
            ;; ,@(when index `((for ,index initially ,(or (and start (hy start)) (if dend `(1- ,(hy dim)) 0)) then (,(if dend '- '+) ,index ,(or (and step (hy step)) 1)))))
            () ;pre-test
            ()
            ;; (,@(print (first iterable))) ; psteps
            t
            ((loop for ,(hy ai) in ,(hy axis)
                  when ,(hy ai) 
                  do (incf (slot-value (car ,(hy ai)) 'head) 
                       ,(if step `(* ,(hy step) (cdr ,(hy ai))) `(cdr ,(hy ai)))))))))))))

(sb-loop::add-loop-path '(slice) 'loop-slice-iteration-path *loop-ansi-universe*
                        :preposition-groups '((:of :across :in) (:along) (:from) (:below) (:to) (:downto) (:with-index) (:by))
                        :inclusive-permitted nil)

(defmacro with-coordinates ((&rest syms) vector &body code)
  (with-gensyms (vec)
    `(let ((,vec ,vector))
       (declare (type tensor-vector ,vec))
       (assert (= (dimensions ,vec 0) ,(length syms)) nil 'tensor-dimension-mismatch)
       (symbol-macrolet (,@(mapcar (let ((i -1)) #'(lambda (x) `(,x (ref ,vec ,(incf i))))) syms))
         ,@code))))
