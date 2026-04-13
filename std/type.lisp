;;; std/types.lisp --- Standard Types

;;

;;; Code:
(in-package :std/type)
(declaim (optimize (speed 3)))

;; Bytes aren't necessarily 8 bits wide in Lisp. OCTET is always 8
;; bits.
(deftype octet () 
  "An 8-bit unsigned-byte."
  '(unsigned-byte 8))

(deftype abstract-ds-lambda-list () 
  "The SBCL type used internally for the abstract representation of a
destructuring lambda list."
  '(simple-vector 7))

(defun parse-meta-ds-lambda-list (lambda-list)
  (let ((ds-ll (parse-ds-lambda-list lambda-list)))
    (declare (abstract-ds-lambda-list ds-ll))
    (meta-abstractify-ds-lambda-list ds-ll)))

;; these are already defined by SB-SIMD
#+nil
(macrolet ((def (name)
               `(deftype ,name () ,@(let* ((s (string name))
                                           (c (ecase (schar s 0)
                                                (#\U ''unsigned-byte)
                                                (#\S ''signed-byte)))
                                           (n (parse-integer (subseq s 1)))
                                           (d (format nil "~A-bit ~A." n (eval c))))
                                      `(,d `(,,c ,,n)))))
           (defs (&rest names)
             `(progn
                ,@(loop for n in names collect `(def ,n)))))
  (defs u1 u2 u3 u4 u5 u6 u7 u8 u16 u24 u32 u64)
  (defs s1 s2 s3 s4 s5 s6 s7 s8 s16 s24 s32 s64))
                                         

(deftype simple-octet-vector ()
  `(simple-array (unsigned-byte 8) (*)))

(deftype octet-vector (&optional length)
  "A simple-array of OCTETs."
  (if length `(simple-array octet (,length))
      `(simple-vector octet)))

(defun octet-vector-p (self &optional length)
  "Return T if SELF is an OCTET-VECTOR, optionally with a fixed LENGTH."
  (typep self (if length `(octet-vector ,length) 'octet-vector)))

(defconstant +default-element-type+ 'character
  "The default ELEMENT-TYPE used by some array operations.")

(deftype array-index (&optional (length (1- array-dimension-limit)))
  "Type designator for an index into array of LENGTH: an integer between
0 (inclusive) and LENGTH (exclusive). LENGTH defaults to one less than
ARRAY-DIMENSION-LIMIT."
  `(integer 0 (,length)))

(deftype array-length (&optional (length (1- array-dimension-limit)))
  "Type designator for a dimension of an array of LENGTH: an integer between
0 (inclusive) and LENGTH (inclusive). LENGTH defaults to one less than
ARRAY-DIMENSION-LIMIT."
  `(integer 0 ,length))

;; This MACROLET will generate most of CDR5 (http://cdr.eurolisp.org/document/5/)
;; except the RATIO related definitions and ARRAY-INDEX.
(macrolet
    ((frob (type &optional (base-type type))
       (let ((subtype-names (list))
             (predicate-names (list)))
         (flet ((make-subtype-name (format-control)
                  (let ((result (format-symbol :std format-control
                                               (symbol-name type))))
                    (push result subtype-names)
                    result))
                (make-predicate-name (sybtype-name)
                  (let ((result (format-symbol :std '#:~A-p
                                               (symbol-name sybtype-name))))
                    (push result predicate-names)
                    result))
                (make-docstring (range-beg range-end range-type)
                  (let ((inf (ecase range-type (:negative "-inf") (:positive "+inf"))))
                    (format nil "Type specifier denoting the ~(~A~) range from ~A to ~A."
                            type
                            (if (equal range-beg ''*) inf (ensure-car range-beg))
                            (if (equal range-end ''*) inf (ensure-car range-end)))))
                (make-docstring* (type)
                  (format nil "Return Non-nil if N is of type ~A." type)))
           (let* ((negative-name     (make-subtype-name '#:negative-~a))
                  (non-positive-name (make-subtype-name '#:non-positive-~a))
                  (non-negative-name (make-subtype-name '#:non-negative-~a))
                  (positive-name     (make-subtype-name '#:positive-~a))
                  (negative-p-name     (make-predicate-name negative-name))
                  (non-positive-p-name (make-predicate-name non-positive-name))
                  (non-negative-p-name (make-predicate-name non-negative-name))
                  (positive-p-name     (make-predicate-name positive-name))
                  (negative-extremum)
                  (positive-extremum)
                  (below-zero)
                  (above-zero)
                  (zero))
             (setf (values negative-extremum below-zero
                           above-zero positive-extremum zero)
                   (ecase type
                     (fixnum       (values 'most-negative-fixnum -1 1 'most-positive-fixnum 0))
                     (integer      (values ''* -1       1        ''* 0))
                     (rational     (values ''* '(0)     '(0)     ''* 0))
                     (real         (values ''* '(0)     '(0)     ''* 0))
                     (float        (values ''* '(0.0E0) '(0.0E0) ''* 0.0E0))
                     (short-float  (values ''* '(0.0S0) '(0.0S0) ''* 0.0S0))
                     (single-float (values ''* '(0.0F0) '(0.0F0) ''* 0.0F0))
                     (double-float (values ''* '(0.0D0) '(0.0D0) ''* 0.0D0))
                     (long-float   (values ''* '(0.0L0) '(0.0L0) ''* 0.0L0))))
             `(progn
                (deftype ,negative-name ()
                  ,(make-docstring negative-extremum below-zero :negative)
                  `(,',base-type ,,negative-extremum ,',below-zero))

                (deftype ,non-positive-name ()
                  ,(make-docstring negative-extremum zero :negative)
                  `(,',base-type ,,negative-extremum ,',zero))

                (deftype ,non-negative-name ()
                  ,(make-docstring zero positive-extremum :positive)
                  `(,',base-type ,',zero ,,positive-extremum))

                (deftype ,positive-name ()
                  ,(make-docstring above-zero positive-extremum :positive)
                  `(,',base-type ,',above-zero ,,positive-extremum))

                (declaim (inline ,@predicate-names))

                (defun ,negative-p-name (n)
                  ,(make-docstring* negative-name)
                  (and (typep n ',type)
                       (< n ,zero)))

                (defun ,non-positive-p-name (n)
                  ,(make-docstring* positive-name)
                  (and (typep n ',type)
                       (<= n ,zero)))

                (defun ,non-negative-p-name (n)
                  ,(make-docstring* non-negative-name)
                  (and (typep n ',type)
                       (<= ,zero n)))

                (defun ,positive-p-name (n)
                  ,(make-docstring* positive-name)
                  (and (typep n ',type)
                       (< ,zero n)))))))))
  (frob fixnum integer)
  (frob integer)
  (frob rational)
  (frob real)
  (frob float)
  (frob short-float)
  (frob single-float)
  (frob double-float)
  (frob long-float))

(defun of-type (type)
  "Returns a function of one argument, which returns true when its argument is
of TYPE."
  (lambda (thing) (typep thing type)))

(define-compiler-macro of-type (&whole form type &environment env)
  ;; This can yeild a big benefit, but no point inlining the function
  ;; all over the place if TYPE is not constant.
  (if (constantp type env)
      (with-gensyms (thing)
        `(lambda (,thing)
           (typep ,thing ,type)))
      form))

(definline type-class-of (obj)
  "Return the TYPE-CLASS of OBJ."
  (type-class (ctype-of obj)))

(definline type-class-name-of (obj)
  "Return the name of the TYPE-CLASS of OBJ."
  (type-class-name (type-class-of obj)))

(definline type-class-id-of (obj)
  "Return the ID of the TYPE-CLASS of OBJ."
  (type-class-id (ctype-of obj)))

(definline type= (type1 type2)
  "Returns a primary value of T if TYPE1 and TYPE2 are the same type,
and a secondary value that is true is the type equality could be reliably
determined: primary value of NIL and secondary value of T indicates that the
types are not equivalent."
  (multiple-value-bind (sub ok) (subtypep type1 type2)
    (cond ((and ok sub) ; type1 is known to be a subtype of type 2
           ; so type= return values come from the second invocation of subtypep
           (subtypep type2 type1))
          ;; type1 is assuredly NOT a subtype of type2,
          ;; so assuredly type1 and type2 cannot be type=
          (ok
           (values nil t))
          ;; our first result is uncertain ( ok == nil ) and it follows
          ;; from specification of SUBTYPEP that sub = ok = NIL
          (t
           (assert (not sub))           ; is the implementation correct?
           (multiple-value-bind (sub2 ok2)
               (subtypep type2 type1)
             (if  (and (not sub2) ok2)  ; we KNOW type2 is not a subtype of type1
                  ;; so our results are certain...
                  (values nil t)
                  ;; otherwise, either type2 is surely a subtype of type1 (t t)
                  ;; or type2 is not a subtype of type1, but we don't
                  ;; know that for sure (nil nil)
                  ;; In either case our result is negative but unsure
                  (values nil nil)))))))

(define-modify-macro coercef (type-spec) coerce
  "Modify-macro for COERCE.")

(defparameter *primitive-object-table*
  (let ((tbl (make-hash-table)))
    (dolist (obj *primitive-objects* tbl)
      (setf (gethash (primitive-object-name obj) tbl) 
            (cons (symbol-value (primitive-object-lowtag obj)) 
                  (symbol-value (primitive-object-widetag obj))))))
  "Primitive objects are defined by SBCL and will not change. Convenient as a
non-unique ID prefix.")

;;; Type IDs
;; would CART-TYPECASE be useful here?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *simple-type-table* (make-hash-table :test 'equal)
    "A hash-table mapping simple type names to integers.")

  (defvar *simple-types* (make-array 128 :adjustable nil)
    "A vector containing the simple set of lisp objects .")

  (declaim (hash-table *core-type-table*)
           (simple-vector *core-types*))
  (defvar *core-type-table*)
  (defvar *core-types*)

(definline next-type-id (&optional (table *core-type-table*))
  (hash-table-count table))

(defun reset-core-types ()
  (setq *core-type-table* *simple-type-table*
        *core-types* *simple-types*))

(defun register-type-id (type &optional id (table *core-type-table*) (vector *core-types*))
  (declare (simple-vector vector) (hash-table table))
  (unless id (setf id (next-type-id table)))
  (setf (gethash type table) id
        (aref vector id) type))

(macrolet ((simple-id (type id)
             `(register-type-id ,type ,id *simple-type-table* *simple-types*))
           (simple-id-order (&rest types &aux (i 0))
             `(progn
                ,@(mapcar (lambda (x) (prog1 `(simple-id ',x ,i) (incf i))) types))))
  (simple-id-order 
   t
   character base-char
   double-float  single-float 
   (complex double-float) (complex single-float) 
   integer
   bignum
   fixnum
   bit 
   symbol 
   boolean
   null cons 
   standard-object structure-object
   pathname hash-table
   array
   (array character)
   (array base-char)
   (array double-float)
   (array single-float)
   (array (complex double-float))
   (array (complex single-float))
   (array fixnum)
   (array bit)
   vector
   (vector character)
   (vector base-char)
   (vector double-float)
   (vector single-float)
   (vector (complex double-float))
   (vector (complex single-float))
   (vector fixnum)
   (vector bit)
   string
   simple-array simple-vector 
   simple-string base-string
   octet-vector)
  (reset-core-types)))

(defmacro simple-type-id (obj)
  (let ((cases))
    (maphash (lambda (x y) (push (list x y) cases)) *simple-type-table*)
    `(typecase ,obj
       ,@cases)))

(defun get-type-id (obj)
  (declare (optimize (safety 0) (speed 3)))
  (or (gethash (type-of obj) *core-type-table*)
      (let ((id (simple-type-id obj)))
        (when id
          (gethash (aref *core-types* id) *core-type-table*)))))

(definline prim-type (obj)
  "Return the name of the primitive type of OBJ."
  (sb-vm::primitive-type-name (sb-vm::primitive-type-of obj)))

(defun type-id (obj)
  "Return the 'type-id' of OBJ which is a 16-bit integer containing type
information. The first 8 bits are the associated object widetag followed by an
8-bit tag corresponding to an index of the *CORE-OBJECTS* vector, which may be
extended by the user using the REGISTER-TYPE-ID function. "
  (declare (optimize (speed 3) (safety 0)))
  (let ((id 0))
    (declare ((unsigned-byte 16) id) (dynamic-extent id))
    (setf (ldb (byte 8 0) id) (widetag-of obj)) ;; 8 bits
    (setf (ldb (byte 4 8) id) (get-type-id obj))
    id))

(defun type-id<= (obj1 obj2)
  (<= (type-id obj1) (type-id obj2)))

(defun type-id< (obj1 obj2)
  (< (type-id obj1) (type-id obj2)))

(defun type-id= (obj1 obj2)
  (= (type-id obj1) (type-id obj2)))

(defun array-type= (t1 t2)
  (and (subtypep t1 t2) (subtypep t2 t1)))
