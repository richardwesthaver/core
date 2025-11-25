;;; alien.lisp --- foreign alien friends

;; 

;;; Commentary:

;; FFI in Lisp is somewhat different than FFI in other host langs. As
;; such, we usually refer to our Lispy FFI interfaces inline with the
;; CMUCL terminology: alien interfaces.

;; ref: https://www.sbcl.org/manual/#Foreign-Function-Interface for details

;; sb-alien is a high-level interface which automatically converts C
;; memory pointers to lisp objects and back, but this can be slow for
;; large or complex objects.

;; The lower-level interface is based on System Area Pointers (or
;; SAPs), which provide untyped access to foreign memory.

;; Objects which can't be automatically converted into Lisp values are
;; represented by objects of type ALIEN-VALUE.

;;; Code:
(in-package :std/alien)
;; (shadowing-import
;;  '(sb-unix::syscall sb-unix::syscall* sb-unix::int-syscall
;;    sb-unix::with-restarted-syscall sb-unix::void-syscall) :std)

;; (reexport-from :sb-vm
;;  	       :include
;;  	       '(:with-pinned-objects :with-pinned-object-iterator :with-code-pages-pinned
;;  		 :sanctify-for-execution))

;;;_* Types
(deftype alien-or-lisp-octets () '(or array 
                                   (alien (* char)) 
                                   (alien (array char))
                                   (alien (* unsigned-char))
                                   (alien (array unsigned-char))))

(deftype alien-array (element-type &rest dimensions) `(alien (sb-alien:array ,element-type ,@dimensions)))

(defun element-type-to-alien (ty)
  "Convert the given SB-ALIEN element-type spec to one understood by Lisp."
  (cond
    ((symbolp ty)
     (ecase ty
       (character 'char)
       (octet 'unsigned-char)
       (string 'c-string)
       (single-float 'float)
       (double-float 'double)))
    ((listp ty)
     (destructuring-bind (%ty elt) ty
         (ecase %ty
           (signed-byte
            (ecase elt
              (8 'char)
              (16 'short)
              (32 'int)
              (64 'long)))
           (unsigned-byte
            (ecase elt
              (8 'unsigned-char)
              (16 'unsigned-short)
              (32 'unsigned-int)
              (64 'unsigned-long)))
           (complex
            (let ((aty (element-type-to-alien elt)))
              ;; ? todo
              (values `(complex ,aty) `(* (,aty 2)))))
           (simple-array 
            (let ((aty (element-type-to-alien (car elt)))) ;; CDR should be '((*))
              (values `(:* ,aty)
                      (if (and (listp aty) (eql (car aty) 'complex)) 
                          aty
                          `(* ,aty))))))))))

;; (alien-type-class (sb-alien::make-alien-c-string-type :external-format :utf-8 :element-type 'character))

(defun alien-to-element-type (ty)
  (cond
    ((symbolp ty)
     (ecase ty
       (char 'character)
       (unsigned-char 'octet)
       (c-string 'string)
       (short '(signed-byte 16))
       (unsigned-short '(unsigned-byte 16))
       (int '(signed-byte 32))
       (unsigned-int '(unsigned-int 32))
       (long '(signed-byte 64))
       (unsigned-long '(unsigned-int 64))
       (float 'single-float)
       (double 'double-float)
       (* (values 'system-area-pointer '(* t)))
       (callback (values 'symbol '(* t)))))
    ;; greatly simplifying logic from MATLISP here - not handling :* or :&
    ((listp ty)
     (destructuring-bind (%ty elt) ty
       (ecase %ty
         (complex (values `(simple-array ,(element-type-to-alien elt) (*)) `(* (,elt 2))))
         ((or char unsigned-char short unsigned-short int unsigned-int long unsigned-long float double)
          (values `(simple-array ,(element-type-to-alien %ty) (*)) `(* . ,(if (listp elt) `((,%ty ,elt)) `(,%ty))))))))))

;;; Look up alien type information and build both define-sap-accessors form
;;; and convert-alien-type function definition.
(defmacro define-type-mapping (accessor-table alien-table)
  (let* ((accessible-types
           (remove 'void alien-table :key #'second))
         (size-and-signedp-forms
           (mapcar (lambda (name)
                     (list (eval `(alien-size ,(second name)))
                           (typep -1 `(alien ,(second name)))))
                   accessible-types)))
    `(progn
       (define-sap-accessors
         ,@(loop for (kw alien-type fixed-accessor)
                   in accessible-types
                 and (alien-size signedp)
                   in size-and-signedp-forms
                 for (signed-ref unsigned-ref)
                   = (cdr (assoc alien-size accessor-table))
                 collect
                 `(,kw
                   ,(or fixed-accessor
                        (if signedp signed-ref unsigned-ref)
                        (error "No accessor found for ~S"
                               alien-type)))))
       (defun convert-alien-type (type-keyword)
         (ecase type-keyword
           ,@(loop for (kw alien-type) in alien-table
                   collect `(,kw (quote ,alien-type))))))))

(define-type-mapping
    ((8  sb-sys:signed-sap-ref-8  sb-sys:sap-ref-8)
     (16 sb-sys:signed-sap-ref-16 sb-sys:sap-ref-16)
     (32 sb-sys:signed-sap-ref-32 sb-sys:sap-ref-32)
     (64 sb-sys:signed-sap-ref-64 sb-sys:sap-ref-64))
    ((char               char)
     (unsigned-char      unsigned-char)
     (short              short)
     (unsigned-short     unsigned-short)
     (int                int)
     (unsigned-int       unsigned-int)
     (long               long)
     #+nil
     (word               word
                         sb-sys:sap-ref-word)
     (unsigned-long      unsigned-long)
     (long-long          long-long)
     (unsigned-long-long unsigned-long-long)
     (float              single-float
                          sb-sys:sap-ref-single)
     (double             double-float
                          sb-sys:sap-ref-double)
     (pointer            system-area-pointer
                          sb-sys:sap-ref-sap)
     (void               void)))

;; TODO: translate-into-alien-memory translate-to-alien
;; expand-into-alien-memory expand-to-alien bare-alien-struct-p expand-from-alien

;;;_* Utils
(defun shared-object-name (name &optional path)
  "Return a filename with the correct extension for a shared library."
  (unless (string= (subseq name 0 3) "lib") 
    (setf name (format nil "lib~a" name)))
  (unless (search #+darwin ".dylib" #-darwin ".so" name)
    (setf name (format nil #+darwin "~a.dylib" #-darwin "~a.so" name)))
  (if path
      (merge-pathnames name path)
      (pathname name)))

(defun lisp-name-from-c (name &optional (package *package*))
  "Convert a C symbol NAME as a string into a lisp symbol, interning it in PACKAGE."
  (let ((n name))
  (etypecase n
    (list
     (lisp-name-from-c (car n)))
    (string
     ;; set prefix to %
     (when (eql #\_ (char n 0)) (setf (char n 0) #\%))
     (intern (substitute #\- #\_ (string-upcase n)) package)))))

(defun c-name-from-lisp (name)
  "Convert a lisp symbol or string NAME instead a C symbol name as a string."
  (etypecase name
    (list
     (c-name-from-lisp (cadr name)))
    ((or symbol string)
     (let ((n (string name)))
       ;; reset prefix to _
       (when (eql #\% (char n 0)) (setf (char n 0) #\_))
       (substitute 
        #\_ #\- 
        (if (every #'upper-case-p n) ; only apply case conversion on all-caps, else use the string as is
            (string-downcase n)
            n))))))

(defun list-all-shared-objects ()
  "Return the current value of *SHARED-OBJECTS*."
  *shared-objects*)

(defvar *alien-load-table* (make-hash-table))

(std/prim:definline load-alien (name) (funcall (gethash name *alien-load-table*)))

(defmacro define-alien-loader (name &optional (root "/usr/local/lib/") path (package *package*))
  "Define a default loader function named load-NAME which calls
SB-ALIEN:LOAD-SHARED-OBJECT."
  (let ((fname (intern (format nil "~@:(LOAD-~a~)" name) package))
        (%name (keywordicate (string-upcase name))))
    (when (symbolp name)
      (setf name (symbol-name name)))
    `(progn
       (defun ,fname (&optional save)
         (prog1 (sb-alien:load-shared-object (shared-object-name ,(or path (string-downcase name)) ,root) :dont-save (not save))
           (pushnew ,%name *features*)))
       (setf (gethash ,%name *alien-load-table*) (function ,fname)))))

(defmacro alien-size* (ty &optional (units :bits))
  `(alien-size ,(eval ty) ,units))

(defmacro define-opaque (ty &optional foreign-type)
  "Define an 'opaque' alien type. This is an internal convenience function for
types which are effectively aliases for other types. The default target type
is TY with a -T prepended as is customary in many C codebases."
  (eval-when (:compile-toplevel :load-toplevel :execute)
    `(define-alien-type ,ty (struct ,(or foreign-type (symbolicate ty '-t))))))

(defun double-array-pointer (array)
  "Return a SAP pointing to the start of ARRAY's storage vector."
  (sap-alien (vector-sap (array-storage-vector array)) (* double)))

(defun float-array-pointer (array)
  "Return a SAP pointing to the start of ARRAY's storage vector."
  (sap-alien (vector-sap (array-storage-vector array)) (* float)))

(defun octet-vector-pointer (array)
  "Return a SAP pointing to the start of ARRAY's storage vector."
  (sap-alien (vector-sap (array-storage-vector array)) (* unsigned-char)))

(defun setfa (place from)
  "Fill in a foreign array PLACE using lisp vector FROM."
  (declare (vector from))
  (loop for i below (length from)
        for x across from
	do (setf (deref place i)
                 x)))

(defun copy-c-string (src dest &aux (index 0))
  "Copy a C-allocated string SRC into lisp string DEST."
  (declare (type array-index index))
  (loop (let ((b (sb-sys:sap-ref-8 src index)))
          (when (= b 0) 
            (when (array-has-fill-pointer-p dest)
              (setf (fill-pointer dest) index))
            (return))
          (setf (char dest index) (code-char b))
          (incf index))))

(defun clone-strings (list &optional null-terminate)
  "Copy the list of strings in LIST to a foreign array of C strings. When
NULL-TERMINATE is T we append a null character to each string."
  (let ((len (length list)))
    (with-alien ((x (* (* char)) (make-alien (* char) len)))
      (labels ((populate (list index)
                 (declare (type array-index index))
                 (if list
                     (let ((array (sb-ext:string-to-octets (car list) :null-terminate null-terminate)))
                       (with-pinned-objects (array)
                         (setf (deref x index) (sap-alien (vector-sap array) (* char)))
                         (populate (cdr list) (1+ index))))
                     x)))
        (cast (populate list 0) (* c-string))))))

(defun c-strings-to-string-list (c-strings)
  "Copy the foreign array C-STRINGS to a lisp list of strings."
  (declare (type (alien (* c-string)) c-strings))
  (let ((reversed-result nil))
    (dotimes (i most-positive-fixnum)
      (declare (type array-index i))
      (let ((c-string (deref c-strings i)))
        (if c-string
            (push c-string reversed-result)
            (return (nreverse reversed-result)))))))

(defun clone-octets-to-alien (lispa alien)
  "Copy the octet-vector LISPA to foreign array ALIEN."
  (declare (optimize (speed 3))
           (octet-vector lispa))
  ;; (setf alien (cast alien (array (unsigned 8))))
  (loop for i from 0 below (length lispa)
        do (setf (deref alien i)
                 (aref lispa i)))
  alien)

(defun clone-octet-vector-list (lst)
  "Clone a list of OCTET-VECTORs into an alien (* (* (UNSIGNED 8))). Keep in mind
that the size of the individual OCTET-VECTORs are not encoded."
  (let ((n (length lst)))
    (let ((va (make-alien (* (unsigned 8)) n))) ;; (* (* u8))
      (loop for i below n do (setf (deref va n) (octets-to-alien (pop lst))))
      va)))

(defun clone-integer-list (lst)
  "Clone a list of integers to (* SIZE-T)."
  (let ((n (length lst)))
    (let ((va (make-alien (* size-t) n)))
      (loop for i below n do (setf (deref va n) (pop lst)))
      va)))

(defun clone-octet-vector-list* (lst)
  "Like CLONE-OCTET-VECTOR-LIST but also returns a second value containing an
alien (* size-t) with same size as the first value."
  (values (clone-octet-vector-list lst) (clone-integer-list (mapcar 'length lst))))

(defun octets-to-alien (lispa)
  "Copy octet-vector LISPA to a foreign byte pointer."
  (let ((a (make-alien (unsigned 8) (length lispa))))
    (clone-octets-to-alien lispa a)))

(defun octets-to-alien-array (lispa)
  "Copy octet-vector LISPA to a foreign byte array."
  (values (cast (octets-to-alien lispa) (array (unsigned 8))) (length lispa)))

(defun clone-octets-from-alien (aliena lispa &optional len)
  "Copy the foreign byte pointer ALIENA to lisp octet-vector LISPA. When LEN is non-nil only copy that number of bytes starting from the beginning."
  (declare (optimize (speed 3))
           (octet-vector lispa))
  (unless len (setf len (length lispa)))
  (loop for i from 0 below len
        do (setf (aref lispa i)
                 (deref aliena i)))
  lispa)

(defun foreign-int-to-integer (buffer size)
  "Check SIZE of int BUFFER. return BUFFER."
  (assert (= size (alien-size int :bytes)))
  buffer)

(defun foreign-int-to-bool (x size)
  "Convert a foreign integer X of length SIZE to a lisp boolean."
  (if (zerop (foreign-int-to-integer x size))
      nil
      t))

(defun bool-to-foreign-int (val)
  "Convert a lisp boolean to an integer."
  (if val 1 0))

;;; Conditions
(define-condition invalid-enum-variant (simple-error) ()
  (:documentation "Error signaled when an invalid enum variant is used."))
(define-condition invalid-enum-value (simple-error) ()
  (:documentation "Error signaled when an invalid enum value is used."))

(defun invalid-enum-variant (var enum)
  "Signal an INVALID-ENUM-VARIANT error."
  (error 'invalid-enum-variant
         :format-control "~A is not a variant of enum ~A"
         :format-arguments (list var enum)))

(defun invalid-enum-value (var enum)
  "Signal an INVALID-ENUM-VALUE error."
  (error 'invalid-enum-value
         :format-control "~A is not a value associated with a variant of enum ~A"
         :format-arguments (list var enum)))

;;; SAP-REF
(defmacro define-sap-accessors (&body pairs)
  `(progn
     (defun %sap-ref (ptr type &optional (offset 0))
       (ecase type
         ,@(loop for (keyword fn) in pairs
                 collect `(,keyword (,fn ptr offset)))))
     (defun %sap-set (value ptr type &optional (offset 0))
       (ecase type
         ,@(loop for (keyword fn) in pairs
                 collect `(,keyword (setf (,fn ptr offset) value)))))
     (define-compiler-macro %sap-ref
         (&whole form ptr type &optional (offset 0))
       (if (constantp type)
           (ecase (eval type)
             ,@(loop for (keyword fn) in pairs
                     collect `(,keyword `(,',fn ,ptr ,offset))))
           form))
     (define-compiler-macro %sap-set
         (&whole form value ptr type &optional (offset 0))
       (if (constantp type)
           (once-only (value)
             (ecase (eval type)
               ,@(loop for (keyword fn) in pairs
                       collect `(,keyword `(setf (,',fn ,ptr ,offset)
                                                 ,value)))))
           form))))

;; TODO
(defun aggregatep (type)
  "Return T if the given ALIEN-TYPE is 'aggregate'."
  ;; always arrays and structs, never 'built-in'
  (or (sb-alien::alien-array-type-p type)
      (sb-alien::alien-record-type-p type)))

(defun sap-ref (sap type &optional (offset 0))
  "Return the value of TYPE at OFFSET bytes from SAP. If TYPE is aggregate we
return a pointer instead of its value."
  (naturalize (sap+ sap offset) (parse-alien-type type nil)))

(define-compiler-macro sap-ref (&whole form ptr type &optional (offset 0))
  "Open-code SAP-REF when TYPE is constant."
  (if (constantp type)
      (let* ((parsed-type (parse-alien-type (eval type) nil))
             (ctype (compute-alien-rep-type parsed-type)))
        (if (aggregatep parsed-type)
            (if (bare-struct-type-p parsed-type)
                `(sap+ ,ptr ,offset)
                (expand-from-foreign `(sap+ ,ptr ,offset) parsed-type))
            (expand-from-foreign `(%sap-ref ,ptr ,ctype ,offset) parsed-type)))
      form))

(defun sap-set (value sap type &optional (offset 0))
  "Set the value of TYPE at OFFSET bytes from SAP to VALUE."
  (let* ((ptype (parse-alien-type type nil))
         (ctype (compute-alien-rep-type ptype)))
    (if (aggregatep ptype) ; XXX: backwards incompatible?
        (translate-into-foreign-memory value ptype (sap+ sap offset))
        (%sap-set (translate-to-foreign value ptype) sap ctype offset))))

(define-setf-expander sap-ref (sap type &optional (offset 0) &environment env)
  "SETF expander for SAP-REF that doesn't rebind TYPE.
This is necessary for the compiler macro on SAP-SET to be able
to open-code (SETF SAP-REF) forms."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion sap env)
    (declare (ignore setter newval))
    ;; if either TYPE or OFFSET are constant, we avoid rebinding them
    ;; so that the compiler macros on SAP-SET and %SAP-SET work.
    (with-gensyms (store type-tmp offset-tmp)
      (values
       (append (unless (constantp type)   (list type-tmp))
               (unless (constantp offset) (list offset-tmp))
               dummies)
       (append (unless (constantp type)   (list type))
               (unless (constantp offset) (list offset))
               vals)
       (list store)
       `(progn
          (sap-set ,store ,getter
                   ,@(if (constantp type)   (list type)   (list type-tmp))
                   ,@(if (constantp offset) (list offset) (list offset-tmp)))
          ,store)
       `(sap-ref ,getter
                 ,@(if (constantp type)   (list type)   (list type-tmp))
                 ,@(if (constantp offset) (list offset) (list offset-tmp)))))))

(define-compiler-macro sap-set
    (&whole form value sap type &optional (offset 0))
  "Compiler macro to open-code (SETF SAP-REF) when type is constant."
  (if (constantp type)
      (let* ((parsed-type (parse-alien-type (eval type) nil))
             (ctype (compute-alien-rep-type parsed-type)))
        (if (aggregatep parsed-type)
            (expand-into-foreign-memory
             value parsed-type `(sap+ ,sap ,offset))
            `(%sap-set ,(expand-to-foreign value parsed-type)
                       ,sap ,ctype ,offset)))
      form))

;;;; SAP-SVREF
(defun sap-svref (sap type &optional (index 0))
  "Like SAP-REF except for accessing simple 1d arrays."
  (sap-ref sap type (* index (foreign-type-size type))))

(define-compiler-macro sap-svref (&whole form sap type &optional (index 0))
  "Open-code SAP-SVREF when TYPE (and eventually INDEX)."
  (if (constantp type)
      (if (constantp index)
          `(sap-ref ,sap ,type
                    ,(* (eval index) (foreign-type-size (eval type))))
          `(sap-ref ,sap ,type (* ,index ,(foreign-type-size (eval type)))))
      form))

(define-setf-expander sap-svref (sap type &optional (index 0) &environment env)
  "SETF expander for SAP-SVREF."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion sap env)
    (declare (ignore setter newval))
    ;; we avoid rebinding type and index, if possible (and if type is not
    ;; constant, we don't bother about the index), so that the compiler macros
    ;; on SAP-SET or %SAP-SET can work.
    (with-gensyms (store type-tmp index-tmp)
      (values
       (append (unless (constantp type)
                 (list type-tmp))
               (unless (and (constantp type) (constantp index))
                 (list index-tmp))
               dummies)
       (append (unless (constantp type)
                 (list type))
               (unless (and (constantp type) (constantp index))
                 (list index))
               vals)
       (list store)
       ;; Here we'll try to calculate the offset from the type and index,
       ;; or if not possible at least get the type size early.
       `(progn
          ,(if (constantp type)
               (if (constantp index)
                   `(sap-set ,store ,getter ,type
                             ,(* (eval index) (foreign-type-size (eval type))))
                   `(sap-set ,store ,getter ,type
                             (* ,index-tmp ,(foreign-type-size (eval type)))))
               `(sap-set ,store ,getter ,type-tmp
                         (* ,index-tmp (foreign-type-size ,type-tmp))))
          ,store)
       `(sap-svref ,getter
                  ,@(if (constantp type)
                        (list type)
                        (list type-tmp))
                  ,@(if (and (constantp type) (constantp index))
                        (list index)
                        (list index-tmp)))))))

;; (defun sap-svref (sap type &optional (index 0))
;;  "Like SAP-REF expect for accessing simple (1d) arrays.")
;; (define-setf-expander sap-svref (sap type &optional (index 0))

;;; DEFAR
(defmacro defar (name result-type &rest args)
  "Like DEFINE-ALIEN-ROUTINE but always inline the resulting alien function."
  (multiple-value-bind (lisp-name alien-name) (pick-lisp-and-alien-names name)
    `(progn
       (declaim (inline ,lisp-name))
       (define-alien-routine ,(list alien-name lisp-name) ,result-type ,@args))))

;;; DEFINE-ALIEN-ENUM
;; TODO: use SB-ALIEN:ENUM
(defmacro define-alien-enum ((name type &key (test 'eql) (default :error)) &body forms)
  "Define a pseudo-enum type, used to work-around difficulties working with
SB-ALIEN, groveller, typedef enums, etc.

NAME specified the name of the alien-type and keyword-based lookup
function. Additionally a NAME* reverse-lookup function is provided.

Two hash-tables are defined in the environment of the accessor functions
containing the variants. These are technically exposed anaphors
%lisp-enum-table and %lisp-enum-table*."
  (setf forms (loop for (k . v) on forms by #'cddr
                    collect (cons k v)))
  (with-gensyms (val)
    (let ((%lisp-enum-table (make-hash-table :test test :size (length forms)))
          (%lisp-enum-table* (make-hash-table :size (length forms))))
      (mapc (lambda (x) (setf (gethash (car x) %lisp-enum-table) (eval (cadr x)))) forms)
      (mapc (lambda (x) (setf (gethash (eval (cadr x)) %lisp-enum-table*) (car x))) forms)
      `(progn
         (define-alien-type ,name ,type)
         (defun ,name (,val)
           ,(format nil "Given a keyword naming a variant of ~A, return the associated value." name)
           (let ((found (gethash ,val ,%lisp-enum-table ,default)))
             ,@(when (eql default :error)
                 `((when (eql found :error) (invalid-enum-variant ,val ',name))))
             found))
         (defun ,(symbolicate name '*) (,val)
           ,(format nil "Given a ~A, check that it is equal to one of the variants of ~A and return
it. This function returns a second value which indicates the name of the
variant associated with this value." type name)
           (std:when-let ((found (gethash ,val ,%lisp-enum-table*
                                          ,default)))
             ,@(when (eql default :error)
                 `((when (eql found :error) (invalid-enum-value ,val ',name))))
             (values found ,val)))))))

(define-alien-type unsigned-char-pointer (* unsigned-char))
(define-alien-type char-pointer (* char))

;;; C Char pointer readers and writers

;; inspired by ELEPHANT

;; FIXME: this is probably inefficient after recent discoveries in
;; SB-ALIEN-INTERNALS - can be reworked to use direct SAP accessors when
;; SAP-REF is finished.

;; all operations are performed on (* unsigned-char)
(define-io alien
  ((unsigned-char 8) 
   (:read (vec) 
          vec))
  ((signed-byte 64)
   (:read (buf &optional (offset 0))
          "Read a 64-bit signed integer from a foreign char buffer."
          (declare (type (alien (* unsigned-char)) buf)
                   (type fixnum offset))
          (the (signed-byte 64)
               (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                            (* (signed 64))))))
   (:write (buf num &optional (offset 0))
           "Write a 64-bit signed integer to a foreign char buffer."
           (declare (type (alien (* unsigned-char)) buf)
                    (type (signed-byte 64) num)
                    (type fixnum offset))
           (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                              (* (signed 64)))) 
                 num)))
  ((unsigned-byte 64)
   (:read (buf &optional (offset 0))
          "Read a 64-bit unsigned integer from a foreign char buffer."
          (declare (type (alien (* unsigned-char)) buf)
                   (type fixnum offset))
          (the (signed-byte 64)
               (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                            (* (signed 64))))))
   (:write (buf num &optional (offset 0))
           "Write a 64-bit unsigned integer to a foreign char buffer."
           (declare (type (alien (* unsigned-char)) buf)
                    (type (unsigned-byte 64) num)
                    (type fixnum offset))
           (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                              (* (unsigned 64)))) 
                 num)))
  (single-float
   (:read (buf &optional (offset 0))
          (declare (type (alien (* unsigned-char)) buf)
                   (type fixnum offset))
          (the single-float
               (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                            (* single-float)))))
   (:write (buf num &optional (offset 0))
           "Write a single-float to a foreign char buffer."
           (declare (type (alien (* unsigned-char)) buf)
                    (type single-float num)
                    (type fixnum offset))
           (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                              (* single-float))) 
                 num)))
  (double-float
   (:read (buf &optional (offset 0))
          "Read a double-float from a foreign char buffer."
          (declare (type (alien (* unsigned-char)) buf)
                   (type fixnum offset))
          (the double-float
               (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                            (* double-float)))))
   (:write (buf num &optional (offset 0))
           "Write a double-float to a foreign char buffer."
           (declare (type (alien (* unsigned-char)) buf)
                    (type double-float num)
                    (type fixnum offset))
           (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                              (* double-float))) 
                 num)))
  #+x86-64
  (fixnum 
   (:read (buf)
          (declare (type (alien (* unsigned-char)) buf))
          (read-alien-signed-byte-64 buf))
   (:write (buf num &optional (offset 0))
           "Write a 32-bit signed integer to a foreign char buffer."
           (declare (type (alien (* unsigned-char)) buf)
                    (type fixnum num)
                    (type fixnum offset))
           (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                              (* (signed 64)))) 
                 num)))
  #+x86
  (fixnum 
   (:read (buf &optional (offset 0))
          "Read a 32-bit signed integer from a foreign char buffer."
          (declare (type (alien (* unsigned-char)) buf)
                   (type fixnum offset))
          (the fixnum
               (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                            (* (signed 32))))))
   (:write (buf num &optional (offset 0))
           "Write a 32-bit signed integer to a foreign char buffer."
           (declare (type (alien (* unsigned-char)) buf)
                    (type fixnum num)
                    (type fixnum offset))
           (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                              (* (signed 32)))) 
                 num)))
  ((signed-byte 32)
   (:read (buf &optional (offset 0))
          "Read a 32-bit signed integer from a foreign char buffer."
          (declare (type (alien (* unsigned-char)) buf)
                   (type fixnum offset))
          (the (signed-byte 32)
               (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                            (* (signed 32))))))
   (:write (buf num &optional (offset 0))
           "Write a 32-bit signed integer to a foreign char buffer."
           (declare (type (alien (* unsigned-char)) buf)
                    (type (signed-byte 32) num)
                    (type fixnum offset))
           (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                              (* (signed 32)))) 
                 num)))
  ((unsigned-byte 32)
   (:read (buf &optional (offset 0))
          "Read a 32-bit unsigned integer from a foreign char buffer."
          (declare (type (alien (* unsigned-char)) buf)
                   (type fixnum offset))
          (the (unsigned-byte 32)
               (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                            (* (unsigned 32))))))
   (:write (buf num &optional (offset 0))
	   "Write a 32-bit unsigned integer to a foreign char buffer."
           (declare (type (alien (* unsigned-char)) buf)
                    (type (unsigned-byte 32) num)
                    (type fixnum offset))
           (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
                              (* (unsigned 32))))
                 num)))
  ;; complex types
  (octet-vector
   (:read (buf len)
	  "Read an octet-vector from a foreign unsigned-char buffer."
          (let ((ret (make-octets len)))
            (clone-octets-from-alien buf ret len)))
   (:write (buf vec)
	   "Write an octet-vector to a foreign unsigned-char buffer."
           (declare (type (alien (* unsigned-char)) buf))
           (clone-octets-to-alien vec buf))))

(defun offset-char-pointer (p &optional (offset 0))
  "Return a pointer to the address OFFSET bytes from P."
  (declare (type (alien (* unsigned-char)) p)
           (type fixnum offset))
  (sap-alien (sap+ (alien-sap p) offset) (* unsigned-char)))

(defmacro with-vector-sap ((name vector) &body body)
  "Do BODY with NAME bound to the vector-sap of VECTOR. VECTOR is pinned for the duration."
  `(with-pinned-objects (,vector)
     (let ((,name (vector-sap ,vector)))
       ,@body)))

;; from CFFI
(defmacro with-alien-slots (vars struct &body body)
  "Create local symbol macros for each var in VARS to reference
foreign slots in STRUCT. Similar to WITH-SLOTS.
Each var can be of the form: 
  name                       name bound to slot of same name              
  (* name)            name bound to pointer to slot of same name
  (name slot-name)           name bound to slot-name
  (name :pointer slot-name)  name bound to pointer to slot-name"
  `(symbol-macrolet
       ,(loop for var in vars
              collect
                 (if (listp var)
                     (let ((p1 (first var)) (p2 (second var)) (p3 (third var)))
                       (if (eq (keywordicate p1) :*)
                           `(,p2 (addr (slot ,struct ',p2)))
                           (if (eq (keywordicate p2) :*)
                               `(,p1 (addr (slot ,struct ',p3)))
                               `(,p1 (slot ,struct ',p2)))))
                     `(,var (slot ,struct ',var))))
     ,@body))

(declaim (inline foreign-type-size))
(defun foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (/ (alien-type-bits
      (parse-alien-type
       type nil)) 
     8))

(declaim (inline %foreign-alloc))
(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  ;; (declare (type (unsigned-byte 32) size))
  (alien-sap (make-alien (unsigned 8) size)))

(declaim (inline foreign-free))
(defun foreign-free (sap)
  "Free a SAP allocated by FOREIGN-ALLOC."
  (declare (type system-area-pointer sap)
           (optimize speed))
  (free-alien (sap-alien sap (* (unsigned 8)))))

(defmacro with-foreign-pointer ((var size &optional size-var) &body body)
  "Bind VAR to SIZE bytes of foreign memory during BODY.  The
pointer in VAR is invalid beyond the dynamic extent of BODY, and
may be stack-allocated if supported by the implementation.  If
SIZE-VAR is supplied, it will be bound to SIZE during BODY."
  (unless size-var
    (setf size-var (gensym "SIZE")))
  ;; If the size is constant we can stack-allocate.
  (if (constantp size)
      (let ((alien-var (gensym "ALIEN")))
        `(with-alien ((,alien-var (array (unsigned 8) ,(eval size))))
           (let ((,size-var ,(eval size))
                 (,var (alien-sap ,alien-var)))
             (declare (ignorable ,size-var))
             ,@body)))
      `(let* ((,size-var ,size)
              (,var (%foreign-alloc ,size-var)))
         (unwind-protect
              (progn ,@body)
           (foreign-free ,var)))))

(defmacro with-foreign-object ((var type &optional (count 1)) &body body)
  "Bind VAR to a pointer to COUNT objects of TYPE during BODY.
The buffer has dynamic extent and may be stack allocated."
  `(with-foreign-pointer
       (,var ,(if (constantp type)
                  ;; with-foreign-pointer may benefit from constant folding:
                  (if (constantp count)
                      (* (eval count) (foreign-type-size (eval type)))
                      `(* ,count ,(foreign-type-size (eval type))))
                  `(* ,count (foreign-type-size ,type))))
     ,@body))

(defmacro with-foreign-objects (bindings &body body)
  (if bindings
      `(with-foreign-object ,(car bindings)
         (with-foreign-objects ,(cdr bindings)
           ,@body))
      `(progn ,@body)))

(defun foreign-alloc (type &key (initial-element nil initial-element-p)
                      (initial-contents nil initial-contents-p)
                      (count 1 count-p) null-terminated-p)
  "Allocate enough memory to hold COUNT objects of type TYPE. If
INITIAL-ELEMENT is supplied, each element of the newly allocated
memory is initialized with its value. If INITIAL-CONTENTS is supplied,
each of its elements will be used to initialize the contents of the
newly allocated memory."
  (let (contents-length)
    ;; Some error checking, etc...
    (when (and null-terminated-p
               (not (alien-pointer-type-p type)))
      (error "Cannot use :NULL-TERMINATED-P with non-pointer types."))
    (when (and initial-element-p initial-contents-p)
      (error "Cannot specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
    (when initial-contents-p
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    ;; Everything looks good.
    (with-alien ((sap (* t) 
                      (%foreign-alloc (* (foreign-type-size type)
                                         (if null-terminated-p (1+ count) count)))))
      (when initial-element-p
        (dotimes (i count)
          (setf (deref sap i) initial-element)))
      (when initial-contents-p
        (dotimes (i contents-length)
          (setf (deref sap i) (elt initial-contents i))))
      (when null-terminated-p
        (setf (deref sap count) nil))
      sap)))

;; Simple compiler macro that kicks in when TYPE is constant and only
;; the COUNT argument is passed.  (Note: hard-coding the type's size
;; into the fasl will likely break CLISP fasl cross-platform
;; compatibilty.)
(define-compiler-macro foreign-alloc (&whole form type &rest args
                                      &key (count 1 count-p) &allow-other-keys)
  (if (or (and count-p (<= (length args) 2)) (null args))
      (cond
        ((and (constantp type) (constantp count))
         `(%foreign-alloc ,(* (eval count) (foreign-type-size (eval type)))))
        ((constantp type)
         `(%foreign-alloc (* ,count ,(foreign-type-size (eval type)))))
        (t form))
      form))

;;;_. Macro Accessors
;; TODO
;; (defmacro @ (obj index))
;; (defmacro & (obj))
;;;_. Alien Stack/Heap
;; (defmacro with-alien-stack (decl &rest body))
;; (defmacro with-alien-heap (decl &rest body))

;;; CPUs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun num-cpus ()
    "Return the number of CPU threads online."
    (alien-funcall (extern-alien "sysconf" (function int int)) sb-unix:sc-nprocessors-onln)))

(sb-ext:defglobal *cpus* (num-cpus)
  "The number of unique processors (cores) reported by the OS.")

;;;_. C Standard
;; types
(define-alien-type pid-t int)
(define-alien-type uid-t unsigned-int)
(define-alien-type gid-t unsigned-int)
(define-alien-type loff-t long-long)

(defar memset void (ptr (* t)) (constant int) (size size-t))
(defar memcpy void (dst (* t)) (src (* t)) (size size-t))
(defar posix-memalign int (box (* (* t))) (alignment size-t) (size size-t))

(define-alien-type timeval
  (struct timeval
          (tv-sec (signed 64))
          (tv-usec (signed 64))))

(define-alien-type timespec
  (struct timespec
          (tv-sec (signed 64))
          (tv-nsec (signed 64))))

;;;_. Fortran
(defmacro with-fortran-float-modes (&body body)
  "Execute the body with the IEEE FP modes appropriately set for Fortran"
  `(with-float-traps-masked (:underflow :overflow :inexact :divide-by-zero :invalid)
     ,@body))

;;; CLOS
(defgeneric sap (self)
  (:documentation "Return a system-area-pointer to the alien bound to object SELF or nil if no
such alien exists.")
  (:method ((self t)) nil)
  (:method ((self system-area-pointer)) self)
  (:method ((self integer)) (int-sap self))
  (:method ((self alien-value)) (alien-sap self)))

(defgeneric (setf sap) (new self)
  (:documentation "Set the value of system-area-pointer SELF to NEW."))

;; TODO 2024-12-31: 
(defgeneric free (self)
  (:documentation "Free the SAP associated with object SELF if one exists and return NIL.")
  (:method ((self alien-value)) (free-alien self))
  (:method ((self t)) nil))

(defgeneric push-sap (self key)
  (:documentation "Push a value associated with KEY to the sap associated
with SELF. Typically used to send a value from one slot, to a foreign
handle stored in another slot of the same object."))

(defgeneric push-sap* (self)
  (:documentation "Implicitly push values to the sap associated with SELF."))

(defgeneric pull-sap (self key)
  (:documentation "Pull a foreign value identified by KEY from the sap associated with SELF."))

(defgeneric pull-sap* (self)
  (:documentation "Implicitly pull foreign values from the sap associated with SELF."))

;;;_. Foreign Vector
;; from MATLISP
(defclass foreign-vector-class (standard-class)
  ((element-type :reader element-type)))

(defmethod sb-mop:validate-superclass ((class foreign-vector-class) (superclass standard-class))  t)

(defclass foreign-vector ()
  ((sap :initarg :sap :initform nil)
   (length :initarg :length :initform 0))
  (:metaclass foreign-vector-class))

(with-memoization ()
  (memoizing
   (defun foreign-vector (element-type)
     (or (std/macs:if-let ((class (find element-type (std/meta:class-direct-subclasses (find-class 'foreign-vector)) :key #'element-type)))
           (class-name class)
           (let* ((cl-name (intern (format nil "<FOREIGN-VECTOR: ~a>"  element-type) (find-package "STD/ALIEN"))))
             (assert (member (element-type-to-alien element-type) '(char unsigned-char short unsigned-short int unsigned-int long unsigned-long float double)) nil 'invalid-arguments)
             (compile-and-eval
              `(progn
                 (defclass ,cl-name (foreign-vector) ()
                   (:metaclass foreign-vector-class))
                 (setf (slot-value (find-class ',cl-name) 'element-type) ',element-type)))
             cl-name))))))
;;
(defparameter *fvref-range-check* t)
  
#+nil
(defun fvref (x i)
  (declare (type foreign-vector x))
  (let ((n (slot-value (the foreign-vector x) 'length)))
    (assert (< -1 i n) nil 'out-of-bounds-error :requested i :bound n)

    (cast
     (sap-alien (sap+ (alien-sap x) i) (* t))
     (element-type-to-alien (element-type (class-of x))))))

#+nil
(define-compiler-macro fvref (&whole form x i)
  (if (listp x)
  (destructuring-case x
    ((the fv obj)
     (let ((alien-type (element-type-to-alien (element-type fv))))
       (with-gensyms (obj-v i-v n-v)
         `(lety ((,obj-v ,obj :type ,fv)
                      (,i-v ,i :type fixnum))
            ,@(if *fvref-range-check*
                  `((let ((,n-v (slot-value ,obj-v 'length)))
                      (assert (< -1 ,i-v ,n-v) nil 'out-of-bounds-error :requested ,i-v :bound ,n-v))))
            (sap-ref (slot-value (the ,fv ,obj-v) 'sap) ,alien-type (the fixnum (* (the fixnum ,i-v) (the fixnum ,(foreign-type-size alien-type))))))))))
    form))

#+nil
(defun (setf fvref) (value x i)  
  (declare (type foreign-vector x))
  (let ((n (slot-value (the foreign-vector x) 'length)))
    (assert (< -1 i n) nil 'out-of-bounds-error :requested i :bound n)
    (setf (sap-svref (slot-value x 'sap) (element-type-to-alien (element-type (class-of x))) i) value)))

#+nil
(define-compiler-macro (setf fvref) (&whole form value x i)
  (multiple-value-match (values x value)
    (((list 'the (and (type symbol) (guard fv (subtypep fv 'foreign-vector))) obj)
      (list 'the (and (type symbol) (guard lt (eql lt (element-type fv)))) val))
     (let ((alien-type (lisp->mffi (element-type fv))))
       (with-gensyms (obj-v i-v n-v)
         `(lety ((,obj-v ,obj :type ,fv)
                      (,i-v ,i :type fixnum))
            ,@(if *fvref-range-check*
                  `((let ((,n-v (slot-value ,obj-v 'length)))
                      (assert (< -1 ,i-v ,n-v) nil 'out-of-bounds-error :requested ,i-v :bound ,n-v))))
            (setf (sap-ref (slot-value (the ,fv ,obj-v) 'sap) 
                                ,alien-type
                                (the fixnum (* (the fixnum ,i-v) 
                                               (the fixnum ,(foreign-type-size alien-type)))))
                  (the ,lt ,val))))))
    (_ form)))
