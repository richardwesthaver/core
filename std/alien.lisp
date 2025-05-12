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

(defun shared-object-name (name &optional path)
  "Return a filename with the correct extension for a shared library."
  (unless (string= (subseq name 0 3) "lib") 
    (setf name (format nil "lib~a" name)))
  (unless (search #+darwin ".dylib" #-darwin ".so" name)
    (setf name (format nil #+darwin "~a.dylib" #-darwin "~a.so" name)))
  (if path
      (merge-pathnames name path)
      (pathname name)))

(defun list-all-shared-objects ()
  "Return the current value of SB-ALIEN::*SHARED-OBJECTS*."
  sb-alien::*shared-objects*)

(defmacro define-alien-loader (name &optional (root "/usr/local/lib/") path)
  "Define a default loader function named load-NAME which calls
SB-ALIEN:LOAD-SHARED-OBJECT."
  (let ((fname (sb-int:symbolicate (format nil "~@:(LOAD-~a~)" name))))
    (when (symbolp name)
      (setf name (symbol-name name)))
    `(defun ,fname (&optional save)
       (prog1 (sb-alien:load-shared-object (shared-object-name ,(or path (string-downcase name)) ,root) :dont-save (not save))
         (pushnew ,(sb-int:keywordicate (string-upcase name)) *features*)))))
       
(defmacro define-opaque (ty &optional foreign-type)
  "Define an 'opaque' alien type. This is an internal convenience function for
types which are effectively aliases for other types. The default target type
is TY with a -T prepended as is customary in many C codebases."
  (eval-when (:compile-toplevel :load-toplevel :execute)
    `(define-alien-type ,ty (struct ,(or foreign-type (symbolicate ty '-t))))))

(defun double-array-pointer (array)
  "Return a SAP pointing to the start of ARRAY's storage vector."
  (sap-alien (sb-sys:vector-sap (sb-ext:array-storage-vector array)) (* double)))

(defun float-array-pointer (array)
  "Return a SAP pointing to the start of ARRAY's storage vector."
  (sap-alien (sb-sys:vector-sap (sb-ext:array-storage-vector array)) (* float)))

(defun octet-vector-pointer (array)
  "Return a SAP pointing to the start of ARRAY's storage vector."
  (sap-alien (sb-sys:vector-sap (sb-ext:array-storage-vector array)) (* unsigned-char)))

(defun setfa (place from)
  "Fill in a foreign array PLACE using lisp vector FROM."
  (declare (vector from))
  (loop for i below (length from)
        for x across from
	do (setf (deref place i)
                 x)))

(defun copy-c-string (src dest &aux (index 0))
  "Copy a C-allocated string SRC into lisp string DEST."
  (declare (type sb-int:index index))
  (loop (let ((b (sb-sys:sap-ref-8 src index)))
          (when (= b 0)
            (setf (fill-pointer dest) index)
            (return))
          (setf (char dest index) (code-char b))
          (incf index))))

(defun clone-strings (list &optional null-terminate)
  "Copy the list of strings in LIST to a foreign array of C strings. When
NULL-TERMINATE is T we append a null character to each string."
  (let ((len (length list)))
    (with-alien ((x (* (* char)) (make-alien (* char) len)))
      (labels ((populate (list index)
                 (declare (type sb-int:index index))
                 (if list
                     (let ((array (sb-ext:string-to-octets (car list) :null-terminate null-terminate)))
                       (sb-sys:with-pinned-objects (array)
                         (setf (deref x index) (sap-alien (sb-sys:vector-sap array) (* char)))
                         (populate (cdr list) (1+ index))))
                     x)))
        (cast (populate list 0) (* c-string))))))

(defun c-strings-to-string-list (c-strings)
  "Copy the foreign array C-STRINGS to a lisp list of strings."
  (declare (type (alien (* c-string)) c-strings))
  (let ((reversed-result nil))
    (dotimes (i most-positive-fixnum)
      (declare (type sb-int:index i))
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
  (assert (= size (sb-alien:alien-size sb-alien:int :bytes)))
  buffer)

(defun foreign-int-to-bool (x size)
  "Convert a foreign integer X of length SIZE to a lisp boolean."
  (if (zerop (foreign-int-to-integer x size))
      nil
      t))

(defun bool-to-foreign-int (val)
  "Convert a lisp boolean to an integer."
  (if val 1 0))

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

;;; DEFAR
(defmacro defar (name result-type &rest args)
  "Like DEFINE-ALIEN-ROUTINE but always inline the resulting alien function."
  (multiple-value-bind (lisp-name alien-name) (sb-alien::pick-lisp-and-alien-names name)
    `(progn
       (declaim (inline ,lisp-name))
       (define-alien-routine ,(list alien-name lisp-name) ,result-type ,@args))))

;;; DEFINE-ALIEN-ENUM
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
  `(sb-sys:with-pinned-objects (,vector)
     (let ((,name (sb-sys:vector-sap ,vector)))
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
                       (if (eq (sb-int:keywordicate p1) :*)
                           `(,p2 (addr (slot ,struct ',p2)))
                           (if (eq (sb-int:keywordicate p2) :*)
                               `(,p1 (addr (slot ,struct ',p3)))
                               `(,p1 (slot ,struct ',p2)))))
                     `(,var (slot ,struct ',var))))
     ,@body))

(declaim (inline foreign-type-size))
(defun foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (/ (sb-alien-internals:alien-type-bits
      (sb-alien-internals:parse-alien-type
       type nil)) 
     8))

(declaim (inline %foreign-alloc))
(defun %foreign-alloc (size)
  "Allocate SIZE bytes on the heap and return a pointer."
  ;; (declare (type (unsigned-byte 32) size))
  (alien-sap (make-alien (unsigned 8) size)))

(declaim (inline foreign-free))
(defun foreign-free (ptr)
  "Free a PTR allocated by FOREIGN-ALLOC."
  (declare (type system-area-pointer ptr)
           (optimize speed))
  (free-alien (sap-alien ptr (* (unsigned 8)))))

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
               (not (sb-alien::alien-pointer-type-p type)))
      (error "Cannot use :NULL-TERMINATED-P with non-pointer types."))
    (when (and initial-element-p initial-contents-p)
      (error "Cannot specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
    (when initial-contents-p
      (setq contents-length (length initial-contents))
      (if count-p
          (assert (>= count contents-length))
          (setq count contents-length)))
    ;; Everything looks good.
    (with-alien ((ptr (* t) 
                      (%foreign-alloc (* (foreign-type-size type)
                                         (if null-terminated-p (1+ count) count)))))
      (when initial-element-p
        (dotimes (i count)
          (setf (deref ptr i) initial-element)))
      (when initial-contents-p
        (dotimes (i contents-length)
          (setf (deref ptr i) (elt initial-contents i))))
      (when null-terminated-p
        (setf (deref ptr count) nil))
      ptr)))

;;; Simple compiler macro that kicks in when TYPE is constant and only
;;; the COUNT argument is passed.  (Note: hard-coding the type's size
;;; into the fasl will likely break CLISP fasl cross-platform
;;; compatibilty.)
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

(defun num-cpus ()
  "Return the number of CPU threads online."
  (alien-funcall (extern-alien "sysconf" (function int int)) sb-unix:sc-nprocessors-onln))

(defparameter *cpus* (num-cpus)
  "The number of unique processors (cores) reported by the OS.")

;;; Non-standard types
(deftype alien-or-lisp-octets () '(or array 
                                   (alien (* char)) 
                                   (alien (array char))
                                   (alien (* unsigned-char))
                                   (alien (array unsigned-char))))
;;; C Standard

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

;;; Linux
;; based on functions from Shinmera's CL-SPIDEV
;; TODO 2025-04-27: 
(defun ioctl (fd cmd)
  (sb-alien:with-alien ((result sb-alien:int))
    (multiple-value-bind (wonp error)
        (sb-unix:unix-ioctl fd
                            (if (< cmd (expt 2 31)) cmd (- cmd (expt 2 32)))
                            (sb-alien:alien-sap (sb-alien:addr result)))
      (unless wonp
        (error "IOCTL ~a failed: ~a" cmd (sb-impl::strerror error))))
    result))

(defun (setf ioctl) (arg fd cmd)
  (sb-alien:with-alien ((value sb-alien:int))
    (setf value arg)
    (multiple-value-bind (wonp error)
        (sb-unix:unix-ioctl fd 
                            (if (< cmd (expt 2 31)) cmd (- cmd (expt 2 32)))
                            (sb-alien:alien-sap (sb-alien:addr value)))
      (unless wonp
        (error "IOCTL ~a failed: ~a" cmd (sb-impl::strerror error))))
    arg))

;; (defmacro define-ioctl (name fd cmd))

;;; CLOS
(defgeneric sap (self)
  (:documentation "Return a system-area-pointer to the alien bound to object SELF or nil if no
such alien exists.")
  (:method ((self t)) nil)
  (:method ((self sb-sys:system-area-pointer)) self)
  (:method ((self integer)) (sb-alien::int-sap self))
  (:method ((self sb-alien-internals:alien-value)) (alien-sap self)))

(defgeneric (setf sap) (new self)
  (:documentation "Set the value of system-area-pointer SELF to NEW."))

;; TODO 2024-12-31: 
(defgeneric free (self)
  (:documentation "Free the SAP associated with object SELF if one exists and return NIL.")
  (:method ((self sb-alien-internals:alien-value)) (free-alien self))
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
