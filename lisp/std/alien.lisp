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
  (let ((name #+darwin (format nil "lib~a.dylib" name)
              #-darwin (format nil "lib~a.so" name)))
    (if path
      (merge-pathnames name path)
      (pathname name))))

(defun list-all-shared-objects ()
  sb-alien::*shared-objects*)

(defmacro define-alien-loader (name &optional export (root "/usr/local/lib/") path)
  "Define a default loader function named load-NAME which calls
SB-ALIEN:LOAD-SHARED-OBJECT."
  (let* ((fname (sb-int:symbolicate (format nil "~@:(load-~a~)" name))))
    `(prog1
       (defun ,fname (&optional save)
         (prog1 (sb-alien:load-shared-object (shared-object-name ',(or path name) ,root) :dont-save (not save))
           (pushnew ,(sb-int:keywordicate (string-upcase name)) *features*)))
       ,@(when export (list `(export '(,fname)))))))
       
(defmacro define-opaque (ty &optional no-export foreign-type)
  `(prog1
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (define-alien-type ,ty (struct ,(or foreign-type (symbolicate ty '-t)))))
     ,(unless no-export `(export '(,ty)))))

(defun setfa (place from) 
  (loop for x across from
	for i from 0 below (length from)
	do (setf (deref place i) x)))

(defun copy-c-string (src dest &aux (index 0))
  (declare (type sb-int:index index))
  (loop (let ((b (sb-sys:sap-ref-8 src index)))
          (when (= b 0)
            (setf (fill-pointer dest) index)
            (return))
          (setf (char dest index) (code-char b))
          (incf index))))

(defun clone-strings (list)
  (let ((len (length list)))
    (with-alien ((x (* (* char)) (make-alien (* char) len)))
      (labels ((populate (list index)
                 (declare (type sb-int:index index))
                 (if list
                     (let ((array (sb-ext:string-to-octets (car list) :null-terminate t)))
                       (sb-sys:with-pinned-objects (array)
                         (setf (deref x index) (sap-alien (sb-sys:vector-sap array) (* char)))
                         (populate (cdr list) (1+ index))))
                     x)))
        (cast (populate list 0) (* c-string))))))

(defun c-strings-to-string-list (c-strings)
  (declare (type (alien (* c-string)) c-strings))
  (let ((reversed-result nil))
    (dotimes (i most-positive-fixnum)
      (declare (type sb-int:index i))
      (let ((c-string (deref c-strings i)))
        (if c-string
            (push c-string reversed-result)
            (return (nreverse reversed-result)))))))

(defun clone-octets-to-alien (lispa alien)
  (declare (optimize (speed 3))
           ((vector (unsigned-byte 8)) lispa))
  ;; (setf aliena (cast aliena (array (unsigned 8))))
  (loop for i from 0 below (length lispa)
        do (setf (deref alien i)
                 (aref lispa i)))
  alien)

(defun octets-to-alien (lispa)
  (let ((a (make-alien (unsigned 8) (length lispa))))
    (clone-octets-to-alien lispa a)))

;; TODO 2024-09-19: maybe want to return values, second being the length?
(defun octets-to-alien-array (lispa)
  (cast (octets-to-alien lispa) (array (unsigned 8))))

(defun clone-octets-from-alien (aliena lispa &optional len)
  (declare (optimize (speed 3))
           (vector lispa))
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
  (if (zerop (foreign-int-to-integer x size))
      nil
      t))

(defun bool-to-foreign-int (val)
  (if val 1 0))

(define-condition invalid-enum-variant (simple-error) ())
(define-condition invalid-enum-value (simple-error) ())

(defun invalid-enum-variant (var enum)
  (error 'invalid-enum-variant
         :format-control "~A is not a variant of enum ~A"
         :format-arguments (list var enum)))

(defun invalid-enum-value (var enum)
  (error 'invalid-enum-value
         :format-control "~A is not a value associated with a variant of enum ~A"
         :format-arguments (list var enum)))

(defmacro define-alien-enum ((name type &key (test 'eql) (default :error)) &rest forms)
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
          (%lisp-enum-table* (make-hash-table :test 'equal :size (length forms)))) ; TODO: may want this to be EQL,
                                                                                   ; taking strings for now.
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
             (values ,val found)))))))

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

(defun num-cpus ()
  "Return the number of CPU threads online."
  (alien-funcall (extern-alien "sysconf" (function int int)) sb-unix:sc-nprocessors-onln))

(defvar *cpus* (num-cpus))

;;; C Standard

;; types
(define-alien-type loff-t long-long)

(define-alien-routine memset void (ptr (* t)) (constant int) (size size-t))
(define-alien-routine memcpy void (dst (* t)) (src (* t)) (size size-t))
(define-alien-routine posix-memalign int (box (* t)) (alignment size-t) (size size-t))
