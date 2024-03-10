;;; alien.lisp --- foreign alien friends

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
(in-package :std)
(shadowing-import '(sb-unix::syscall sb-unix::syscall* sb-unix::int-syscall sb-unix::with-restarted-syscall sb-unix::void-syscall) :std)

;; (reexport-from :sb-vm
;;  	       :include
;;  	       '(:with-pinned-objects :with-pinned-object-iterator :with-code-pages-pinned
;;  		 :sanctify-for-execution))

(defun shared-object-name (name)
  "Return a filename with the correct extension for a shared library
on Linux and Darwin."
  #+darwin (format nil "/usr/local/lib/lib~a.dylib" name)
  #-darwin (format nil "lib~a.so" name))

(defun list-all-shared-objects ()
  sb-alien::*shared-objects*)

(defmacro define-alien-loader (name &optional export)
  "Define a default loader function named load-NAME which calls
SB-ALIEN:LOAD-SHARED-OBJECT."
  (let* ((fname (sb-int:symbolicate (format nil "~@:(load-~a~)" name))))
    `(prog1
       (defun ,fname (&optional save)
         (prog1 (sb-alien:load-shared-object (shared-object-name ',name) :dont-save (not save))
           (pushnew ,(sb-int:keywordicate (string-upcase name)) *features*)))
       ,@(when export (list `(export '(,fname)))))))
       
(defmacro define-opaque (ty &optional no-export)
  `(prog1
       (define-alien-type ,ty (struct ,(symbolicate ty '-t)))
     ,(unless no-export `(export '(,ty)))))

(defun setfa (place from) 
  (loop for x across from
	for i from 0 below (length from)
	do (setf (deref place i) x)))

(defun copy-c-string (src dest &aux (index 0))
  (loop (let ((b (sb-sys:sap-ref-8 src index)))
          (when (= b 0)
            (setf (fill-pointer dest) index)
            (return))
          (setf (char dest index) (code-char b))
          (incf index))))

(defun clone-strings (list)
  (with-alien ((x (* (* char))
                  (make-alien (* char) (length list))))
    (unwind-protect
         (labels ((populate (list index function)
                    (if list
                        (let ((array (sb-ext:string-to-octets (car list) :null-terminate t)))
                          (sb-sys:with-pinned-objects (array)
                            (setf (deref x index) (sap-alien (sb-sys:vector-sap array) (* char)))
                            (populate (cdr list) (1+ index) function)))
                        (funcall function))))
           (populate list 0
                     (lambda ()
                       (loop for i below (length list)
                             do (print (cast (deref x i) c-string))))))
      (free-alien x))))

(defun c-strings-to-string-list (c-strings)
  (declare (type (alien (* c-string)) c-strings))
  (let ((reversed-result nil))
    (dotimes (i most-positive-fixnum)
      (declare (type index i))
      (let ((c-string (deref c-strings i)))
        (if c-string
            (push c-string reversed-result)
            (return (nreverse reversed-result)))))))

(defmacro clone-octets-to-alien (lispa aliena)
  (with-gensyms (i)
    `(loop for ,i from 0 below (length ,lispa)
        do (setf (deref ,aliena ,i)
                 (aref ,lispa ,i)))))

(defmacro clone-octets-from-alien (aliena lispa len)
  (with-gensyms (i)
    `(loop for ,i from 0 below ,len
           do (setf (aref ,lispa ,i)
                 (deref ,aliena ,i)))))

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

(defun num-cpus ()
  "Return the number of CPU threads online."
  (alien-funcall (extern-alien "sysconf" (function int int)) sb-unix:sc-nprocessors-onln))
