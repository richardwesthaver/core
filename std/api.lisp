;;; api.lisp --- API Macros

;; DEFAPI

;;; Commentary:

;; The purpose of this macro is to eliminate the amount of boilerplate code
;; required to define simple hash-table-based APIs.

;; We have many protocols which follow a similar pattern of defining a
;; hash-table in a special variable and providing a bunch of functions,
;; conditions, and macros.

;; For example for a protocol FOO we may want:
#|
(defvar *foo*) ; needs to be initialized
(defvar *foo-table* (make-hash-table)) ; all possible FOOs

(define-condition foo-error (simple-error) ())
(define-condition foo-warning (simple-warning) ())

(defun foo (name &optional (tbl *foo*)) (gethash name tbl))
(defun (setf foo) (new name &optional (tbl *foo*)) (setf (gethash name tbl) new))

(defmacro with-foo (name &body body)
 `(let ((*foo* (foo ,name))) 
   ,@body))
|#

;; Capturing all this dynamically may be a challenge, but we apply this type
;; of pattern often enough to merit special support.

;; DEFAPI does not store any state of its own.

;;; Code:
(in-package :std/prim)

;; (defmacro defapi (name))
