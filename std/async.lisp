;;; async.lisp --- Aynchronous Functions

;; Futures, Promises, etc

;;; Commentary:

;; based on LPARALLEL

;; NOTE: instead of 'force' we use 'await'

;; ref: https://github.com/lmj/lparallel

;; ref: https://doc.rust-lang.org/book/ch17-01-futures-and-syntax.html

;;; Code:
(in-package :std/async)

(defconstant +no-result+ :null)

(defstruct (promise (:constructor promise))
  (result +no-result+)
  (lock (make-mutex))
  (cvar nil)
  (availablep t :type boolean))

(defstruct future
  (result +no-result+)
  (lock (make-mutex))
  (canceledp nil :type boolean)
  (fn nil :type (or null function)))

(defun fulfill-promise (obj fn)
  (loop while (and (promise-availablep obj) (eq (promise-result obj) +no-result+))
        do (with-mutex ((promise-lock obj) :wait-p nil)
             (unwind-protect
                  (setf (promise-availablep obj) nil
                        ;; TODO 2025-04-04: 
                        (promise-result obj) (multiple-value-list (funcall fn)))
               (setf (promise-availablep obj) t))
             (when-let ((cvar (promise-cvar obj))) (condition-notify cvar))
             (return t))))

(defun force-promise (obj)
  (let ((res (promise-result obj))
        (lock (promise-lock obj))
        (cvar (promise-cvar obj)))
    (unless cvar
      (setf cvar (sb-thread:make-waitqueue)))
    (loop while (eq res +no-result+)
          do (condition-wait cvar lock))
    (condition-notify cvar)))

(defun fulfill-future (obj fn)
  (when (eq (future-result obj) +no-result+)
    (with-mutex ((future-lock obj) :wait-p nil)
      ;; task has been stolen from pool
      (setf (future-canceledp obj) t)
      ;; TODO 2025-04-04: 
      (funcall fn (future-fn obj)))))

(defun force-future (obj)
  ;; task has been stolen from pool
  (setf (future-canceledp obj) t)
  ;; TODO 2025-04-04:
  (setf (future-result obj) (funcall (future-fn obj))
        (future-fn obj) nil))

(defun fulfill (obj fn)
  (etypecase obj
    (promise (fulfill-promise obj fn))
    (future (fulfill-future obj fn))))

(defun result (obj)
  (etypecase obj
    (promise (promise-result obj))
    (future (future-result obj))))

(defmacro future (&body body)
  `(make-future :fn (lambda () ,@body)))

(defmacro while-waiting-for (obj &body body)
  (with-gensyms (lock canceledp res)
    `(let ((,lock (future-lock ,obj))
           (,canceledp (future-canceledp ,obj))
           (,res (future-result ,obj)))
       (when (and (not ,canceledp)
                  (eq ,res +no-result+))
         (with-mutex (,lock :wait-p nil)
           ,@body)))))

(defun fulfilledp (obj)
  (typecase obj
    (promise (not (eq (promise-result obj) +no-result+)))
    (future (not (eq (future-result obj) +no-result+)))
    (t t)))

(defun await (object)
  (typecase object
    ((or promise future)
     (while-waiting-for object
       (etypecase object
         (future (force-future object))
         (promise (force-promise object))))
     (result object))
    (t object)))
