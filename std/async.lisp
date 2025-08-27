;;; async.lisp --- Aynchronous Functions

;; Futures, Promises, etc

;;; Commentary:

;; based on LPARALLEL

#|
           promise-base
             /     \
          promise  plan
                   /  \
 speculation = future  delay
|#
;; NOTE: instead of 'force' we use 'await'

;; ref: https://github.com/lmj/lparallel

;; ref: https://doc.rust-lang.org/book/ch17-01-futures-and-syntax.html

;;; Code:
(in-package :std/async)

(defconstant +no-result+ :null)
(declaim (inline promise %future))

;;; Promise/Future
(defstruct (promise (:constructor promise))
  "A placeholder object for a result which is TBD."
  (result +no-result+)
  (lock (make-mutex))
  (cvar nil)
  (availablep t :type boolean))

(defstruct (future (:constructor %future))
  "A promise which is fulfilled in parallel by evaluating the KERNEL slot."
  (result +no-result+)
  (lock (make-mutex))
  (canceledp nil :type boolean)
  (kernel nil :type (or null function)))

(defmacro with-lock-op (operation promise &body body)
  (with-gensyms (lock result)
    `(with-slots ((,lock lock) (,result result)) ,promise
       (,operation ,lock (eq ,result +no-result+)
         ,@body))))

(defmacro with-unfulfilled* (promise &body body)
  `(with-lock-op std/thread::with-lock-no-wait ,promise
     ,@body))

(defmacro with-unfulfilled (promise &body body)
  `(with-lock-op std/thread::with-lock-wait ,promise
     ,@body))

(defun fulfill-promise (obj fn)
  (declare (function fn))
  (loop while (and (promise-availablep obj) (eq (promise-result obj) +no-result+))
        do (with-unfulfilled* obj
             (setf (promise-availablep obj) nil)
             (std/condition:unwind-protect-case 
                 ()
                 (setf (promise-result obj) (multiple-value-list (funcall fn)))
               (:abort (setf (promise-availablep obj) t)))
             (when-let ((cvar (promise-cvar obj))) (condition-notify cvar))
             (return t))))

(defun await-promise (obj)
  (let ((res (promise-result obj))
        (lock (promise-lock obj))
        (cvar (promise-cvar obj)))
    (unless cvar
      (setf cvar (sb-thread:make-waitqueue)))
    (loop while (eq res +no-result+)
          do (condition-wait cvar lock))
    (condition-notify cvar)))

(std/prim:definline fulfill-future-values (fut values)
  (with-slots (result kernel) fut
    (setf result values
          kernel nil)))

(std/prim:definline fulfill-future-call (fut)
  (fulfill-future-values fut (multiple-value-list (std/thread::call-with-work-handler (future-kernel fut)))))

(defun fulfill-future-error (fut err)
  (fulfill-future-values fut (list (std/condition:wrap-error err))))

(defun fulfill-future (obj fn)
  (declare (function fn))
  (with-unfulfilled* obj
    ;; task has been stolen from pool
    (setf (future-canceledp obj) t)
    (fulfill-future-values obj (multiple-value-list (funcall fn)))
    t))

(defmacro with-unfulfilled-future* (fut &body body)
  (with-gensyms (lock canceledp result)
    `(with-slots ((,lock lock) (,canceledp canceledp) (,result result)) ,fut 
       (std/thread::with-lock-no-wait ,lock (and (not ,canceledp)
                                                 (eq ,result +no-result+))
         ,@body))))

(defun await-future (obj)
  ;; task has been stolen from pool
  (setf (future-canceledp obj) t)
  (fulfill-future-call obj))

(defun make-future-work (fut)
  (std/thread::make-kernel
   (lambda ()
     (with-unfulfilled-future* fut
       (std/condition::unwind-protect-case () (fulfill-future-call fut)
         (:abort (fulfill-future-error fut 'std/thread::worker-killed-error)))))))

(defun make-future (fn)
  (declare (function fn))
  (let ((pool (check-thread-pool))
        (fut (%future :kernel fn)))
    (std/thread::submit-raw-work (make-future-work fut) pool)
    fut))

(defun fulfill-object (obj fn)
  (etypecase obj
    (promise (fulfill-promise obj fn))
    (future (fulfill-future obj fn))))

(defmacro fulfill (object &body body)
  "Attempt to give `object' a value.

If `object' is a promise which is not fulfilled and not currently
being fulfilled, then the implicit progn `body' will be executed and
the promise will store the result. In this case `fulfill' returns
true.

If `object' is a promise that is either already fulfilled or actively
being fulfilled, then `body' will not be executed and `fulfill'
returns false.

If `object' is a chain, call `fulfill' on the chained object.

If `object' is not a promise and not a chain then false is returned
immediately, with `body' being ignored."
  `(fulfill-object ,object (lambda () ,@body)))

(defun result (obj)
  (etypecase obj
    (promise (promise-result obj))
    (future (future-result obj))))

(defmacro future (&body body)
  "Create a future which is fulfilled in parallel by the implicit progn BODY."
  `(make-future (std/thread::work-lambda ,@body)))

(defmacro speculate (&body body)
  "Create a speculation. A speculation is a low-priority future."
  `(let ((std/thread::*work-priority* :low))
     (future ,@body)))

(defun fulfilledp (obj)
  (typecase obj
    (promise (not (eq (promise-result obj) +no-result+)))
    (future (not (eq (future-result obj) +no-result+)))
    (t t)))

(defun await (object)
  (typecase object
    ((or future promise)
     (with-unfulfilled object
       (etypecase object
         (future (await-future object))
         (promise (await-promise object))))
     (let ((ret (result object)))
       (typecase (first ret)
         (std/condition:wrapped-error (replace-error object)
          (await object))
         (t (values-list ret)))))
    (t object)))

(defun replace-error (promise)
  ;; It is not possible to return from `force' while the promise
  ;; contains an error. Therefore we do not violate the
  ;; one-result-only constraint by replacing a wrapped error result
  ;; with value(s).
  ;;
  ;; If a successful store-value invocation happens concurrently then
  ;; skip.
  (with-slots (result lock) promise
    (std/thread::with-lock-wait lock (typep (first result) 'std/condition:wrapped-error)
      (restart-case (std/thread::unwrap-result (first result))
        (store-value (&rest values)
          :report "Set promise value(s)."
          :interactive (lambda () (std/condition::interact "Promise value(s): "))
          (setf result values))))))
