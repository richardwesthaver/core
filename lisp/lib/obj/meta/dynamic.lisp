;;; dynamic.lisp --- Dynamic Bindings for CLOS

;; Copied from Daniel 'jackdaniel' Kochmański's Dynamic Let

;; ref: https://turtleware.eu/posts/Dynamic-Let.html

;;; Commentary:

;; jackdaniel's post does a great job at explaining the need for Dynamics in
;; CLOS and where 'Dynamic Slots' can be most useful. In the case of McCLIM,
;; it is being implemented in the repaint queue to enable thread-safe stream
;; operations. The exact same strategy can be applied to virtually any other
;; program that involves excessive thread-local bindings of slot values.

;; The context I'm thinking of applying this to currently is NET/SRV, where it
;; may be useful in binding SERVICE and REQUEST slots dynamically for worker
;; threads. Many of our other libs can potentially benefit from this metaclass
;; (LOGGER, RDB, etc).

;;; Code:
(in-package :obj/meta/dynamic)

;;; Macros

(defmacro dset (&rest pairs)
  "Like SETF but for dynamic slot bindings."
  `(setf ,@(loop for (var val) on pairs by #'cddr
                 collect `(symbol-value ,var)
                 collect val)))

(defmacro dref (variable)
  "Extract the dynamic value of VARIABLE." 
  `(symbol-value ,variable))

;;; Low-Level

;; Accessing and binding symbols behind the slot. We don't use SLOT-VALUE,
;; because it will return the _value_ of the dynamic variable, and not the
;; variable itself.
(defun slot-dvar (object slotd)
  (sb-mop:standard-instance-access
   object (sb-mop:slot-definition-location slotd)))

(defun slot-dvar* (object slot-name)
  (let* ((class (class-of object))
         (slotd (find slot-name (sb-mop:class-slots class)
                      :key #'sb-mop:slot-definition-name)))
    (slot-dvar object slotd)))

(defmacro slot-dlet (bindings &body body)
  `(dlet ,(loop for ((object slot-name) val) in bindings
                 collect `((slot-dvar* ,object ,slot-name) ,val))
     ,@body))

(defclass dynamic-class (standard-class) ())

;;; Class with dynamic slots may be subclasses of the standard class.
(defmethod sb-mop:validate-superclass ((class standard-class)
                                    (super dynamic-class))
  t)

(defmethod sb-mop:validate-superclass ((class dynamic-class)
                                    (super standard-class))
  t)

;; When allocating the instance we initialize all slots to a fresh symbol that
;; represents the dynamic variable.
(defmethod allocate-instance ((class dynamic-class) &rest initargs)
  (declare (ignore initargs))
  (let ((object (call-next-method)))
    (loop for slotd in (sb-mop:class-slots class)
          when (typep slotd 'dynamic-effective-slot) do
            (setf (sb-mop:standard-instance-access
                   object
                   (sb-mop:slot-definition-location slotd))
                  (gensym (string (sb-mop:slot-definition-name slotd)))))
    object))

;; To improve potential composability of CLASS-WITH-DYNAMIC-SLOTS with other
;; metaclasses we treat specially only slots that has :DYNAMIC in initargs,
;; otherwise we call the next method.
(defmethod sb-mop:direct-slot-definition-class ((class dynamic-class) &rest initargs)
  (loop for (key val) on initargs by #'cddr
        when (eq key :dynamic)
          do (return-from sb-mop:direct-slot-definition-class
               (find-class 'dynamic-direct-slot)))
  (call-next-method))

;;; The metaobject protocol did not specify an elegant way to communicate
;;; between the direct slot definition and the effective slot definition.
;;; Luckily we have dynamic bindings! :-)
(defvar *dynamic-slot-p* nil)
(defmethod sb-mop:compute-effective-slot-definition ((class dynamic-class) name direct-slotds)
  (if (typep (first direct-slotds) 'dynamic-direct-slot)
      (let* ((*dynamic-slot-p* t))
        (call-next-method))
      (call-next-method)))

(defmethod sb-mop:effective-slot-definition-class
    ((class dynamic-class) &rest initargs)
  (declare (ignore initargs))
  (if *dynamic-slot-p*
      (find-class 'dynamic-effective-slot)
      (call-next-method)))

;; There is a considerable boilerplate involving customizing slots.
;;
;; - direct slot definition: local to a single defclass form
;;
;; - effective slot definition: combination of all direct slots with the same
;;   name in the class and its superclasses
;;
(defclass dynamic-direct-slot (sb-mop:standard-direct-slot-definition)
  ((dynamic :initform nil :initarg :dynamic :reader dynamic-slot-p)))

;; DYNAMIC-EFFECTIVE-SLOT is implemented to return as slot-value values of the
;; dynamic variable that is stored with the instance.
;;
;; It would be nice if we could specify :ALLOCATION :DYNAMIC for the slot, but
;; then STANDARD-INSTANCE-ACCESS would go belly up. We could make a clever
;; workaround, but who cares?
(defclass dynamic-effective-slot (sb-mop:standard-effective-slot-definition)
  ())

(defmethod sb-mop:slot-value-using-class
    ((class dynamic-class)
     object
     (slotd dynamic-effective-slot))
  (dref (slot-dvar object slotd)))

(defmethod (setf sb-mop:slot-value-using-class) (new-value (class dynamic-class) object (slotd dynamic-effective-slot))
  (dset (slot-dvar object slotd) new-value))

(defmethod sb-mop:slot-boundp-using-class ((class dynamic-class) object
                                           (slotd dynamic-effective-slot))
  (boundp (slot-dvar object slotd)))

(defmethod sb-mop:slot-makunbound-using-class
  ((class dynamic-class)
   object
   (slotd dynamic-effective-slot))
  (makunbound (slot-dvar object slotd)))
