;;; val.lisp --- Simple Value API

;; GET-VAL and friends

;;; Code:
(in-package :obj/val)

(defun slot-val (instance slot-name)
  (if (and instance
           (slot-boundp instance slot-name))
      (slot-value instance slot-name)))

(deferror value-error (invalid-argument) ()
  (:auto t)
  (:default-initargs :reason "GET-VAL does not handle this type of object. Specialize your own method."))

(defgeneric rem-val (object element &key)
  (:documentation "Remove the value ELEMENT from OBJECT returning T if it was present and NIL otherwise.")
  (:method (object element &rest args &key data-type test &allow-other-keys)
    (macrolet ((.check (&body body)
                 (with-gensyms (l) 
                   `(let ((,l (length object))) 
                      ,@body
                      (= (length object) ,l)))))
      (when object
        (typecase object
          (hash-table (remhash element object))
          (array
           (.check (setf object (apply 'remove element object args))))
           (standard-object (slot-makunbound object element))
           (t
            (if data-type
                (progn
                  (remf args :data-type)
                  (cond 
                    ((equal 'alist data-type)
                     (.check (setf object 
                                   (apply 'remove (assoc element object :test (or test #'equal)) object args))))
                    ((equal 'plist data-type)
                     (remf object element))
                    (t
                     (value-error object))))
                (if (listp object)
                    (.check (setf object (apply 'remove element object args)))
                    (value-error object)))))))))

(defgeneric get-val (object element &key)
  (:documentation "Returns the value in a object based on the supplied element name and possible
type hints.")
  (:method (object element &rest args &key data-type default test key start end from-end &allow-other-keys)
    (when object
      (typecase object
        (hash-table
         (gethash element object default))
        (array
         (if (or test key start end from-end)
             (apply 'find element object args)
             (aref object element)))
        (standard-object
         (slot-val object element))
        (t
         (if data-type
             (cond 
               ((equal 'alist data-type)
                (second (assoc element object :test #'equal)))
               ((equal 'plist data-type)
                (get object element))
               (t
                (value-error object)
             (if (listp object)
                 (apply 'find element object args)
                 (value-error object))))))))))

(defgeneric (setf get-val) (new-value object element &key &allow-other-keys)
  (:documentation "Set the value in a object based on the supplied element name and possible type
hints.")
  (:method (new-value object element &rest args &key default data-type test key start end from-end &allow-other-keys)
    (typecase (or data-type object)
      (hash-table (setf (gethash element object default) new-value))
      (array
       (if (or test key start end from-end)
           (if-let ((n (apply 'position element object args)))
             (setf (aref object n) new-value)
             ;; new element
             (vector-push-extend new-value object))
           (setf (aref object element) new-value)))
      (standard-object (setf (slot-value object element) new-value))
      (t
       (if data-type
           (cond ((equal 'alist data-type)
                  (setf (assoc-value object element) new-value))
                 ((equal 'plist data-type)
                  (setf (getf object element) new-value))
                 (t
                  (value-error object)))
           (if (listp object)
               (apply 'nsubstitute new-value element object args)
               (value-error object)))))))

(defgeneric get-value (elt obj)
  (:method (elt (obj sequence))
    (find elt obj :test 'equal))
  (:method (elt (obj hash-table))
    (gethash elt obj)))

(defgeneric (setf get-value) (new elt obj)
  (:method (new elt (obj sequence))
    (setf (elt obj elt) new))
  (:method (new elt (obj hash-table))
    (setf (gethash elt obj) new)))
