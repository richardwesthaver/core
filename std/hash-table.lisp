;;; std/hash-table.lisp --- Standard Hash Tables

;;

;;; Code:
(in-package :std/hash-table)

(declaim (inline maphash-keys))
(defun maphash-keys (function table)
  "Like MAPHASH, but calls FUNCTION with each key in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           table))

(declaim (inline maphash-values))
(defun maphash-values (function table)
  "Like MAPHASH, but calls FUNCTION with each value in the hash table TABLE."
  (maphash (lambda (k v)
             (declare (ignore k))
             (funcall function v))
           table))

(defun hash-table-keys (table)
  "Returns a list containing the keys of hash table TABLE."
  (let ((keys nil))
    (maphash-keys (lambda (k)
                    (push k keys))
                  table)
    keys))

(defun hash-table-values (table)
  "Returns a list containing the values of hash table TABLE."
  (let ((values nil))
    (maphash-values (lambda (v)
                      (push v values))
                    table)
    values))

(defun hash-table-alist (table)
  "Returns an association list containing the keys and values of a hash-table."
  (let ((alist))
    (maphash (lambda (k v)
               (push (cons k v) alist))
             table)
    alist))

(defun hash-table-plist (table)
  "Returns a property list contains the keys and values of a hash-table."
  (let ((plist))
    (maphash (lambda (k v)
               (setf plist (list* k v plist)))
             table)
    plist))

(defun alist-hash-table (alist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the association list
ALIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (dolist (cons alist)
      (ensure-gethash (car cons) table (cdr cons)))
    table))

(defun plist-hash-table (plist &rest hash-table-initargs)
  "Returns a hash table containing the keys and values of the property list
PLIST. Hash table is initialized using the HASH-TABLE-INITARGS."
  (let ((table (apply #'make-hash-table hash-table-initargs)))
    (do ((tail plist (cddr tail)))
        ((not tail))
      (ensure-gethash (car tail) table (cadr tail)))
    table))
