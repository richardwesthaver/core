;;; skel/packy/pkgfile.lisp --- Pkgfile spec

;; Readers and Writers for pkgfiles.

;;; Commentary:

;;; Code:
(in-package :skel/packy)

(defclass pkgfile (sk-lisp-component) ()
  (:documentation "Package build files."))

(defmethod load-ast ((self sk-lisp-component))
  (let ((ast (ast self)))
    (multiple-value-bind (slots body) (plist-split ast)
      ;; todo: slots
      (doplist (k v) slots
        (setf (slot-value self (symbolicate k)) v))
      (setf (ast self) body)
      self)))

(defmethod deserialize ((from pathname) (format (eql :pkg)) &key)
  (load-ast (read-ast (make-instance 'pkgfile) from)))
