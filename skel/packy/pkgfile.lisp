;;; skel/packy/pkgfile.lisp --- Pkgfile spec

;; Readers and Writers for pkgfiles.

;;; Commentary:

;;; Code:
(in-package :skel/packy)

(defclass! pkgfile (sk-lisp-component simple-project) 
  (arch url require provide src options checksum bind)
  (:documentation "Package build files."))

(defmethod load-ast ((self pkgfile))
  (let ((ast (ast self)))
    (multiple-value-bind (slots body) (plist-split ast)
      (doplist (k v) slots
        (setf (slot-value self (find-symbol (string-upcase k) :skel/packy)) v))
      (setf (ast self) body)
      self)))

(defmethod deserialize ((from pathname) (format (eql :pkg)) &key)
  (load-ast (read-ast (make-instance 'pkgfile) from)))
