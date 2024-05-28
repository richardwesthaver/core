(in-package :vc)

(defun namestring-or (obj)
  (if (pathnamep obj)
      (namestring obj)
      obj))

(defun rel-pathname (path)
  (pathname (string-left-trim '(#\/) path)))

(defun glob-path-match (glob)
  (lambda (p start end)
    (member (subseq p start end) (directory (rel-pathname glob)) :test 'equal)))
