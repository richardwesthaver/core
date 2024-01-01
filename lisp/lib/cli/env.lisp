;;; lib/cli/env.lisp --- CLI Environment

;;

;;; Code:
(in-package :cli)

(declaim (inline exec-path-list))
(defun exec-path-list ()
  "Return a list of all members of PATH"
  (let ((var (sb-posix:getenv "PATH")))
    (let ((lst (loop for i = 0 then (1+ j)
		     as j = (position #\: var :start i)
                     when (uiop:directory-exists-p (probe-file (subseq var i j)))
		       collect (probe-file (subseq var i j))
		     while j)))
      (unless (null (car lst))
        (mapcar (lambda (x) (car (directory x)))
                lst)))))

(defun program-list ()
  "Return a fresh list of all files in PATH directories."
  (loop for p in (exec-path-list)
        append (uiop:directory-files p)))

(defun find-exe (name &optional programs)
  "Find NAME in list of PROGRAMS, defaulting to the result of #'program-list."
  (find name (or programs (program-list))
        :test #'equalp
        :key #'pathname-name))

(declaim (inline ld-library-path-list))
(defun ld-library-path-list ()
  (let ((var (sb-posix:getenv "LD_LIBRARY_PATH")))
    (let ((lst (loop for i = 0 then (1+ j)
		     as j = (position #\: var :start i)
		     collect (subseq var i j)
		     while j)))
      (unless (null (car lst))
        (mapcar (lambda (x) (car (directory x))) lst)))))
