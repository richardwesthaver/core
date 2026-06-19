;;; lib/cli/env.lisp --- Shell Environments

;;

;;; Code:
(in-package :cli/env)

(defvar *default-global-env-var-names* 
  '("LOG_LEVEL" "CORE_HOME" "PACKY_URL" "VC_URL" "INFRA_HOME" "KRYPT_HOME" "SKEL_HOME" "LISP" "ESHELL" "ORGANIZATION" "TERM"))
(defvar *default-local-env-var-names* 
  '("PREFIX" "STASHDIR" "STOREDIR" "BINDIR" "LIBDIR" "DATADIR" "CARGO_TARGET_DIR"))

(defglobal *env-table* (make-hash-table :test 'equal)
    "Global environment variables available in this image.")

(defun get-env (name &optional default)
  (gethash name *env-table* default))

(defun (setf get-env) (new name)
  (setf (gethash name *env-table*) new))

(defun load-env (scope)
  "Load the environment variables specified by SCOPE."
  (dolist (e scope)
    (setf (gethash e *env-table*) (sb-posix:getenv e))))

(defmethod init ((self (eql :env)) &key (scope (append *default-local-env-var-names* 
                                                       *default-global-env-var-names*)))
  (load-env scope))

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
  (let ((name (pathname name)))
    (find name (or programs (program-list))
          :test (lambda (x y)
                  (and (equal (pathname-name x) (pathname-name y))
                       (equal (pathname-type x) (pathname-type y)))))))

(declaim (inline ld-library-path-list))
(defun ld-library-path-list ()
  (let ((var (sb-posix:getenv "LD_LIBRARY_PATH")))
    (let ((lst (loop for i = 0 then (1+ j)
		     as j = (position #\: var :start i)
		     collect (subseq var i j)
		     while j)))
      (unless (null (car lst))
        (mapcar (lambda (x) (car (directory x))) lst)))))

(defun make-env-var (k v)
  (concatenate 'string k "=" v))

(defun concat-env-table (table)
  "Concatenate key val pairs in hash-table TABLE to strings of the form
  'key=val'. Returns a list which can be passed directly to the :ENVIRONMENT
  slot of SB-EXT:RUN-PROGRAM."
  (let ((ret))
    (flet ((%make (k v) (push (make-env-var k v) ret)))
      (maphash #'%make table))
    ret))
