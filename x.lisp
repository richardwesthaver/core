#!/usr/local/bin/sbcl --script
;;; core build tool

;; 
#|
x.lisp
|#
(require 'asdf)
(require 'sb-posix)
(require 'sb-cover)
(require 'sb-sprof)
(require 'sb-concurrency)
(require 'sb-rotate-byte)
(require 'sb-introspect)
(require 'sb-grovel)
(require 'sb-cltl2)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(unless (asdf:find-system :cl-ppcre nil)
  (ql:quickload :cl-ppcre)
  ;; (asdf:load-asd (probe-file #P"ext/cl-ppcre.asd"))
  )

(asdf:load-asd (probe-file (merge-pathnames "std.asd" "lisp/std/std.asd")))
(asdf:load-system :std)
(defpackage :x
  (:use :cl :std :std/named-readtables)
  (:export :*core-path* :*lisp-path* :*lib-path* :*std-path* :*ffi-path* :*stash-path* :*app-path* :*bin-path*))

(in-package :x)

(defvar *core-path* (directory-namestring #.(or *load-truename* *compile-file-truename* (error "run me as an executable!"))))

(defvar *lisp-path* (merge-pathnames "lisp/" *core-path*))
(defvar *app-path* (merge-pathnames "app/" *lisp-path*))
(defvar *bin-path* (merge-pathnames "bin/" *app-path*))
(defvar *lib-path* (merge-pathnames "lib/" *lisp-path*))
(defvar *std-path* (merge-pathnames "std/" *lisp-path*))
(defvar *ffi-path* (merge-pathnames "ffi/" *lisp-path*))
(defvar *stash-path* (merge-pathnames ".stash/" *core-path*))

(push *core-path* asdf:*central-registry*)
(push *lisp-path* ql:*local-project-directories*)
(push *lib-path* ql:*local-project-directories*)
(push *bin-path* ql:*local-project-directories*)
(push *ffi-path* ql:*local-project-directories*)
(ql:register-local-projects)

(unless (asdf:find-system :log nil)
  (asdf:load-asd (probe-file (merge-pathnames "log/log.asd" *lib-path*))))

(asdf:load-system :log)
(use-package :log)

(unless (asdf:find-system :rocksdb nil)
  (asdf:load-asd (probe-file (merge-pathnames "rocksdb/rocksdb.asd" *ffi-path*)))
  (asdf:load-system :rocksdb))

(unless (asdf:find-system :cli nil)
  (asdf:load-asd (probe-file (merge-pathnames "cli/cli.asd" *lib-path*))))

(asdf:load-system :cli)
(use-package :cli)

(defun done () (print :OK))

(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (merge-pathnames (car (last (std::ssplit #\/ (asdf:component-name c)))) *stash-path*) :executable t :compression t))

(defun compile-std (&optional force save)
  (asdf:compile-system :std :force force)
  (asdf:load-system :std :force force)
  (when save (sb-ext:save-lisp-and-die (merge-pathnames "std.core" *stash-path*) :compression nil)))

(defun compile-prelude (&optional force save)
  ;; (compile-std)
  (asdf:compile-system :prelude :force force)
  ;; (rocksdb:load-rocksdb save)
  (when save (sb-ext:save-lisp-and-die (merge-pathnames "prelude.core" *stash-path*) :compression 19)))

(defun save-foreign (name exports &rest args)
  (apply #'sb-ext:save-lisp-and-die name (append `(:executable nil :callable-exports ,exports) args)))

(sb-alien:define-alien-callable compile-prelude sb-alien:void () (compile-prelude))
(sb-alien:define-alien-callable compile-std sb-alien:void () (compile-std))

(defvar *thunk* nil)
#-(or sbcl cl) (error "unsupported Lisp compiler")
(setq *print-level* 32
      *print-length* 64)
;; collect args from shell
(defvar *args* (cdr sb-ext:*posix-argv*))
(defvar *flags*
  '((version "0.1.0")
    (help "x --- core build tool
x.lisp [CMD] [OPTS...]
CMDS:
build
run
test
save
OPTS:
--version/v
--help/h
")))

(defun getflag (k)
  (cadar
   (member
    (string-upcase k)
    *flags*
    :test #'string=
    :key #'car)))

(defun setflag (k v)
  (setf
   (getflag k)
   v))

(defun parse-flag (arg)
  (flet ((f (k)
           (if (or (characterp k) (= (length k) 1))
               (case (char-downcase (character k))
                 (#\v "VERSION")
                 (#\h "HELP"))
               k)))
    (if (char-equal (aref arg 0) #\-)
        (if (= (length arg) 2) ;; short
            (f (aref arg 1))
            (if (char-equal (aref arg 1) #\-) ;; long
                (f (subseq arg 2))
                (error "invalid flag"))))))

;; (defun parse-arg (arg))

(defun x-build (&optional args)
  (let ((name (car args)))
    (ensure-directories-exist *stash-path*)
    (info! "saving executable to:" (merge-pathnames name *stash-path*))
    (let ((sys (sb-int:keywordicate (format nil "BIN/~A" (string-upcase name)))))
      (ql:quickload sys)
      (asdf:make sys))))

(defun x-run (&optional args))

(defun x-test (&optional args)
  (if args
      (let ((name (car args)))
        (ql:quickload name)
        (ql:quickload (format nil "~A/TESTS" name))
        (ignore-some-conditions (warning) (asdf:test-system name)))))

(defun x-parse-args ()
  (if (null *args*)
      (progn
        (println "Welcome to CORE/X")
        (in-package :std-user)
        (sb-impl::toplevel-repl nil))
      (let ((cmd (pop *args*)))
        (cond
          ((equal cmd "build") (setq *thunk* #'x-build))
          ((equal cmd "run") (setq *thunk* #'x-run))
          ((equal cmd "test") (setq *thunk* #'x-test))
          ((equal cmd "save") (setq *thunk* #'x-save))
          (t (princ (getflag (parse-flag cmd))) (terpri) (sb-ext:exit :code 0))))))

(defun x-init ()
  (in-package :x)
  (let ((*args* (cdr sb-ext:*posix-argv*)))
    (x-parse-args)
    (log:info! "running command" *thunk* *args*)
    (funcall *thunk* *args*)))

(defun x-save (&optional args)
  (if args
      (let ((name (car args)))
        (info! "saving core to:" (merge-pathnames name *stash-path*))
        (string-case (name)
          ("prelude" (compile-prelude t))
          ("std" (compile-std t))))
        ;; self save
      (progn
        (info! "saving self to ./x")
        (sb-ext:save-lisp-and-die "x"
                                  :toplevel #'x-init
                                  ;; :callable-exports '("compile_std" "compile_prelude")
                                  :purify t
                                  :executable t
                                  :save-runtime-options t))))

(x-save)
;; (x-repl)
