#!/usr/bin/env -S sbcl --script
;;; core build tool

;; 
#|
x.lisp
|#
(require 'asdf)
;; (require 'sb-posix)
(require 'sb-concurrency)

#-(or sbcl cl) (error "unsupported Lisp compiler")
(in-package :cl-user)
#-quicklisp
(let ((quicklisp-init "/usr/local/share/lisp/quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(unless (asdf:find-system :cl-ppcre nil)
  (ql:quickload :cl-ppcre)
  ;; (asdf:load-asd (probe-file #P"ext/cl-ppcre.asd"))
  )

(asdf:load-asd (probe-file (merge-pathnames "std.asd" "lisp/std/")))
(asdf:load-system :std)

(defpackage :x
  (:use :cl :std :std/named-readtables)
  (:export :*core-path* :*lisp-path* :*lib-path* :*std-path* :*ffi-path* :*stash-path* :*web-path* :*bin-path*
           :*compression-level*))

(in-package :x)
(require 'sb-rotate-byte)
(require 'sb-introspect)
(require 'sb-grovel)
(require 'sb-cltl2)
(require 'sb-cover)
(require 'sb-sprof)
(use-package :sb-gray)
;; (require 'sb-aclrepl)
(sb-ext:enable-debugger)
(defvar *core-path* (directory-namestring #.(or *load-truename* *compile-file-truename* (error "run me as an executable!"))))

(defvar *lisp-path* (merge-pathnames "lisp/" *core-path*))
(defvar *bin-path* (merge-pathnames "bin/" *lisp-path*))
(defvar *web-path* (merge-pathnames "web/" *lisp-path*))
(defvar *lib-path* (merge-pathnames "lib/" *lisp-path*))
(defvar *std-path* (merge-pathnames "std/" *lisp-path*))
(defvar *ffi-path* (merge-pathnames "ffi/" *lisp-path*))
(defvar *stash-path* (merge-pathnames ".stash/" *core-path*))

(defvar *compression-level* nil)

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
  (uiop:dump-image (merge-pathnames (car (last (std::ssplit #\/ (asdf:component-name c)))) *stash-path*) :executable t :compression *compression-level*))

(defun compile-std (&optional force save)
  (ql:quickload :std)
  (when save
    (in-package :std-user)
    (sb-ext:save-lisp-and-die (merge-pathnames "std.core" *stash-path*) :compression *compression-level*)))

(defun compile-prelude (&optional force save)
  ;; (compile-std)
  (asdf:compile-system :prelude :force force)
  (asdf:load-system :prelude :force force)
  ;; (rocksdb:load-rocksdb save)
  (when save
    (in-package :std-user)
    (use-package :cl-user)
    (sb-ext:save-lisp-and-die (merge-pathnames "prelude.core" *stash-path*) :compression *compression-level*)))

(defun compile-user (&optional force save)
  (asdf:compile-system :user :force force)
  (asdf:load-system :user :force force)
  (when save
    (in-package :user)
    (use-package :cl-user)
    (sb-ext:save-lisp-and-die (merge-pathnames "user.core" *stash-path*) :compression *compression-level*)))

(defun compile-tests (&optional force save)
  (asdf:compile-system :core/tests :force force)
  (asdf:load-system :core/tests :force force)
  (when save
    (in-package :tests)
    (sb-ext:save-lisp-and-die (merge-pathnames "tests.core" *stash-path*) :compression *compression-level*)))

(defun compile-core (&optional force save)
  (asdf:compile-system :core :force force)
  (asdf:load-system :core :force force)
  (when save
    (in-package :core)
    (sb-ext:save-lisp-and-die (merge-pathnames "core.core" *stash-path*) :compression *compression-level*)))

(defun save-foreign (name exports &rest args)
  (apply #'sb-ext:save-lisp-and-die name (append `(:executable nil :callable-exports ,exports) args)))

(sb-alien:define-alien-callable compile-prelude sb-alien:void () (compile-prelude))
(sb-alien:define-alien-callable compile-std sb-alien:void () (compile-std))
(sb-alien:define-alien-callable compile-user sb-alien:void () (compile-user))

(defvar *thunk* nil)

(setq *print-level* 32
      *print-length* 64)
;; collect args from shell
(defvar *args* (cdr sb-ext:*posix-argv*))
(defvar *flags*
  '((version "0.1.0")
    (help "x --- core build tool
x.lisp [CMD]
CMDS:
test
compile
build
test
run
save
install")))

(defun getflag (k)
  (cadar
   (member
    (string-upcase k)
    *flags*
    :test #'string=
    :key #'car)))

(defun bail (msg)
  (log::fatal! msg))

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
                (bail "invalid flag"))))))

;; (defun parse-arg (arg))
(defun x-compile (args)
  (if args
      (let ((name (car args)))
        (ql:quickload name)
        (asdf:compile-system name :force t))
      (compile-prelude t nil)))

(defun %build (name)
  (format t "saving ~A to: ~A~%" name (merge-pathnames name *stash-path*))
  (let ((sys (sb-int:keywordicate (format nil "BIN/~A" (string-upcase name)))))
    (ql:quickload sys)
    (push :ssl *features*)
    ;; (std/sys:forget-shared-objects)
    (asdf:make sys)))

(defun x-build (args)
  (if args
      (let ((name (car args)))
        (ensure-directories-exist *stash-path*)
        (%build name))
      (std:wait-for-threads (mapcar
                             (lambda (x)
                               (sb-thread:make-thread
                                (lambda ()
                                  (sb-ext:run-program "x" (list "build" x) :wait t :output t))
                                :name x))
                             (list "skel" "rdb" "organ" "homer" "packy")))))

(defun x-save (args)
  (if args
      (let ((name (car args)))
        (ensure-directories-exist *stash-path*)
        (format t "saving core to: ~A~%" (merge-pathnames name *stash-path*))
        (string-case (name)
          ("prelude" (compile-prelude t t))
          ("core" (compile-core t t))
          ("std" (compile-std t t))
          ("user" (compile-user t t))
          ("tests" (compile-tests t t))))
      ;; self save
      (sb-ext:run-program "x.lisp" nil :input t :output t)))

(asdf:load-asd (probe-file (merge-pathnames "log.asd" "lisp/lib/log/")))
(asdf:load-asd (probe-file (merge-pathnames "rt.asd" "lisp/lib/rt/")))
(asdf:load-system :log)
(asdf:load-system :rt)
(ql:quickload :rt)

(defun x-test (args)
  (if args
      (let ((name (car args)))
        (ql:quickload :rt)
        (ql:quickload (string-upcase (format nil "~A/tests" name)))
        (rt:do-tests (string-upcase name) t))
      (bail "missing arg")))

(defun x-run (args)
  (if args
      (let* ((name (car args))
             (path (merge-pathnames name *stash-path*)))
        (unless (probe-file path)
          (sb-ext:run-program "x" (list "build" name) :wait t :output t))
        (sb-ext:run-program path (cdr args) :output t))
      (bail "missing arg")))

(defun %install (name)
  (let ((path (merge-pathnames name *stash-path*)))
    (unless (probe-file path)
      (sb-ext:run-program "x" (list "build" name) :wait t :output t))
    (sb-ext:run-program "/bin/sudo"
                        (list "install" "-C" "-m" "755" (namestring path) "/usr/local/bin/")
                        :input t
                        :wait t
                        :output t)
    (format t "installed ~A to ~A~%" name (merge-pathnames name "/usr/local/bin/"))))

(defun x-install (args)
  (mapc #'%install
        (or args
            (list "skel" "rdb" "organ" "homer" "packy"))))

(defun x-parse-args ()
  (if (null *args*)
      (progn
        (println "Welcome to CORE/X")
        (use-package :cl-user)
        (use-package :sb-ext)
        (use-package :std-user)
        (sb-impl::toplevel-repl nil))
      (let ((cmd (pop *args*)))
        (cond
          ((equal cmd "compile") (setq *thunk* #'x-compile))
          ((equal cmd "build") (setq *thunk* #'x-build))
          ((equal cmd "run") (setq *thunk* #'x-run))
          ((equal cmd "test") (setq *thunk* #'x-test))
          ((equal cmd "save") (setq *thunk* #'x-save))
          ((equal cmd "install") (setq *thunk* #'x-install))
          (t (princ (getflag (parse-flag cmd))) (terpri) (sb-ext:exit :code 0))))))

(defun x-init ()
  (in-package :x)
  (let ((*args* (cdr sb-ext:*posix-argv*))
        (*log-level* :info))
    (x-parse-args)
    (log:debug! "running command" *thunk* *args*)
    (funcall *thunk* *args*)))

(format t "saving self to ./x~%")
(sb-ext:save-lisp-and-die
 "x"
 :toplevel #'x-init
 ;; :callable-exports '("compile_std" "compile_prelude")
 :purify nil
 :executable t
 :save-runtime-options t)
