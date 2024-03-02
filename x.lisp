#!/usr/local/bin/sbcl --script
;;; lisp build tool

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
(unless (find-package :std-user)
  (make-package :std-user :use '(cl) :nicknames '(user)))

(in-package :user)
#-(or sbcl cl) (error "unsupported Lisp compiler")
(setq *print-level* 32
      *print-length* 256)
;; collect args from shell
(defvar *args* (cdr sb-ext:*posix-argv*))
(defvar *flags*
  '((version "0.1.0")
    (help "x.lisp [OPTS] [ARGS...]
--version/v
--help/h
--prelude/p
--jobs/j
")
    (quicklisp t)
    (prelude t)
    (jobs 4)))

(defun getflag (k)
  (cadar
   (member 
    (intern (if (or (characterp k) (= (length k) 1))
                (case (char-downcase (character k))
                  (#\v "VERSION")
                  (#\h "HELP")
                  (#\p "PRELUDE")
                  (#\l "LEVEL")
                  (#\j "JOBS"))
                (string-upcase k)))
    *flags*
    :test #'string=
    :key #'car)))

(defun setflag (k v)
  (setf
   (getflag k)
   v))

(defun parse-flag (arg)
  (if (char-equal (aref arg 0) #\-)
      (if (= (length arg) 2) ;; short
          (aref arg 1)
          (if (char-equal (aref arg 1) #\-) ;; long
              (subseq arg 2)
              (error "invalid flag")))))

(cond
  ((null *args*) nil)
  ((= 1 (length *args*))
   (let ((flag? (parse-flag (car *args*))))
     (cond
       (flag? (princ (getflag flag?)) (sb-ext:exit :code 0))))))

(defvar *core-path* (directory-namestring #.(or *load-truename* *compile-file-truename* (error "run me as an executable!"))))
(defvar *lisp-path* (merge-pathnames "lisp/" *core-path*))
(defvar *lib-path* (merge-pathnames "lib/" *lisp-path*))
(defvar *std-path* (merge-pathnames "std/" *lisp-path*))
(defvar *ffi-path* (merge-pathnames "ffi/" *lisp-path*))
(push *core-path* asdf:*central-registry*)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(unless (asdf:find-system :cl-ppcre nil)
  (ql:quickload :cl-ppcre)
  ;; (asdf:load-asd (probe-file #P"ext/cl-ppcre.asd"))
  )
(unless (asdf:find-system :std nil)
  (asdf:load-asd (probe-file (merge-pathnames "std.asd" *std-path*))))

(asdf:load-system :std)
(use-package :std)
(in-readtable :std)
(println (std:list-all-named-readtables))
(println (sb-thread:list-all-threads))

(unless (asdf:find-system :log nil)
  (asdf:load-asd (probe-file (merge-pathnames "log/log.asd" *lib-path*))))
(asdf:load-system :log)
;; (unless (asdf:find-system :cl-readline nil)
;;   (ql:quickload :cl-readline)
;;   ;; (asdf:load-asd (probe-file (merge-pathnames "lib/log/log.asd")))
;;   )
(unless (asdf:find-system :cli nil)
  (asdf:load-asd (probe-file (merge-pathnames "cli/cli.asd" *lib-path*))))
(asdf:load-system :cli)
(use-package :cli)

(unless (asdf:find-system :rocksdb nil)
  (asdf:load-asd (probe-file (merge-pathnames "rocksdb/rocksdb.asd" *ffi-path*))))
(asdf:load-system :rocksdb)
(println *features*)

(defun compile-std ()
  (let ((v (getflag "VERSION")))
    (asdf:load-system :std :force t :version v)
    (asdf:compile-system :std :force t :version v)
  (sb-ext:save-lisp-and-die "std" :compression nil)))

(defun compile-prelude () 
  (push (pathname *lisp-path*) ql:*local-project-directories*)
  (mapc #'ql:quickload
        '(:rocksdb :xkb :btrfs
          :nlp :rdb :organ :packy :skel
          :obj :net :parse :pod :dat
          :rt :syn :xdb :doc :vc))
  (use-package :std)
  (use-package :log)
  (use-package :dat)
  (use-package :net)
  (asdf:make :prelude)
  (println :ok)
  (rocksdb:load-rocksdb t)
  (in-package :user)
  (sb-ext:save-lisp-and-die "prelude" :compression 19))

(sb-alien:define-alien-callable compile-prelude sb-alien:void () (compile-prelude))
(sb-alien:define-alien-callable compile-std sb-alien:void () (compile-std))

(sb-ext:save-lisp-and-die "x"
                          :toplevel #'compile-prelude
                          ;; :callable-exports '("compile_std" "compile_prelude")
                          :executable t
                          :save-runtime-options t
                          :compression 19)
