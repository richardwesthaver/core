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
(defvar *core-path* (directory-namestring #.(or *load-truename* *compile-file-truename* (error "run me as an executable!"))))
(defvar *lisp-path* (merge-pathnames "lisp/" *core-path*))
(defvar *lib-path* (merge-pathnames "lib/" *lisp-path*))
(defvar *std-path* (merge-pathnames "std/" *lisp-path*))
(defvar *ffi-path* (merge-pathnames "ffi/" *lisp-path*))
(defvar *core-stash* (merge-pathnames ".stash/" *core-path*))

(push *core-path* asdf:*central-registry*)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(unless (asdf:find-system :cl-ppcre nil)
  (ql:quickload :cl-ppcre)
  ;; (asdf:load-asd (probe-file #P"ext/cl-ppcre.asd"))
  )

(asdf:load-asd (probe-file (merge-pathnames "std.asd" *std-path*)))
(asdf:load-system :std)
(use-package :std)
(in-readtable :std)

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

(defun compile-std (&optional save)
  (cl:in-package :user)
  (let ((v (getflag "VERSION")))
    (asdf:compile-system :std :force t :version v)
    (asdf:load-system :std :force t :version v)
    (when save (sb-ext:save-lisp-and-die "std" :compression nil))))

(defun compile-prelude (&optional save)
  (compile-std)
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
  (use-package :rdb)
  (asdf:make :prelude)
  (rocksdb:load-rocksdb save)
  (when save (sb-ext:save-lisp-and-die "prelude" :compression 19)))

#-(or sbcl cl) (error "unsupported Lisp compiler")
(setq *print-level* 32
      *print-length* 256)
;; collect args from shell
(defvar *args* (cdr sb-ext:*posix-argv*))
(defvar *flags*
  '((version "0.1.0")
    (help "x --- core build tool

x.lisp [OPT] [CMD] [ARGS...]
OPTS:
--version/v
--help/h
--level/l
--jobs/j
CMDS:
build
run
test
")
    (quicklisp t)
    (prelude t)
    (jobs 4)))

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
                 (#\h "HELP")
                 (#\l "LEVEL")
                 (#\j "JOBS"))
               k)))
    (if (char-equal (aref arg 0) #\-)
        (if (= (length arg) 2) ;; short
            (f (aref arg 1))
            (if (char-equal (aref arg 1) #\-) ;; long
                (f (subseq arg 2))
                (error "invalid flag"))))))

;; (defun parse-arg (arg))

(defun done () (print :OK))

(defun x-parse-args ()
  (tagbody
   0
     (cond
       ((null *args*) nil)
       ((= 1 (length *args*))
        (let ((flag? (parse-flag (car *args*))))
          (cond
            (flag?
             (cond
               ((equalp flag? "help") (princ (getflag flag?)) (sb-ext:exit :code 0))
               ((equalp flag? "version") (princ (getflag flag?)) (sb-ext:exit :code 0))
               ((equalp flag? "level") (setflag flag? t))
               ((equalp flag? "jobs") (setflag flag? (cadr *args*)))
               (t (error "invalid flag") (sb-ext:exit :code 0))))
            (t (error "invalid arg") (sb-ext:exit :code 0))))))
     ok (done)))

(defun save-foreign (name exports &rest args)
  (apply #'sb-ext:save-lisp-and-die name (append `(:executable nil :callable-exports ,exports) args)))

(sb-alien:define-alien-callable compile-prelude sb-alien:void () (compile-prelude))
(sb-alien:define-alien-callable compile-std sb-alien:void () (compile-std))

(defun x-init ()
  (sb-impl::toplevel-init))

(defun x-repl (&optional noprint)
  (sb-impl::toplevel-repl noprint))

(defun x-respawn (&optional noprint)
  (x-init)
  (done))

;; (save-lisp-and-live "x"  #'respawn #'respawn :executable t :save-runtime-options t)
(defun x-save ()
  (save-lisp-tree-shake-and-die "x"
                            :toplevel #'x-respawn
                            ;; :callable-exports '("compile_std" "compile_prelude")
                            :purify t
                            :executable t
                            :save-runtime-options t))

(x-parse-args)
(x-save)
;; (x-repl)
