;;; main.lisp --- Main Entrypoints Macros

;; DEFMAIN/DEFINE-MULTI-MAIN

;;; Commentary:

;;;; Multi-entry Lisp Cores (Busybox-style Lisp binaries)

;; We have quite a few Lisp 'binaries' at this point, each of which
;; are quite bloated Lisp core images with tons of duplication.

;; This setup isn't ideal and while we can compress each individual
;; core, we are much better off if we can just share the same core
;; image and access multiple top-level entrypoints easily.

;; The problem of course is that we want to be able to execute the
;; single core the same as we would the individual bloated
;; binaries. To do this we have two options:

;; - build (non-lisp) trampoline programs which loads the
;;   (non-executable) core as a shared library, and calls
;;   foreign-symbols exposed from lisp.

;; - parse argv[0] and dispatch to the correct top-level
;;   function. Control argv[0] by symlinking to the executable core.

;; This package currently exposes an API for the latter.

;;; Code:
(in-package :cli/main)

;;; Main
(defvar *main*)

(defmacro defmain (name (&key (exit t) (debug t) (package *package*) cli
                              commands readtable printer annotations)
                   &body body)
  "Define a CLI main function in the current package."
  (multiple-value-bind (body decls docs) (parse-body body :documentation t)
    (let ((*no-exit* (not exit))
          (*no-debug* (not debug))
          (*package* (find-package package)))
      `(defun ,name ()
         ,(or docs (format nil "~A toplevel." (package-name *package*)))
         ,@decls
         (in-package ,(package-name *package*))
         ,@(when readtable `((in-readtable ,readtable)))
         ,@(when annotations `((load-annotations ,annotations)))
         ,@(when printer `((in-printer ,printer)))
         ,@(when commands `((load-commands ,commands)))
         ,@(if cli
               `((clap:with-cli ((clap:cli ,cli))
                   (with-cli-handlers ,@body)))
               `((with-cli-handlers ,@body)))))))

;;; Multi-main
(defmacro define-multi-main (name default &rest mains)
  "Define a MAIN function for the current package which dispatches
  based on the value of '(ARG0)' at runtime to one of the pairs in
  MAINS.

Each element of MAINS is a list of the form (NAME FUNCTION) where NAME
is the filename of the symlink which will be handled by the associated
main FUNCTION.

When you save an executable lisp image with this function you should
arrange for symlinks for each handled value of (ARG0) to be generated
."
  `(defun ,name ()
     (let ((*main* (keywordicate (string-upcase (pathname-name (cli/clap::arg0))))))
       (case *main*
         ,@mains
         (t ,default)))))
