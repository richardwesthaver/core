;;; multi.lisp --- Multi-entry Lisp Cores

;; Busybox-style Lisp binaries

;;; Commentary:

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
(in-package :cli/multi)

(defmacro define-multi-main ((&key default (exit t) (export t)) &rest mains)
  "Define a MAIN function for the current package which dispatches
  based on the value of '(ARG0)' at runtime to one of the pairs in
  MAINS.

Each element of MAINS is a list of the form (NAME FUNCTION) where NAME
is the filename of the symlink which will be handled by the associated
main FUNCTION.

When you save an executable lisp image with this function you should
arrange for symlinks for each handled value of (ARG0) to be generated
."
  `(cli/clap::defmain (:exit ,exit :export ,export)
       (string-case ((pathname-name (arg0)) :default ,default)
         ,@mains)))

(defun make-symlinks (src &optional directory &rest names)
  "Make a set of symlinks from SRC to NAMES.

If DIRECTORY is non-nil each name in NAMES is considered relative to
it."
  (when directory
    (setf names (mapcar (lambda (n) (merge-pathnames n directory)) names)))
  (dolist (n names)
    (sb-posix:symlink src n)))
