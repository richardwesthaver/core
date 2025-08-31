;;; lisp/core.lisp --- CC Core Lisp

;; Top-level namespaces accessible to Core lisps.

;;; Code:
(pkg:defpkg :core 
  (:use :cl)
  (:use-reexport :std :log :io :obj :net :cry :parse :dat :cl-user :sb-ext :sb-debug))
(in-package :core)
(pushnew :core *features*)

;; (defun exclamation-mark-reader (stream inchar)
;;   (declare (ignore inchar))
;;   (read stream))

;; (defun question-mark-reader (stream inchar)
;;   "Reader function for the #\? macro character in the :CORE readtable. Processes
;; a query from input STREAM."
;;   (declare (ignore inchar)))

(defreadtable :core
  (:merge :modern :std :shell :graph :tensor))

(define-lisp-package :core)

(pkg:defpkg :core/user 
  (:nicknames :user)
  (:use :cl :cl-user :std :std-user :core)
  (:import-from :tree-sitter :load-tree-sitter :load-tree-sitter-alien :load-tree-sitter-c)
  (:import-from :cli/tools/sbcl :with-sbcl))
(in-package :user)

(defpkg lib/prelude)
(defpkg ffi/prelude)
(defpkg prelude)

(eval-when (:compile-toplevel)
  (setq *default-package* "USER"))

(eval-when (:load-toplevel)
  (pushnew :user *features*))
