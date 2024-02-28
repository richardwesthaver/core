(require 'asdf)
#-(or sbcl cl) (error "unsupported Lisp compiler")
(in-package :cl-user)

(unless (find-package :std-user)
  (make-package :std-user :use '(:cl) :nicknames '(:dev)))
(in-package :std-user)

(defparameter *core-src-path*
  (make-pathname
   :directory 
   '#.(if *compile-file-truename*
          (butlast (pathname-directory *compile-file-truename*))
          (if *load-truename*
              (butlast (pathname-directory *load-truename*))
              (butlast
               (pathname-directory
                (asdf:system-source-directory :std))
               2)
              ))))

(defparameter *lisp-src* (merge-pathnames "lisp/" *core-src-path*))
(defparameter *rust-src* (merge-pathnames "rust/" *core-src-path*))
(defparameter *nu-src* (merge-pathnames "nu/" *core-src-path*))
(defparameter *emacs-src* (merge-pathnames "emacs/" *core-src-path*))

(asdf:load-asd "std/std.asd")
(asdf:compile-system :std)
(asdf:load-system :std)
