;;; loader.lisp --- Compile and load the Slime backend.

;; Created 2003, James Bielman <jamesjb@jamesjb.com>

;; This code has been placed in the Public Domain.  All warranties
;; are disclaimed.

;; If you want customize the source- or fasl-directory you can set
;; swank:*slime-source-directory* resp. swank:*slime-fasl-directory*
;; before loading this files.
;; E.g.:
;;
;;   (load "../loader.lisp")
;;   (setq swank::*slime-fasl-directory* "/tmp/fasl/")
;;   (swank:init-swank)

;;; Code:
(in-package :swank)

(defvar *started-from-emacs* nil)

(defvar *swank-source-directory*
  (make-pathname :name nil :type nil
                 :defaults (or (when-let ((sys (find-system :swank))) (path sys))
                               *load-pathname* 
                               *default-pathname-defaults*))
  "The directory where to look for the source.")

(defvar *slime-source-directory*
  (make-pathname :name nil :type nil
                 :defaults (or (when-let ((sys (find-system :core))) 
                                 (merge-pathnames "etc/emacs/slime/" (path sys)))
                               *load-pathname* 
                               *default-pathname-defaults*))
  "The directory where to look for the source.")

(defparameter *sysdep-files*
  '(source-path-parser source-file-cache sbcl gray))

(defparameter *os-features*
  '(:macosx :linux :windows :mswindows :win32 :solaris :darwin :sunos :hpux
    :unix :mezzano))

(defparameter *architecture-features*
  '(:powerpc :ppc :ppc64 :x86 :x86-64 :x86_64 :amd64 :i686 :i586 :i486 :pc386 :iapx386
    :sparc64 :sparc :hppa64 :hppa :arm :armv5l :armv6l :armv7l :arm64 :aarch64
    :pentium3 :pentium4
    :mips :mipsel
    :java-1.4 :java-1.5 :java-1.6 :java-1.7))

(defun lisp-version-string ()
  (format nil "~a~:[~;-no-threads~]"
          (lisp-implementation-version)
          #+sb-thread nil
          #-sb-thread t))

(defun unique-dir-name ()
  "Return a name that can be used as a directory name that is
unique to a Lisp implementation, Lisp implementation version,
operating system, and hardware architecture."
  (flet ((first-of (features)
           (loop for f in features
                 when (find f *features*) return it))
         (maybe-warn (value fstring &rest args)
           (cond (value)
                 (t (apply #'warn fstring args)
                    "unknown"))))
    (let ((os   (maybe-warn (first-of *os-features*)
                            "No os feature found in ~a." *os-features*))
          (arch (maybe-warn (first-of *architecture-features*)
                            "No architecture feature found in ~a."
                            *architecture-features*))
          (version (maybe-warn (lisp-version-string)
                               "Don't know how to get Lisp ~
                                implementation version.")))
      (format nil "~(~@{~a~^-~}~)" "core" version os arch))))

(defun slime-version-string ()
  "Return a string identifying the SLIME version.
Return nil if nothing appropriate is available."
  (with-open-file (s (merge-pathnames "slime.el" *slime-source-directory*)
                     :if-does-not-exist nil)
    (when s
      (loop with prefix = ";; Version: "
            for line = (read-line s nil :eof)
            until (eq line :eof)
            when (string-starts-with line prefix)
              return (subseq line (length prefix))))))

(defun default-fasl-dir () (merge-pathnames "slime/" (user-fasl-cache)))

(defvar *slime-fasl-directory* (default-fasl-dir)
  "The directory where fasl files should be placed.")

(defun binary-pathname (src-pathname binary-dir)
  "Return the pathname where SRC-PATHNAME's binary should be compiled."
  (let ((cfp (compile-file-pathname src-pathname)))
    (merge-pathnames (make-pathname :name (pathname-name cfp)
                                    :type (pathname-type cfp))
                     binary-dir)))

(defun handle-swank-load-error (condition context pathname)
  (fresh-line *error-output*)
  (pprint-logical-block (*error-output* () :per-line-prefix ";; ")
    (format *error-output*
            "~%Error ~A ~A:~%  ~A~%"
            context pathname condition)))

(defun compile-files (files fasl-dir load quiet)
  "Compile each file in FILES if the source is newer than its
corresponding binary, or the file preceding it was recompiled.
If LOAD is true, load the fasl file."
  (let ((needs-recompile nil)
        (state :unknown))
    (dolist (src files)
      (let ((dest (binary-pathname src fasl-dir)))
        (handler-bind
            ((error (lambda (c)
                      (ecase state
                        (:compile (handle-swank-load-error c "compiling" src))
                        (:load    (handle-swank-load-error c "loading" dest))
                        (:unknown (handle-swank-load-error c "???ing" src))))))
          (when (or needs-recompile
                    (not (probe-file dest))
                    (file-newer-p src dest))
            (ensure-directories-exist dest)
            ;; need to recompile SRC, so we'll need to recompile
            ;; everything after this too.
            (setf needs-recompile t
                  state :compile)
            (or (compile-file src :output-file dest :print nil
                                  :verbose (not quiet))
                ;; An implementation may not necessarily signal a
                ;; condition itself when COMPILE-FILE fails (e.g. ECL)
                (error "COMPILE-FILE returned NIL.")))
          (when load
            (setf state :load)
            (load dest :verbose (not quiet))))))))

;; TODO 2026-02-27: replace with xdg/rc
(defun src-files (names src-dir)
  (mapcar (lambda (name)
            (multiple-value-bind (dirs name)
                (etypecase name
                  (symbol (values '() name))
                  (cons (values (butlast name) (car (last name)))))
              (make-pathname
               :directory (append (or (pathname-directory src-dir)
                                      '(:relative))
                                  (mapcar #'string-downcase dirs))
               :name (string-downcase name)
               :type "lisp"
               :defaults src-dir)))
          names))

(defvar *swank-files*
  `(pkg
    backend ,@*sysdep-files* select-match rpc
    swank))

(defvar *contribs*
  '(swank-util swank-repl
    swank-c-p-c swank-arglists swank-fuzzy
    swank-fancy-inspector
    swank-presentations
    swank-package-fu
    swank-hyperdoc
    swank-sbcl-exts
    ;; swank-mrepl
    swank-trace-dialog
    swank-macrostep
    swank-indentation)
  "List of names for contrib modules.")

(defun append-dir (absolute name)
  (merge-pathnames
   (make-pathname :directory `(:relative ,name) :defaults absolute)
   absolute))

(defun contrib-dir (base-dir)
  (append-dir base-dir "ext"))

(defun load-swank (&key (src-dir *swank-source-directory*)
                     (fasl-dir *slime-fasl-directory*)
                        quiet)
  (with-compilation-unit ()
    (compile-files (src-files *swank-files* src-dir) fasl-dir t quiet))
  (funcall (read-from-string "swank::before-init")
           (slime-version-string)
           (list (contrib-dir fasl-dir)
                 (contrib-dir src-dir))))

(defun delete-stale-contrib-fasl-files (swank-files contrib-files fasl-dir)
  (let ((newest (reduce #'max (mapcar #'file-write-date swank-files))))
    (dolist (src contrib-files)
      (let ((fasl (binary-pathname src fasl-dir)))
        (when (and (probe-file fasl)
                   (<= (file-write-date fasl) newest))
          (delete-file fasl))))))

(defun compile-contribs (&key (src-dir (contrib-dir *swank-source-directory*))
                           (fasl-dir (contrib-dir *slime-fasl-directory*))
                           (swank-src-dir *swank-source-directory*)
                           load quiet)
  (let* ((swank-src-files (src-files *swank-files* swank-src-dir))
         (contrib-src-files (src-files *contribs* src-dir)))
    (delete-stale-contrib-fasl-files swank-src-files contrib-src-files
                                     fasl-dir)
    (compile-files contrib-src-files fasl-dir load quiet)))

(defun loadup ()
  (load-swank)
  (compile-contribs :load t))

(defun setup ()
  (when (probe-file (contrib-dir *swank-source-directory*))
    (eval `(pushnew 'compile-contribs ,(read-from-string "swank::*after-init-hook*"))))
  (funcall (read-from-string "swank::start-swank")))

(defun list-swank-packages ()
  (remove-if-not 
   (lambda (package)
     (let ((name (package-name package)))
       (string-starts-with name "swank")))
   (list-all-packages)))

(defun delete-packages (packages)
  (dolist (package packages)
    (flet ((handle-package-error (c)
             (let ((pkgs (set-difference (package-used-by-list package)
                                         packages)))
               (when pkgs
                 (warn "deleting ~a which is used by ~{~a~^, ~}."
                       package pkgs))
               (continue c))))
      (handler-bind ((package-error #'handle-package-error))
        (delete-package package)))))

(defun init-swank (&key delete reload load-contribs (setup t)
                  (quiet (not *load-verbose*))
                  from-emacs)
  "Load SWANK and initialize some global variables.
If DELETE is true, delete any existing SWANK packages.
If RELOAD is true, reload SWANK, even if the SWANK package already exists.
If LOAD-CONTRIBS is true, load all contribs
If SETUP is true, load user init files and initialize some
global variabes in SWANK."
  (when from-emacs
    (setf *started-from-emacs* t))
  (when (and delete (find-package :swank))
    (delete-packages (list-swank-packages)))
  (cond ((or (not (find-package :swank)) reload)
         (load-swank :quiet quiet))
        (t
         (warn "Not reloading SWANK.  Package already exists.")))
  (when load-contribs
    (compile-contribs :load t :quiet quiet))
  (when setup
    (setup)))

(defun list-fasls (&key (include-contribs t) (compile t)
                        (quiet (not *compile-verbose*)))
  "List up SWANK's fasls along with their dependencies."
  (flet ((collect-fasls (files fasl-dir)
           (when compile
             (compile-files files fasl-dir nil quiet))
           (loop for src in files
                 when (probe-file (binary-pathname src fasl-dir))
                   collect it)))
    (append (collect-fasls (src-files *swank-files* *slime-source-directory*)
                           *slime-fasl-directory*)
            (when include-contribs
              (collect-fasls (src-files *contribs*
                                        (contrib-dir *slime-source-directory*))
                             (contrib-dir *slime-fasl-directory*))))))
