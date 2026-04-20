;;; lib/skel/comp/pkgbuild.lisp --- Archlinux PKGBUILDs

;; Readers and Writers for PKBUILD files

;;; Commentary:

;; wiki: https://wiki.archlinux.org/title/PKGBUILD
;; man: https://man.archlinux.org/man/PKGBUILD.5
;; ref: https://wiki.archlinux.org/title/Creating_packages

;;; Code:
(in-package :skel/packy/pkgbuild)

(defparameter *pkgbuild-filename* "PKGBUILD")

(defun parse-pkgbuild-value (l s)
  (destructuring-bind (n1 n2 b1 b2) l
    (cons (keywordicate (string-upcase (subseq s n1 n2)))
          (trim (subseq s b1 b2)))))

;; (parse-pkgbuild #p"/usr/share/pacman/PKGBUILD.proto")

(defun parse-pkgbuild (&optional (file *pkgbuild-filename*))
  "Parse FILE as a pkgbuild script using tree-sitter. Returns multiple
values: (VARS FUNCTIONS SRC)"
  (let* ((path (probe-file file))
         (str (read-file path))
         (tree (copy-list (convert-ts-tree (syn/ts:parse-file :bash path))))
         vars fns)
    (mapc (lambda (x)
            (case (car x)
              (:function-definition
               (when-let ((body (cadar (member :body (caddr x) :key (lambda (x) (car x)))))
                          (name (cadar (member :name (caddr x) :key (lambda (x) (car x))))))
                 (push (nconc name body) fns)))
              (:variable-assignment
               (when-let ((name (cadar (member :name (caddr x) :key (lambda (x) (and (listp x) (listp (car x)) (caar x))))))
                          (val (cadar (member :value (caddr x) :key (lambda (x) (and (listp x) (listp (car x)) (caar x)))))))
                 (push (nconc name val) vars)))))
          (caddr tree))
    (values
     (flatten (mapcar (lambda (x) (parse-pkgbuild-value x str)) vars))
     (flatten (mapcar (lambda (x) (parse-pkgbuild-value x str)) fns)))))

(defmethod deserialize ((from pathname) (format (eql :pkgbuild)) &key)
  (multiple-value-bind (config fns) (parse-pkgbuild from)
    (apply 'make-instance 'pkgbuild :functions fns config)))

(defclass pkgbuild ()
  ((pkgname :initarg :pkgname)
   (pkgver :initarg :pkgver)
   (pkgrel :initarg :pkgrel)
   (pkgdesc :initarg :pkgdesc)
   (arch :initarg :arch)
   (desc :initarg :desc)
   (url :initarg :url)
   (license :initarg :license)
   (groups :initarg :groups)
   (provides :initarg :provides)
   (options :initarg :options)
   (conflicts :initarg :conflicts)
   (replaces :initarg :replaces)
   (backup :initarg :backup)
   (depends :initarg :depends)
   (makedepends :initarg :makedepends)
   (optdepends :initarg :optdepends)
   (checkdepends :initarg :checkdepends)
   (sha256sums :initarg :sha256sums)
   (noextract :initarg :noextract)
   (source :initarg :source)
   (install :initarg :install)
   (functions :initarg :functions)))

(defmethod serde ((self pkgbuild) (path pathname))
  "Serialize a pkgbuild SELF to PATH."
  (let* ((vars (remove 'functions (mapcar 'slot-definition-name (class-slots (find-class 'pkgbuild)))))
         (vals (slot-values self vars))
         (fns (slot-value self 'functions)))
    (with-open-file (f path :direction :output)
      (loop for n in vars
            for v in vals
            when v
            do (format f "~A=~A~%" (string-downcase n) v))
      (doplist (k v) fns (format f "~A() ~A~%" (string-downcase k) v)))))

;;; Install scripts
;; pre_install, post_install
;; pre_upgrade, post_upgrade
;; pre_remove, post_remove

;;; Meta-packages

;;; Arch Build System (ABS)
;; makepkg, makepkg-template
