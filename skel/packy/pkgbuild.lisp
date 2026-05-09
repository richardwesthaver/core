;;; lib/skel/comp/pkgbuild.lisp --- Archlinux PKGBUILDs

;; Readers and Writers for PKBUILD files

;;; Commentary:

;; wiki: https://wiki.archlinux.org/title/PKGBUILD
;; man: https://man.archlinux.org/man/PKGBUILD.5
;; ref: https://wiki.archlinux.org/title/Creating_packages

;;; Code:
(in-package :skel/packy/pkgbuild)
(load-aliens :tree-sitter :tree-sitter-bash)

(defparameter *pkgbuild-filename* "PKGBUILD"
  "Default filename of Arch Linux PKGBUILD files.")

(defun parse-pkgbuild-value (l s)
  (destructuring-bind (n1 n2 b1 b2) l
    (cons (keywordicate (string-upcase (subseq s n1 n2)))
          (let ((val (subseq s b1 b2)))
            (unless (or (string= "()" val)
                        (string= "\"\"" val))
              (let ((v (trim (subseq s b1 b2))))
                (if (simple-string-p v)
                    (let ((l (1- (length v))))
                      ;; \"val\" 'val'
                      (if (or (char= #\" (schar v 0) (schar v l))
                              (char= #\' (schar v 0) (schar v l)))
                          (subseq s 1 (1- l))
                          (or (ignore-errors (parse-number v))
                              v)))
                    v)))))))

;; (parse-pkgbuild #p"/usr/share/pacman/PKGBUILD.proto")
;; (convert-ts-tree (syn/ts::parse-file :bash #p"/usr/share/pacman/PKGBUILD.proto"))
(defun parse-pkgbuild (&optional (file *pkgbuild-filename*))
  "Parse FILE as a pkgbuild script using tree-sitter. Returns multiple
values: (VARS FUNCTIONS SRC)"
  (let* ((path (probe-file file))
         (str (read-file path))
         (tree (convert-ts-tree (syn/ts:parse-file :bash path)))
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
                 (push (cons name val) vars)))))
          (when tree (print (caddr tree))))
    (values
     (collecting (mapcar (lambda (x) 
                           (when-let ((y (parse-pkgbuild-value x str)))
                             (destructuring-bind (k . v) y
                               (collect k) (collect v))))
                         vars))
     (mapcar (lambda (x) (parse-pkgbuild-value x str)) fns))))

(defmethod deserialize ((from pathname) (format (eql :pkgbuild)) &key)
  (multiple-value-bind (config fns) (parse-pkgbuild from)
    (apply 'make-instance 'pkgbuild :functions fns config)))

(defclass pkgbuild (ast)
  ((pkgname :initarg :pkgname)
   (pkgver :initarg :pkgver)
   (pkgrel :initarg :pkgrel)
   (pkgdesc :initarg :pkgdesc)
   (arch :initarg :arch)
   (desc :initarg :desc)
   (url :initarg :url)
   (license :initarg :license)
   (validpgpkeys :initarg :validpgpkeys)
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
   (sha512sums :initarg :sha512sums)
   (noextract :initarg :noextract)
   (source :initarg :source)
   (install :initarg :install)
   (functions :initarg :functions)))

(defvar *pkgbuild-slots* 
  (mapcar (lambda (x) (slot-definition-name x))
          (class-direct-slots (find-class 'pkgbuild))))

(defmethod initialize-instance :before ((self pkgbuild) &rest initargs &key &allow-other-keys)
  (setf (ast self)
        (collecting
          (doplist (k v) initargs
            (unless (memq k *pkgbuild-slots*)
              (remove-from-plist initargs k)
              (collect k) (collect v))))))

(defmethod serde ((self pkgbuild) (path pathname))
  "Serialize a pkgbuild SELF to PATH."
  (let* ((vars (remove 'functions *pkgbuild-slots*))
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
