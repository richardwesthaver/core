;;; skel/packy/pkgfile.lisp --- Pkgfile component

;; Readers and Writers for pkgfiles.

;;; Commentary:

;; The pkgfile format is a lispy version of PKGBUILD. The compile op generates
;; a PKGBUILD file and the build op executes it with makepkg.

;; PKGBUILDs are simply bash scripts, which makes them expressive and
;; convenient but difficult to consume programmatically. The skel/comp/shell
;; package leverages tree-sitter to parse them, but is not always accurate. 

;; PKGFILEs may be translated directly to PKGBUILD objects, which can be
;; written to disk, but it is preferred to skip the translation and write the
;; output directly - providing more flexibility in the sort of expressions we
;; can use in the evaluated body.

;;;; Packaging Functions

;; package :: required - install all files into the packaging directory

;; verify :: arbitrary source authentication

;; prepare :: after source extraction, before build

;; build :: compile source files, prep for package

;; check :: run test suite between build and package

;; Package splitting is not supported.

;;;; (Pacman) Scripts

;; pre-install :: before files are extracted

;; post-install :: after files are extracted

;; pre-upgrade :: before files extracted, 2 args (new old)
;; post-upgrade :: after files extracted, 2 args (new old)

;; pre-remove :: before files removed
;; post-remove :: before files removed

;;;; Dependencies

;; For simplicity we support a single REQUIRE slot with the following
;; properties:

;; atoms added to depends array

;; lists are assumed to have a car starting with one of:

;; :make :: makedepends

;; :opt :: optdepends

;; :check :: checkdepends

;;; Code:
(in-package :skel/packy)

(defcomponent pkgfile (lisp-component simple-project)
  (bind arch url require provide src options checksum)
  (:documentation "Package build files.")
  (:keyword :pkg))

(defmethod load-ast ((self pkgfile))
  (let ((ast (ast self)))
    (multiple-value-bind (slots body) (plist-split ast)
      (doplist (k v) slots
        (setf (slot-value self (find-symbol (string-upcase k) :skel/packy)) v))
      (setf (ast self) body)
      self)))

(defmethod deserialize ((from pathname) (format (eql :pkgfile)) &key)
  (load-ast (read-ast (make-instance 'pkgfile) from)))

(defmethod load-project-component ((kind (eql :pkg)) form &key (path (project-root)))
  (let ((*default-pathname-defaults* path))
    (deserialize form :pkgfile)))
  

  
