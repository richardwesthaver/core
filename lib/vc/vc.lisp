;;; vc/vc.lisp --- VC API

;; High-level API for working with VC objects.

;;; Code:
(pkg:defpkg :vc
  (:use :cl :std)
  (:use-reexport :vc/proto :vc/hg :vc/git #+cli :vc/cli :vc/util))

(in-package :vc)

(defmethod vc-clone ((self pathname) (remote string) &key)
  (let ((repo (if (or (search "git" remote)
                      (search "codeberg" remote))
                  (make-git-repo self)
                  (make-hg-repo self))))
    (vc-clone repo remote)))

(defmethod vc-clone ((self string) (remote t) &key)
  (vc-clone (pathname self) remote))


#+cli
(cli:load-package-cli *vc-cli*)
