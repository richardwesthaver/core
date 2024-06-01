;;; skel/comp/dir-locals.lisp --- Support for Emacs dir-locals.el

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html

;;; Code:
(in-package :skel/comp/dir-locals)
(defvar *dir-locals-file* ".dir-locals.el")
(deftype dir-local-var-designator () '(or symbol string))
