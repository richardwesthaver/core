;;; cli.lisp --- Packy CLI Defs

;; 

;;; Code:
(in-package :skel/packy/cli)
(init :commands :name :packy :clean t :copy :cli)
(defcommand (:packy show) (&optional version))

#+todo
(define-cli "packy" (with-commands :packy (command :show))
  :version "0.1.0"
  :description "Universal Package Manager")

#+todo
(defmain start-packy ()
  (with-cli ((cli :packy))
    (funcall (kernel *cli*))))
