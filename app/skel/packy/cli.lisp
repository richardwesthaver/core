;;; cli.lisp --- Packy CLI Defs

;; 

;;; Code:
(in-package :skel/packy/cli)

(define-command-type (:packy version) () (cli:print-version *cli*))

(defcommand (:packy show) ())

(define-cli "packy"
  :version "0.1.0"
  :description "Universal Package Manager"
  :kernel (with-commands :packy (command 'show)))

(defmain start-packy ()
  (with-cli (*packy-cli*)
    (funcall (kernel *packy-cli*))))
