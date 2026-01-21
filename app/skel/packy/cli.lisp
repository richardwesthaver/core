;;; cli.lisp --- Packy CLI Defs

;; 

;;; Code:
(in-package :skel/packy/cli)

(define-command-type (:packy version) () (cli:print-version *cli*))

(defcommand (:packy show) ())

#+todo
(define-cli "packy"
  :version "0.1.0"
  :description "Universal Package Manager")

#+todo
(defmain start-packy ()
  (with-cli ((cli :packy))
    (funcall (kernel *cli*))))
