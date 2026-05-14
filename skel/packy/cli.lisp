;;; cli.lisp --- Packy CLI Defs

;; 

;;; Code:
(in-package :skel/packy/cli)
(init :commands :name :packy :clean t :copy :skel)

(defcommand (:packy show) (&optional version) (declare (ignore version)))

(defmain start-packy (:package :pk-user :readtable :shell :commands :packy :cli :packy)
  (funcall (kernel *cli*)))

(define-cli "packy" #'start-packy
  :version "0.1.0"
  :description "Universal Package Manager")

(save :commands :packy)
