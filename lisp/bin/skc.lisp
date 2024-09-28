;;; skc.lisp --- Skel Client

;; 

;;; Code:
(in-package :std-user)
(defpkg :bin/skc
  (:use :cl)
  (:nicknames :skc))
(in-package :bin/skc)

(define-cli *skc-cli*
  :name "skc"
  :version #.(format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream)))))

(defmain start-skc ()
  (with-cli (*skc-cli* opts cmds) (cli:args)
    (do-cmd *skc-cli*)))
