;;; skc.lisp --- Skel Client

;; 

;;; Code:
(in-package :std-user)
(defpkg :bin/skc
  (:use :cl)
  (:nicknames :skc))
(in-package :bin/skc)

(cli:define-cli *skc-cli*
  :name "skc"
  :version #.(format nil "0.1.1:~A" (read-line (sb-ext:process-output (vc:run-hg-command "id" '("-i") :stream))))
  :thunk 'cli:args)

(cli:defmain start-skc ()
  (cli:with-cli (*skc-cli* :args (cli:args) :run t :exit t)))
