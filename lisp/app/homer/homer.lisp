;;; homer.lisp --- Homer Top Level

;; 

;;; Code:
(in-package :homer)

#+cli
(progn
  (cli:load-package-cli homer/cli:*homer-cli*)
  (use-package :homer/cli))
