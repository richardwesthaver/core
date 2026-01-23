;;; cli.lisp --- SYN CLI Tools

;; 

;;; Code:
(in-package :syn/cli)

(defcommand (:syn print) (&rest args)
  (when args
    (let ((f (car args)))
      (if (probe-file f)
	  (print-code (syn/gen/c::read-c-file f))
	  (print-code (syn/gen/c::read-c-string f)))
      (terpri))))

(define-command-type (:syn syntax) (input &optional (prompt "select a language: ") (completions *gen-designators*))
  (declare (ignore input))
  (let ((syn (keywordicate (string-upcase (completing-read prompt completions)))))
    (syn/gen:load-gen syn)
    (setq *package* (syn/gen:gen-package syn))
    syn))

#+todo
(define-cli "gen" :description "Syntax GENerator")
  
