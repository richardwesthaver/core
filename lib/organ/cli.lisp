;;; cli.lisp --- Organ CLI

;; 

;;; Code:
(in-package :organ/cli)

;; (defopt organ-output (when *arg* (trace! (or *arg* "output.organ"))))
(defcommand (:organ describe) (&optional doc)
  (if doc
      ;; TODO typed args
      (describe (org-parse :document (pathname doc)))
      (describe (org-parse :document #P"readme.org"))))

(defcommand (:organ inspect) (&optional doc)
  (if doc
      ;; TODO typed args
      (inspect (org-parse :document (pathname doc)))
      (inspect (org-parse :document #P"readme.org"))))

(defcommand (:organ show) (&optional doc)
  (if doc
      (print (org-parse-lines t (read-file doc)))
      (log:error! "missing file arg")))

(defcommand (:organ parse) (&optional doc)
  (let ((input (or doc #P"readme.org")))
    (describe (org-parse :document input))))

(define-cli "organ"
  :version "0.0.1"
  :description "org-mode toolbox"
  :kernel (with-commands :organ (command 'describe)))
