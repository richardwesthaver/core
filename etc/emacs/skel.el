;;; skel.el --- skel Emacs Mode -*- lexical-binding:t -*-

;; skel-mode, skel-minor-mode, skt-minor-mode

;; Copyright (C) 2023  The Compiler Company

;; Author: ellis <ellis@rwest.io>
;; Keywords: languages, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(eval-and-compile 
  (require 'eieio)
  (require 'cl-lib)
  (require 'skeleton)
  (require 'project)
  (require 'org)
  (require 'tempo)
  (require 'autoinsert)
  (defvar skel-debug nil)
  (when skel-debug (require 'ede)))

(defvar skel-version "0.1.0")

(defgroup skel nil
  "skel customization group."
  :group 'local)

(defcustom skel-map-prefix "C-x M-s"
  "Prefix for `skel' keymap."
  :type 'string
  :group 'skel)

(defcustom skel-triggers nil
  "Association of symbols to a specific condition which can be used
to trigger `skel-actions' based on the `skel-behavior' value."
  :type '(list function)
  :group 'skel)

(defcustom skel-actions nil
  "Array of actions which may be performed on skeletons."
  :type 'obarray
  :group 'skel)

(defcustom skel-id-prefix "sk"
  "Default prefix for `make-id'."
  :type 'string
  :group 'skel)

;;; Commands
;; should dispatch to a server, likely covered by eglot tho..
(defvar-keymap skel-map
  :doc "skel keymap"
  :prefix 'skel-map
  "b" 'skel:build
  "m" 'skel:make
  "c" 'skel:compile
  "u" 'skel:update
  "U" 'skel:unpack
  "P" 'skel:pack
  "d" 'skel:dist
  "x" 'skel:clean
  "r" 'skel:run
  "s" 'skel:show
  "i" 'skel:install
  "v" 'skel:vc
  "V" 'skel:view)

(defmacro def-skel-cmd (name)
  `(defun ,(symb 'skel: name) (&optional arg)
     (interactive "P")
     (when arg (setf arg (read-string (format "skel %s " ',name))))
     (let ((default-directory (project-root (project-current t))))
       (async-shell-command (format "skel %s %s" ',name (princ (or arg "")))))))

(def-skel-cmd build)
(def-skel-cmd dist)
(def-skel-cmd compile)
(def-skel-cmd update)
(def-skel-cmd make)
(def-skel-cmd run)
(def-skel-cmd pack)
(def-skel-cmd install)
(def-skel-cmd unpack)
(def-skel-cmd show)
(def-skel-cmd vc)
(def-skel-cmd search)
(def-skel-cmd view)

;;; Minor Mode
(define-minor-mode skel-minor-mode
  "skel-minor-mode"
  :global t
  :lighter " Sk"
  :group 'skel
  :version skel-version
  (keymap-local-set skel-map-prefix skel-map))

(defun project-try-skel (dir)
  (when (or (file-exists-p (join-paths dir "skelfile"))
	    (directory-files dir nil "^.*[.]sk"))
    (let ((res (project-try-vc--search dir)))
      (when res 
	(vc-file-setprop dir 'project-vc res)
	(setf (car res) 'skel))
      (append res (list dir)))))

(defun skel-indent-region (start end)
  "Indent region as a SKEL S-expression."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (beginning-of-line)
    (let* ((parse-state (lisp-indent-initial-state))
	   (pr (unless (minibufferp)
		 (make-progress-reporter "Indenting region..." (point) end))))
      (let ((ppss (lisp-indent-state-ppss parse-state)))
	(unless (or (and (bolp) (eolp)) (nth 3 ppss))
	  (lisp-indent-line (calculate-lisp-indent ppss))))
      (let ((indent nil))
	(while (progn (setq indent (lisp-indent-calc-next parse-state))
		      (< (point) end))
	  (unless (or (and (bolp) (eolp)) (not indent))
	    (lisp-indent-line indent))
	  (and pr (progress-reporter-update pr (point)))))
      (and pr (progress-reporter-done pr))
      (move-marker end nil))))

;;; Major Mode
;; TODO 2023-09-06: 
(define-derived-mode skel-mode lisp-mode "Skel"
  :group 'skel
  (skel-minor-mode 1)
  (setq-local electric-quote-string t)
  (setq imenu-case-fold-search nil)
  (setq-local indent-region-function 'skel-indent-region)
  (setq-local lisp-indent-offset 1))

;;;###autoload
(defun init-skel ()
  (mapc (lambda (x) (add-to-list 'auto-mode-alist `(,x . skel-mode))) 
        '("\\.box\\'" "\\.pod\\'" "\\.pkg\\'"
          "\\.?\\(skelrc\\|skelfile\\|sk\\|sxp\\|homerc\\|kryptrc\\|packyrc\\)\\'")))

;; TODO 2026-05-30: 
;; (defun maybe-skel-minor-mode ()
;;   "Check the current environment and determine if `skel-minor-mode' should
;; be enabled. This function is added as a hook to
;; `lisp-data-mode-hook'.")

(defmacro make-id (&optional pre)
  `(let ((pre ,(if-let* ((pre)) (concat skel-id-prefix "-" pre "-") (concat skel-id-prefix "-")))
	 (current-time-list nil))
     (symb pre (prog1 gensym-counter (setq gensym-counter (1+ gensym-counter))) (format "%x" (car (current-time))))))

(cl-defmethod project-root ((project (head skel)))
  (when (and project (>= (length project) 4))
    (caddr project)))

(cl-defmethod project-root ((project list))
  (when project (car project)))

(cl-defun project-skelfile (&optional (project t))
  "Find skelfile associated with PROJECT. Defaults to current
directory and returns name of skelfile. When PROJECT is T uses
`project-current'."
  (interactive)
  (let* ((dir (unless (eql t project) (expand-file-name (or project default-directory))))
         (project-root (project-root (project-current nil dir))))
    (or
     (when dir
       (cl-find-if 
        (lambda (x)
          (when (string-match
                 (rx (or "skelfile" (and (* any) ".sk")))
                 (file-name-nondirectory x))
            x))
        (directory-files dir t)))
     (when project
       (cl-find-if (lambda (x)
                     (when (string-match (rx (or "skelfile" (and (* any) ".sk")))
                                         (file-name-nondirectory x))
                       x))
                   (directory-files project-root t))))))

(defun read-skelfile-bind (&optional project)
  "Open PROJECT's skelfile and return the :bind form."
  (let ((buffer (find-file-noselect (project-skelfile project))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (goto-char (search-forward-regexp (rx bol ":bind" (* space))))
      (read buffer))))

(defun project-skelfile-dir-locals (&optional project)
  "Return a list of dir-local bindings from a skelfile."
  (let ((form (read-skelfile-bind project)))
    (cl-loop for f in form
             do (cond
                 ((eql (car f) :dir-locals) (cl-return (cdr f)))
                 ;; when used as second element, the first is the name
                 ;; of the CL-local binding, here we discard it and
                 ;; just take the CDDR.
                 ((eql (cadr f) :dir-locals) (cl-return (cddr f)))))))

(defun skel-dir-local--get-variables ()
  "Compute and return the list of :DIR-LOCAL bindings found in the current
project's skelfile, if any. Typically added to
`hack-dir-local--get-variables'."
  (let ((root (project-root (project-current))))
    (cons (expand-file-name root) (project-skelfile-dir-locals root))))

(defun skel-dir-local-get-variables ()
  "Open the project skelfile and return the :dir-locals bindingings if present."
  (let ((root (expand-file-name (project-root (project-current)))))
    (unless (assoc-string root dir-locals-class-alist)
      (push (skel-dir-local--get-variables) dir-locals-class-alist))))

(defun run-skel-shell ()
  (interactive)
  (comint-run "skel" '("shell")))

;;; UI
;; TODO 2025-10-03: 
;; skel project customization ui (overlays skelfile)

;;; organ-minor-mode
;; support ORGAN reader syntax in lisp files 
(defun organ-minor-mode-setup ()
  (make-local-variable 'post-command-hook)
  (add-hook 'post-command-hook 'organ-update-mode nil t)
  (make-local-variable 'minor-mode-alist)
  (or (assq 'organ-minor-mode minor-mode-alist)
      (setq minor-mode-alist
	        (cons '(organ-minor-mode " organ") minor-mode-alist))))

(defun organ-change-mode (to)
  (if (eql to major-mode)
      t
    (progn
      (if (eql to 'org-mode)
	  (org-mode)
	(lisp-mode))
      (organ-minor-mode-setup))))

;; FIX 2026-05-01: 
(defun organ-update-mode ()
  (let ((lm -1)
        (rm -1)
	(vbar nil))
    (save-excursion 
      (if (or (search-backward "#&" nil t)
	      (and (re-search-backward "#|[ ]?org" nil t) (setf vbar t)))
          (setq lm (point))
        (setq lm -1)))
    (save-excursion
      (if (or (and (not vbar) (search-forward "&#" nil t))
	      (and vbar (search-forward "|#" nil t)))
          (setq rm (point))
        (setq rm -1)))
    (if (or (= lm -1) (= rm -1))
        (organ-change-mode nil)
      (organ-change-mode 'org-mode))))

(define-minor-mode organ-minor-mode nil
  :lighter " organ"
  :after-hook (organ-minor-mode-setup))

(provide 'skel)
;;; skel.el ends here
