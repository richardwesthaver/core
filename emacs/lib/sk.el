;;; sk.el --- skel Emacs Mode -*- lexical-binding: t; -*-

;; skel-mode, skt-mode, sk-classes

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

;;; Code:

(eval-and-compile (require 'eieio)
		  (require 'cl-lib)
		  (require 'sxp (expand-file-name "sxp.el" (join-paths user-emacs-directory "lib/")))
                  (require 'skeleton)
                  (require 'tempo)
		  (defvar skel-debug nil)
		  (when skel-debug (require 'ede)))

(defconst skel-version "0.1.0")

(defgroup skel nil
  "skel customization group.")

(defcustom skel-keymap-prefix "C-c C-."
  "Prefix for `skel-mode' keymap."
  :type 'string
  :group 'skel)

(defcustom skel-triggers nil
  "Association of symbols to a specific condition which can be used
to trigger `skel-actions' based on the `skel-behavior' value."
  :type 'cons
  :group 'skel)

(defcustom skel-actions nil
  "Array of 'actions' which may be performed on skeletons."
  :type 'obarray
  :group 'skel)

(defcustom skel-id-prefix "sk"
  "Default prefix for `make-id'."
  :type 'string
  :group 'skel)

(define-minor-mode skel-minor-mode
  "skel-minor-mode."
  :global t
  :lighter " sk"
  :group 'skel
  :version skel-version)

;; TODO 2023-09-06: 
(define-derived-mode skel-mode lisp-data-mode "SKEL"
  "skel-mode")

(defun maybe-skel-minor-mode ()
  "Check the current environment and determine if `skel-minor-mode' should
be enabled. This function is added as a hook to
`lisp-data-mode-hook'.")

(defvar skel-hashtable (make-hash-table :test #'equal)
  "Internal table of available skeletons.")

(defvar skel-stack nil "Internal stack of skeletons.")

(defcustom skel-state 'passive
  "State toggle for the `skel' system. Base states are 'passive' and
'active'."
  :type 'symbol
  :group 'skel)

(defvar skel-active-map nil
  "List of cons cells of the form '(SYM . BODY...)' where SYM is a member of `skel-triggers'.")

(defvar skel-passive-map nil
  "list of cons cells of the form '(SYM . BODY...)' where SYM is a member of `skel-triggers'.")

(defmacro make-id (&optional pre)
  `(let ((pre ,(if-let (pre) (concat skel-id-prefix "-" pre "-") (concat skel-id-prefix "-")))
	 (current-time-list nil))
     (symb pre (prog1 gensym-counter (setq gensym-counter (1+ gensym-counter))) (format "%x" (car (current-time))))))

(defmacro defcmd (name &rest body) `(defun ,name nil (interactive) ,@body))

(defclass sk (sxp)
  ((id :initarg :id :initform (make-id)))
  :documentation "Base class for skeleton objects. Inherits from `sxp'."
  :abstract t)

(defcmd sk-classes (eieio-class-children 'sk))

(defmacro def-sk-class (name doc &optional slots superclasses)
  "Define a new class with superclass of `skel'+SUPERCLASSES, SLOTS,
DOC, and NAME."
  (declare (indent 1))
  `(defclass ,(symb "sk-" name)
     ,(if superclasses `(sk ,@superclasses) '(sk))
     ,(if slots
	  `(,@slots
	    (:id :initarg :id :initform (make-id ,(symbol-name name)) :accessor id))
	`((:id :initarg :id :initform (make-id ,(symbol-name name)) :accessor id)))
     :documentation ,doc))

(def-sk-class target "Target skeleton class.")
(def-sk-class source "Source skeleton class.")
(def-sk-class rule
  "Config skeleton class."
  ((target :initarg :target :initform nil :type (or null sk-target))
   (rules :initarg :source :initform nil :type (or null sk-source))))

(def-sk-class project
  "Project skeleton class."
  ((type :initarg :type :initform nil :accessor sk-project-type :type (or null symbol))
   (rules :initarg :rules :initform nil :accessor sk-project-rules :type list)))

(defun skel-init ()
  "Initialize the skel library."
  (interactive)
  (add-to-list 'auto-mode-alist '("skelfile" . skel-mode))
  (add-to-list 'auto-mode-alist '("\\.sk\\'" . skel-mode)))

;;; Autotype
;; From: https://github.com/xFA25E/skempo/blob/master/skempo.el
(defun modify-lisp-syntax-tables ()
  (modify-syntax-entry ?* "w" (syntax-table))
  (modify-syntax-entry ?- "w" (syntax-table)))

(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook #'modify-lisp-syntax-tables))

(defun skt--tags-variable (mode)
  "Return a tempo tags variable's symbol for MODE."
  (when mode
    (intern (replace-regexp-in-string
             (rx "-mode" eos) "-skt-tags"
             (symbol-name mode)))))

(defun skt--remove-tag-list (tag-list)
  "Remove TAG-LIST from `tempo-local-tags'."
  (setf (alist-get tag-list tempo-local-tags nil t) nil))

(defun skt--insert-mark (marker)
  "Insert a MARKER to `tempo-marks' while keeping it sorted.
Remove duplicate marks from `tempo-marks'.  Set to nil removed
markers.  This function is used as an :override advice to
`tempo-insert-mark', because the original function does not
remove duplicate elements.  Duplicate markers appear when the
buffer gets smaller, markers start pointing to the same location.
We don't want that, because a lot of useless markers can slow
down Emacs."
  (if (not tempo-marks)
      (setq tempo-marks (list marker))
    (let ((markers tempo-marks))
      (cond
       ((< marker (car markers))
        (setq tempo-marks (cons marker tempo-marks)))
       (t
        (while (and (cdr markers) (<= (cadr markers) marker))
          (if (/= (car markers) (cadr markers))
              (setq markers (cdr markers))
            (when (markerp (cadr markers)) (set-marker (cadr markers) nil))
            (setcdr markers (cddr markers))))

        (if (= marker (car markers))
            (when (markerp marker) (set-marker marker nil))
          (setcdr markers (cons marker (cdr markers))))))

      (while (cdr markers)
        (if (/= (car markers) (cadr markers))
            (setq markers (cdr markers))
          (when (markerp (cadr markers)) (set-marker (cadr markers) nil))
          (setcdr markers (cddr markers)))))))

(defun skt--add-tag (tag template &optional tag-list)
  "Add a TEMPLATE TAG to TAG-LIST or to `tempo-tags'.
It is an :override function for `tempo-add-tag'.  The original
function does not update identical tags."
  (interactive "sTag: \nCTemplate: ")
  (let ((tag-list (or tag-list 'tempo-tags)))
    (if-let ((value (assoc tag (symbol-value tag-list))))
        (setcdr value template)
      (set tag-list (cons (cons tag template) (symbol-value tag-list))))
    (tempo-invalidate-collection)))

(defun skt--list-derived-modes (mode)
  "List all derived modes of MODE + MODE itself."
  (let ((modes nil))
    (while mode
      (when-let ((alias (symbol-function mode)))
        (when (symbolp alias)
          (setq mode alias)))
      (push mode modes)
      (setq mode (get mode 'derived-mode-parent))  )
    (nreverse modes)))

;;; Commands

(defvar-keymap skt-mode-map
  :doc "skt-mode keymap."
  :repeat (:enter))

(define-minor-mode skt-mode
  "Minor mode for skt-templates."
  :init-value nil
  :lighter " Skt"
  :keymap skt-mode-map
  (let* ((modes (skt--list-derived-modes major-mode))
         (tag-vars (mapcar #'skt--tags-variable modes))
         (bound-tag-vars (cl-delete-if-not #'boundp tag-vars)))
    (if skt-mode
        (mapc #'tempo-use-tag-list bound-tag-vars)
      (mapc #'skt--remove-tag-list bound-tag-vars))))

(defun skt--define-tempo (function-symbol body &optional docstring)
  "Define a tempo template with BODY.
This will generate a function with FUNCTION-SYMBOL and
DOCSTRING.

The main purpose of this function is to have a better controlled
alternative to `tempo-define-template'."
  (let ((template-symbol (gensym (symbol-name function-symbol))))
    (set template-symbol body)
    (defalias function-symbol
      (lambda (&optional arg)
        (interactive "*P")
        (tempo-insert-template template-symbol (xor tempo-insert-region arg)))
      docstring)))

(defun skt--define-skeleton (function-symbol body &optional docstring)
  "Define a skeleton template with BODY.
This will generate a function with FUNCTION-SYMBOL and
DOCSTRING.

The main purpose of this function is to have a better controlled
alternative to `define-skeleton', especially because it is a
function instead of a macro."
  (defalias function-symbol
    (lambda (&optional str arg)
      (interactive "*P\nP")
      (skeleton-proxy-new body str arg))
    docstring))

(defun skt--define-function (function-symbol function &optional docstring)
  "This will generate an alias to FUNCTION with FUNCTION-SYMBOL.
DOCSTRING is used as a docstring to FUNCTION-SYMBOL."
  (defalias function-symbol function docstring))

(defun skt--mode-name (mode)
  "Get MODE name without a -mode suffix."
  (string-trim-right (symbol-name mode) (rx "-mode" eos)))

(defun skt--function-name (name modes)
  "Generate a name for a skt template function.
NAME and MODES are used to generate unique, but consistent
names."
  (concat "skt-template-"
          (mapconcat (lambda (mode) (concat (skt--mode-name mode) "-"))
                     (sort modes #'string<) "")
          name))

(defun skt--mode-abbrev-table (mode)
  "Get abbrev table for MODE or `global-abbrev-table' if nil."
  (if mode
      (derived-mode-abbrev-table-name mode)
    'global-abbrev-table))

(defun skt--abbrev-table (mode)
  "Get skt abbrev table for MODE."
  (intern (concat "skt-" (symbol-name (skt--mode-abbrev-table mode)))))

(defun skt--abbrev-table-names (table)
  "Return abbrev TABLE names."
  (let ((names nil))
    (mapatoms (lambda (abbrev)
                (when (symbol-value abbrev)
                  (push (symbol-name abbrev) names)))
              (symbol-value table))
    names))

(defun skt--modes (mode)
  "Normalize MODE argument."
  (cond ((consp mode) mode)
        ((null mode) nil)
        ((symbolp mode) (list mode))))

;;;###autoload
(defun skt-define (define-function name modes tag abbrev docstring body)
  "Define a skt template.

DEFINE-FUNCTION is a function that takes a function symbol, BODY
and DOCSTRING as its arguments.  It must define a new function
with that symbol and that docstring.

NAME is a string used in generating a function symbol, TAG and
ABBREV.

MODES is a list of modes for which TAG and ABBREV will be
created.  If it's nil, TAG and ABBREV will be generated
globally.

TAG/ABBREV is a boolean, which indicates whether a tag/abbrev
must be created for this template.

DOCSTRING is a string (or nil) which will be supplied to
DEFINE-FUNCTION.

BODY is an arbitrary argument passed to DEFINE-FUNCTION."
  (let* ((function-symbol (intern (skt--function-name name modes)))
         (modes (or modes '(nil))))
    (funcall define-function function-symbol body docstring)
    (put function-symbol 'no-self-insert t)

    (when tag
      (let ((tag-symbol (gensym (symbol-name function-symbol))))
        (if (eq #'skt--define-tempo define-function)
            (set tag-symbol body)
          (set tag-symbol `((ignore (,function-symbol)))))
        (dolist (mode modes)
          (let ((var (skt--tags-variable mode)))
            (unless (boundp var)
              (set var nil))
            (tempo-add-tag name tag-symbol var)))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (and (or (equal '(nil) modes) (apply #'derived-mode-p modes))
                       skt-mode)
              (skt-mode -1)
              (skt-mode 1))))))

    (when abbrev
      (dolist (mode modes)
        (let ((mode-table (skt--mode-abbrev-table mode))
              (table (skt--abbrev-table mode)))
          (define-abbrev-table mode-table nil)
          (define-abbrev-table table nil :case-fixed t :skt t)
          (define-abbrev (symbol-value table) name "" function-symbol
            :case-fixed t :system t :skt t)

          (let* ((names (skt--abbrev-table-names table))
                 (regexp (concat (regexp-opt names "\\_<\\(") " *")))
            (abbrev-table-put (symbol-value table) :regexp regexp))

          (let ((parents (abbrev-table-get (symbol-value mode-table) :parents)))
            (cl-pushnew (symbol-value table) parents :test #'eq)
            (abbrev-table-put (symbol-value mode-table) :parents parents)))))

    function-symbol))

;;;###autoload
(cl-defmacro skt-define-tempo (name (&key mode tag abbrev docstring) &rest body)
  "Define a tempo template.
This macro defines a new tempo template or updates the old one.
NAME is a symbol.  ARGS is a list of the form ([KEY VALUE]...)
where each KEY can be one of :tag, :abbrev, :docstring or :mode.

If KEY is :tag, VALUE should be a boolean.  If VALUE is non-nil,
then a tempo tag with NAME will be created for this template.

If KEY is :abbrev, VALUE should be a boolean.  If VALUE is
non-nil, then a NAME abbrev will be created for this template.

If KEY is :docstring, VALUE should be a string.  It will be a
docstring of the generated function.

If KEY is :mode, VALUE should be a list of modes or single mode.
If this option is provided, than a tempo tag and an abbrev will
be created for these modes, otherwise they will be global (if
:tag and :abbrev options were provided, of course).

BODY is a sequence of tempo elements that will be passed as a
list directly to `tempo-define-template's second argument.

Example:
\(skt-define-tempo defvar (:mode `emacs-lisp-mode' :tag t :abbrev t
                             :docstring \"defvar template\")
  \"(defvar \" (string-trim-right (buffer-name) (rx \".el\" eos)) \"-\" p n>
  r> \")\")"
  `(skt-define #'skt--define-tempo ,(symbol-name name)
                  ',(skt--modes mode) ,tag ,abbrev ,docstring ',body))

;;;###autoload
(cl-defmacro skt-define-skeleton (name (&key mode tag abbrev docstring) &rest body)
  "Define skeleton template.
See `skt-define-tempo' for explanation of NAME, MODE, TAG,
ABBREV and DOCSTRING.

BODY is a sequence of skeleton elements that will be passed
directly to `define-skeleton'.

Example:
\(skt-define-skeleton defun (:mode (emacs-lisp-mode `lisp-interaction-mode')
                               :tag t :abbrev t
                               :docstring \"defun template\")
  \"(defun \" str \" (\" @ - \")\" \n
  @ _ \")\" \n)"
  `(skt-define #'skt--define-skeleton ,(symbol-name name)
                  ',(skt--modes mode) ,tag ,abbrev ,docstring ',body))

;;;###autoload
(cl-defmacro skt-define-function (name (&key mode tag abbrev docstring) function)
  "Define FUNCTION template.
See `skt-define-tempo' for explanation of NAME, MODE, TAG,
ABBREV and DOCSTRING.

The main purpose of this macro, is to create tempo tags and
abbrevs for existing skeleton templates, such as `sh-case'.

Example:
\(skt-define-function shcase (:tag t :abbrev t :mode `sh-mode') `sh-case')"
  `(skt-define #'skt--define-function ,(symbol-name name)
                  ',(skt--modes mode) ,tag ,abbrev ,docstring ',function))

(defun skt--complete-template (string tag-list)
  "An :override advice function for `tempo-display-completions'.
Show completion for STRING in a TAG-LIST.  After selection
expand template.

Rewritten because the original function uses an old way of
displaying completions in a separate buffer, which is not
clickable anyway.  Now it uses new (compared to the originial
tempo package) and shiny `completing-read' interface."
  (let* ((tags (mapcar #'car tag-list))
         (tag (completing-read "Skt: " tags nil t string)))
    (delete-char (- (length string)))
    (tempo-insert-template (cdr (assoc tag tag-list)) nil)))

;;;###autoload
(defcustom skt-enable-tempo-elements nil
  "Enable extra tempo elements.
These elements add conditionals and looping support for tempo
like those in skeleton, making skeleton pretty much obsolete.

If you want to set this option from ELisp, you have to remove
`skt-tempo-user-elements' from `tempo-user-elements' on nil
and add it on non-nil."
  :type '(boolean :tag "Enable tempo elements?")
  :set (lambda (variable value)
         (if value
             (add-hook 'tempo-user-elements #'skt-tempo-user-elements)
           (remove-hook 'tempo-user-elements #'skt-tempo-user-elements))
         (set-default variable value))
  :group 'skel)

(defcustom skt-completing-read nil
  "Override default `tempo-display-completions'.
By default it uses a completion buffer to show completions.  This
option overrides this function to use `completing-read' to select
partial skempo tag or complete tag on region.

If you wish to set this variable from ELisp code, you have to
remove `skt--complete-template' advice from
`tempo-display-completions' on nil and add it as on :override
advice on non-nil."
  :type '(boolean :tag "Override?")
  :set (lambda (variable value)
         (if value
             (advice-add 'tempo-display-completions :override #'skt--complete-template)
           (advice-remove 'tempo-display-completions #'skt--complete-template))
         (set-default variable value))
  :group 'skel)

(defcustom skempo-delete-duplicate-marks nil
  "Override default `tempo-insert-mark'.
Marks are used to jump on points of interest in a template.  By
default `tempo-insert-mark' does not remove duplicate marks.
Duplicate marks might appear when the buffer shrinks and some of
the marks start pointing to the same location.  This option tries
to fix this by checking for duplicate marks every time the
function is called.  Emacs might get slower with a lot of
marks.

If you want to set this option from ELisp, you have to remove
`skt--insert-mark' advice from `tempo-insert-mark' on nil and
add it as on :override advice on non-nil."
  :type '(boolean :tag "Override?")
  :set (lambda (variable value)
         (if value
             (advice-add 'tempo-insert-mark :override #'skt--insert-mark)
           (advice-remove 'tempo-insert-mark #'skt--insert-mark))
         (set-default variable value))
  :group 'skel)

(progn
  (put 'skt-define-tempo 'lisp-indent-function 2)
  (put 'skt-define-skeleton 'lisp-indent-function 2)
  (put 'skt-define-function 'lisp-indent-function 2))

;;; Tempo Elements
(defvar skt-tempo-else-key (kbd "C-M-g")
  "Key used to execute else branch in tempo conditional.")

(defun skt-tempo--prompt (prompt)
  "Make prompt for tempo conditional.
PROMPT is preceded with `skt-tempo-else-key'."
  (concat "(" (key-description skt-tempo-else-key) " to quit) " prompt))

(defun skt-tempo-user-elements (element)
  "Support for conditional and looping tempo elements.
The following forms are supported for ELEMENT:

\(:if (PROMPT VAR) THEN ELSE)

\(:when (PROMPT VAR) BODY...)

\(:while (PROMPT VAR) BODY...)

PROMPT is a string used to read value for VAR.  VAR is a tempo
variable symbol.  Its value can be read with s, as usual.  BODY,
THEN and ELSE are tempo elements.  To abort the execution of
these elements, user must press `skt-tempo-else-key'.

The main purpose of this extension is to mimic skeleton
conditionals and iterative templats.  Skeleton becomes pretty
much obsolete with this extension."
  (pcase element
    (`(:if (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) ,then ,else)
     (let ((prompt (skt-tempo--prompt prompt))
           (map (make-sparse-keymap)))
       (set-keymap-parent map minibuffer-local-map)
       (define-key map skt-tempo-else-key
         (lambda () (interactive) (throw 'else else)))
       (catch 'else
         (tempo-save-named var (read-from-minibuffer prompt nil map))
         then)))
    (`(:when (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:if (,prompt ,var) (l ,@body) (l)))
    (`(:while (,(and (pred stringp) prompt) ,(and (pred symbolp) var)) . ,body)
     `(:when (,prompt ,var) ,@body ,element))))

(provide 'skel)
(provide 'sk)
;;; sk.el ends here
