;;; std/util.el --- standard utils  -*- lexical-binding: t -*-

;;; Code:
(require 'cl-lib)

;;; Helpers
(defun group (source n)
  "This is Paul Graham's group utility from 'On Lisp'.

Group a list of arguments SOURCE by any provided grouping amount
N.

For example:
(group '(foo 2 bar 4) 2) ;=> ((foo 2) (bar 4))
(group '(a b c d e f) 3) ;=> ((a b c) (d e f))
"
  (when (zerop n) (error "zero length"))
  (cl-labels ((rec (source acc)
                   (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons
                                    (cl-subseq source 0 n)
                                    acc))
                       (nreverse
                        (cons source acc))))))
    (when source (rec source nil))))

(defun flatten (x)
  "Paul Graham's flatten utility from 'On Lisp'.

Given a tree X, return all the 'leaves' of the tree."
  (cl-labels ((rec (x acc)
                   (cond ((null x) acc)
                         ((atom x) (cons x acc))
                         (t (rec
                             (car x)
                             (rec (cdr x) acc))))))
    (rec x nil)))

(defun mkstr (&rest args)
  "Paul Graham's mkstr utility from 'On Lisp'.

Coerce ARGS into a single string and return it."
  (let* ((s ""))
    (dolist (a args)
      (cond
       ((null a) nil)
       ((sequencep a) (setq s (concat s a)))
       ((numberp a) (setq s(concat s (number-to-string a))))
       ((symbolp a) (setq s(concat s (symbol-name a))))))
    s))

(defun symb (&rest args)
  "Paul Graham's symb utility from 'On Lisp'.

Concat ARGS and return a newly interned symbol."
  (intern (apply #'mkstr args)))

;;; Config
(defun add-to-load-path (&rest paths)
  "Add PATHS to `load-path'."
  (mapc (lambda (x)
          (cond
           ((listp x) (mapc #'add-to-load-path x))
           ('_ (cl-pushnew x load-path))))
        paths))

(defmacro add-packages (&rest pkgs)
  "add list of packages PKGS to `package-selected-packages'"
  `(mapc (lambda (x) (add-to-list 'package-selected-packages x)) ',pkgs))

;;; OS
(defmacro when-sys= (name body)
  "(when (string= (system-name) NAME) BODY)"
  `(when ,(string= (system-name) name) ,body))

(defun darwin-p () (string= system-type "darwin"))
(defun linux-p () (string= system-type "gnu/linux"))

(defun join-paths (root &rest dirs)
  "helper function for joining strings to a path."
  (let ((result root))
    (cl-loop for dir in dirs do
             (setq result (concat (file-name-as-directory result) dir)))
    result))

(defun wc (&optional start end)
  "Return a 3-element list with lines, words and characters in
region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (list (count-lines start end) n (- end start))))

;;; Regexps
(defvar default-line-regexp-alist
  '((empty . "[\s\t]*$")
    (indent . "^[\s\t]+")
    (non-empty . "^.+$")
    (list . "^\\([\s\t#*+]+\\|[0-9]+[^\s]?[).]+\\)")
    (heading . "^[=-]+"))
  "Alist of regexp types used by `default-line-regexp-p'.")

(defun default-line-regexp-p (type &optional n)
  "Test for TYPE on line.
TYPE is the car of a cons cell in
`default-line-regexp-alist'.  It matches a regular
expression.
With optional N, search in the Nth line from point."
  (save-excursion
    (goto-char (point-at-bol))
    (and (not (bobp))
         (or (beginning-of-line n) t)
         (save-match-data
           (looking-at
            (alist-get type default-line-regexp-alist))))))

;;; Time
(defun format-iso-week-number (&optional date)
  "format DATE as ISO week number with week days starting on
    Monday. If DATE is nil use current date."
  (let* ((week (format-time-string "%W" date))
         (prefix (if (= (length week) 1)
                     "w0" "w")))
    (concat prefix week)))

(defun last-day-of-year (&optional date)
  "Return the last day of the year as time."
  (encode-time 0 0 0 31 12 (nth 5 (decode-time
                                   (or date (current-time))))))

(defun last-day-of-month (&optional date)
  "Return the last day of month as time."
  (let* ((now (decode-time (or date (current-time))))
         (month (nth 4 now))
         (year (nth 5 now))
         (last-day-of-month (calendar-last-day-of-month month year)))
    (encode-time 0 0 0 last-day-of-month month year)))

(defun last-day-of-week (&optional date)
  "Return the last day of the week as time."
  (let* ((now (or date (current-time)))
         (datetime (decode-time now))
         (dow (nth 6 datetime)))
    (time-add now (days-to-time (- 7 dow)))))

(defun first-day-of-week (&optional date)
  "Return the first day of the week as time."
  (let* ((now (or date (current-time)))
         (datetime (decode-time now))
         (dow (nth 6 datetime)))
    (time-subtract now (days-to-time dow))))

;;; Server
;;;###autoload
(defun kill-emacs-restart (&optional arg)
  (interactive)
  (kill-emacs arg t))


(provide 'util)
;; util.el ends here
