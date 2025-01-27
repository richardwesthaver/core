;;; obj/url.lisp --- Universal Resource Locators

;; Some conveniences for URLs.

;;; Commentary:

;; This package mostly just re-exports from QURI.

;;; Code:
(in-package :obj/url)

;;; String Utils
(defun starts-with-scheme-p (string)
  "Check whether the string STRING represents a URL which starts with
a scheme, i.e. something like 'https://' or 'mailto:'."
  (loop with scheme-char-seen-p = nil
        for c across string
        when (or (char-not-greaterp #\a c #\z)
                 (digit-char-p c)
                 (member c '(#\+ #\- #\.) :test #'char=))
        do (setq scheme-char-seen-p t)
        else return (and scheme-char-seen-p
                         (char= c #\:))))
