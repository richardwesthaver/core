;; -*- coding: utf-8; lexical-binding: t; -*-
(loaddefs-generate 
 '("/usr/share/emacs/site-lisp/"
   "/usr/share/emacs/site-lisp/slime/")
 "/usr/share/emacs/site-lisp/autoloads.el"
 nil nil nil t)
(copy-file "/usr/share/emacs/site-lisp/autoloads.el" "etc/emacs/" t)
