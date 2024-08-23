;;; ini.lisp --- INI Format

;; https://en.wikipedia.org/wiki/INI_file

;;; Code:
(in-package :dat/ini)

(defun ini-write (value &optional stream))
(defun ini-encode (value &optional stream))

(defun ini-read (stream))
(defun ini-decode (string &key (start 0) end))
