;;; ulang.el --- ulang compliance lib -*- lexical-binding: t; -*-

;; Copyright (C) 2023  

;; Author:  <ellis@zor>
;; Keywords: comm

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
;; (setq org-export-global-macros nil)

;;; Code:
;;;###autoload
(defun ulang-init ()
  (interactive)
  (cl-pushnew '("header" .
                "#+TITLE: $1
#+AUTHOR: $2
#+EMAIL: $3
#+DESCRIPTION: $4
#+OPTIONS: ^:nil toc:nil num:nil html-postamble:nil
#+HTML_HEAD: <link rel=\"stylesheet\" href=\"https://fonts.xz.style/serve/inter.css\"/>
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.compiler.company/css/new.min.css\"/>
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.compiler.company/css/night.css\"/>
")
org-export-global-macros)
  (cl-pushnew '("opts" . "#+OPTIONS: $1
") org-export-global-macros)
  (message "Initialized ULANG."))

(provide 'ulang)
;;; ulang.el ends here
