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
(require 'org)
(require 'ox)
(defvar ulang-links-history nil)
(defvar ulang-files-history nil)
;;;###autoload
(defun ulang-dblock-insert-links (regexp)
  "Create dblock to insert links matching REGEXP."
  (interactive (list (read-regexp "Insert links matching: " nil ulang-links-history)))
  (org-create-dblock (list :name "links"
                           :regexp regexp
                           :id-only nil))
  (org-update-dblock))

(org-dynamic-block-define "links" 'ulang-dblock-insert-links)

(cl-pushnew '("header" .
                "#+TITLE: $1
#+AUTHOR: $2
#+EMAIL: $3
#+DESCRIPTION: $4
#+SUBTITLE: $4
#+OPTIONS: ^:nil toc:nil num:nil html-postamble:nil
#+HTML_HEAD: <link rel=\"stylesheet\" href=\"https://fonts.xz.style/serve/inter.css\"/>
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.compiler.company/css/new.min.css\"/>
#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"https://cdn.compiler.company/css/night.css\"/>
")
            org-export-global-macros)

(cl-pushnew '("opts" . "#+OPTIONS: $1
") 
            org-export-global-macros)

(setq org-link-abbrev-alist
      '(("vc" . "https://vc.compiler.company/%s")
        ("comp" . "https://compiler.company/%s")
	("cdn" . "https://cdn.compiler.company/%s")
        ("packy" . "https://packy.compiler.company/%s")
        ("yt" . "https://youtube.com/watch?v=%s")))

(message "Initialized ULANG.")

(provide 'ulang)
;;; ulang.el ends here
