;;; scrum.el --- Scrum-like Planning and Roadmaps in Org  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Richard Westhaver

;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: maint

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

;; ref: https://www.scrum.org/resources/what-scrum-module

;; roadmap: https://compiler.company/plan/roadmap.html

;; tasks: https://compiler.company/plan/tasks

;;; Code:
(require 'ulang)
(require 'uml-mode)
(defvar scrum-properties '("SPRINT" "RELEASE" "TASKID"))

(provide 'scrum)
;;; scrum.el ends here
