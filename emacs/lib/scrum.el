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

;; The point of this package is to enable an Emacs-native scrum
;; workflow. Many years ago I used to use the org-jira package and
;; mirror an external scrum/agile system (Jira).

;; Mind you, I wouldn't dare take a shot at Jira. As far as Products
;; go, when you need to work with hundreds of humans on software and
;; are given a short list you must choose from, it's often the best of
;; the worst.

;; The problem is however, that we don't need Products. What we need,
;; is a plan. How we achieve that end should be via the best and most
;; powerful tools possible.

;; In my opinion, Emacs Org Mode is the most powerful tool
;; available. It is not quite the best tool for the job, but this
;; isn't a problem because it is not a Product. We are given the
;; opportunity to make it the best tool possible, in the only way
;; possible - by doing it ourselves.

;; And yes, the aura of NIH syndrome may be strong here. Most of the
;; time you need to work with lots of folks who don't have the need or
;; patience to learn Org-mode. This package isn't for them. It's for
;; small groups of like-minded Lispers :).

;; ref: https://www.scrum.org/resources/what-scrum-module

;; roadmap: https://compiler.company/plan/roadmap.html

;; tasks: https://compiler.company/plan/tasks

;;; Code:
(require 'ulang)
(require 'uml-mode)

(defgroup scrum nil
  "CC Scrum Framework.")

(defvar scrum-properties '("SPRINT" "EPIC" "RELEASE" "TASKID" "PROJECT" "COMMIT" "GOAL"))

(defvar scrum-tags '("demo" "mvp" "release" "major-release" "ua" "qa"))

(provide 'scrum)
;;; scrum.el ends here
