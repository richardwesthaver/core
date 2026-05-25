;;; early-init.el --- pre-init configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2021 The Compiler Company, LLC

;; Author: Richard Westhaver <ellis@rwest.io>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Code:
(setopt inhibit-startup-buffer-menu nil
        initial-buffer-choice t
        use-dialog-box t
        use-file-dialog nil
        tool-bar-mode nil
        menu-bar-mode nil
        scroll-bar-mode nil
	explicit-shell-file-name "/usr/bin/bash"
	shell-file-name "/usr/bin/bash"
	native-comp-async-report-warnings-errors nil
	comp-deferred-compilation t
	package-native-compile t)
