;;; lisp-style.el --- Core Lisp Style -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The Compiler Company

;; Author: Richard Westhaver <richard.westhaver@gmail.com>
;; Keywords: lisp

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

;;; Code:
(require 'slime-cl-indent)

(define-common-lisp-style 
 "core"
 "Core Common Lisp Indentation Style"
 (:inherit "sbcl")
 (:indentation
  (defpkg (as defpackage))
  (make-instance 1)
  (reinitialize-instance 1)
  (ensure-package 1)
  (init 1)
  (defpackage* (as defpackage))
  (blasfunc 2)
  (symbol-call 2)
  (org-parse 2)
  (lety (as let))
  (lety* (as let*))
  (letv (as let))
  (letv* (as let*))
  (deferror (as define-condition))
  (defcondition (as define-condition))
  (plet (as let))
  (acase (as case))
  (atypecase (as typecase))
  (defwarning (as define-condition))
  (make-db (as make-instance))
  (make-schema (as make-instance))
  (make-simple-schema (as make-instance))
  (make-palette (as defpackage))
  (define-package (as defpackage))
  (defkernel (as defclass))
  (defhook (as defmacro))
  (defcommand (as defun))
  (define-cli (as make-instance))
  (walk-directory 1)
  (using-gensyms (as with-gensyms))
  (binding-gensyms (as with-gensyms))
  (if-let* (as if-let))
  (when-let* (as when-let))
  (load-config 1)
  (with-db 1)
  (incf 1)
  (decf 1)
  (make-load-form-saving-slots 1)
  (defconfig (as defclass))
  (defclass* (as defclass))
  (defsclass (as defclass))))

(provide 'lisp-style)
;;; lisp-style.el ends here
