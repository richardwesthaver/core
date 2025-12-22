;;; bootstrap.lisp --- Core Bootstrap Script

;; Bootstrap the core from a fresh SBCL installation.

;;; Commentary:

;; This script is intended for use with latest public release of SBCL.

;; from the project root:

;; sbcl --script bin/script/bootstrap.lisp 

;;; Code:
(in-package :cl-user)
(require 'asdf)
