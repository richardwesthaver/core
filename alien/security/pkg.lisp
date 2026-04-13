;;; pkg.lisp --- low-level bindings to Linux security modules

;;; Commentary:

;;; Code:
(defpkg :security
  (:use :cl :std :sb-alien :sys)
  (:export :linux-pam :linux-pam-minor :scmp-ver-major
   :scmp-ver-minor :scmp-ver-micro :with-pam :pam-flags
   :pam-call :pam-result :pam-flag :load-pam))
           
