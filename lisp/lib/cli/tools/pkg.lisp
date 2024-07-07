;;; pkg.lisp --- CLI Tools

;; Convenience functions for working with common CLI programs

;;; Code:
(defpackage :cli/tools/term
  (:use :cl :std :cli/env)
  (:export
   :*terminal* :*alacritty-config-path*
   :run-terminal :with-terminal
   :terminal-error))

(defpackage :cli/tools/tmux
  (:use :cl :std :cli/env :cli/tools/term)
  (:export
   :*tmux-user-config-path*
   :*tmux-system-config-path*
   :*default-tmux-socket*
   :run-tmux :spawn-tmux
   :tmux-format
   :tmux-format-string
   :*tmux-variables*
   :*tmux-var-table*
   :tmux-error))

(defpackage :cli/tools/cc
  (:use :cl :std :cli/env)
  (:export
   :*cc*
   :*ld*
   :run-cc
   :run-ld
   :cc-error))

(defpackage :cli/tools/pacman
  (:use :cl :std :cli/env)
  (:export :*pacman* :run-pacman :pacman-error))

(defpackage :cli/tools/systemd
  (:use :cl :std :cli/env)
  (:export :*systemctl* :run-systemd :run-systemctl))

(defpackage :cli/tools/cargo
  (:use :cl :std :cli/env)
  (:export))

(defpackage :cli/tools/sbcl
  (:use :cl :std :cli/env)
  (:export
   :*sbcl*
   :run-sbcl
   :sbcl-error))
