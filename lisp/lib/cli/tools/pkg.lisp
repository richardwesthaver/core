;;; pkg.lisp --- CLI Tools

;; Convenience functions for working with common CLI programs

;;; Code:
(defpackage :cli/tools/term
  (:use :cl :std :cli/env)
  (:export
   :*terminal* :*alacritty-config-path*
   :run-terminal :with-terminal))

(defpackage :cli/tools/tmux
  (:use :cl :std :cli/env :cli/tools/term)
  (:export
   :*tmux-config-path*
   :run-tmux :spawn-tmux))

(defpackage :cli/tools/cc
  (:use :cl :std :cli/env)
  (:export
   :*cc*
   :run-cc))

(defpackage :cli/tools/pacman
  (:use :cl :std :cli/env)
  (:export))
