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
  (:import-from :obj/config
   :defconfig :make-config :find-config)
  (:export
   :*tmux-user-config-path*
   :*tmux-system-config-path*
   :*default-tmux-socket*
   :run-tmux :spawn-tmux
   :tmux-format
   :tmux-format-string
   :*tmux-variables*
   :*tmux-var-table*
   :simple-tmux-error))

(defpackage :cli/tools/cc
  (:use :cl :std :cli/env)
  (:export
   :*cc*
   :*ld*
   :run-cc
   :run-ld
   :cc-error))

(defpackage :cli/tools/nvcc
  (:use :cl :std :cli/env)
  (:export
   :*nvcc*
   :run-nvcc
   :nvcc-error))

(defpackage :cli/tools/net
  (:use :cl :std :cli/env :uri)   
  (:import-from :std/os :with-umask)
  (:export
   :*browser*
   :run-browser
   :browse-url
   :browser-error
   :wg-showconf
   :wg-show
   :wg-set
   :wg-setconf
   :wg-generate-key-files
   :wg-generate-keys
   :wg-public-key
   :wg-private-key
   :run-wg
   :*wg*
   :wg-error
   :ip-addr-add
   :ip-link-up
   :ip-link-add
   :run-ip
   :simple-ip-error
   :*ytdl*
   :run-ytdl
   :ytdl-error))

(defpackage :cli/tools/pacman
  (:use :cl :std :cli/env)
  (:export :*pacman* :run-pacman :pacman-error))

(defpackage :cli/tools/systemd
  (:use :cl :std :cli/env)
  (:export :*systemctl* :run-systemd :run-systemctl
           :systemd-error))

(defpackage :cli/tools/rust
  (:use :cl :std :cli/env)
  (:export
   #:cargo-error
   #:*cargo*
   #:*rustup*
   #:run-cargo
   #:rustup-error
   #:run-rustup))

(defpackage :cli/tools/sbcl
  (:use :cl :std :cli/env)
  (:export
   :*sbcl*
   :run-sbcl
   :sbcl-error
   :with-sbcl
   :*sbcl-runtime-options*
   :*sbcl-toplevel-options*))
