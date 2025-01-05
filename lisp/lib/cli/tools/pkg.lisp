;;; pkg.lisp --- CLI Tools

;; Convenience functions for working with common CLI programs

;;; Code:
(defpackage :cli/tools/proto
  (:use :cl :std :cli/env :config :ast)
  (:export :define-cli-tool :*cli-tools*))

(defpackage :cli/tools/term
  (:use :cl :std :cli/tools/proto :cli/env :config :toml :ast)
  (:export
   :*term* :*alacritty-config-path*
   :alacritty-config :term-config
   :run-term :with-term
   :term-error :load-alacritty-config))

(defpackage :cli/tools/fs
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   #:fs-error))

(defpackage :cli/tools/tmux
  (:use :cl :std :cli/tools/proto :cli/env :cli/tools/term)
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
   :simple-tmux-error
   :tmux-config))

(defpackage :cli/tools/cc
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*cc*
   :*ld*
   :run-cc
   :run-ld
   :cc-error
   :*nvcc*
   :run-nvcc
   :nvcc-error))

(defpackage :cli/tools/go
  (:nicknames :tools/go)
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*go*
   :run-go
   :go-install
   :go-error))

(defpackage :cli/tools/net
  (:use :cl :std :cli/tools/proto :cli/env :uri :config :ast)
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
   :ytdl-error
   :browser-config
   :chromium-config
   :firefox-config
   :caddy-error
   :*caddy*
   :run-caddy*
   :run-caddy
   :start-caddy))

(defpackage :cli/tools/pacman
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :*pacman* :run-pacman :pacman-error))

(defpackage :cli/tools/mail
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :mail-error :*mail-program* :run-notmuch :run-offlineimap :*notmuch* :*offlineimap*))

(defpackage :cli/tools/systemd
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :*systemctl* :run-systemd :run-systemctl
   :systemd-error
           :systemctl-stop
   :systemctl-start))

(defpackage :cli/tools/rust
  (:nicknames :tools/rust)
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   #:cargo-error
   #:*cargo*
   #:*rustup*
   #:run-cargo
   #:rustup-error
   #:run-rustup
   #:cargo-install))

(defpackage :cli/tools/sbcl
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*sbcl*
   :run-sbcl
   :sbcl-error
   :with-sbcl
   :*sbcl-runtime-options*
   :*sbcl-toplevel-options*))

(in-package :cli/tools/proto)

(defvar *cli-tools* nil)

(defmacro define-cli-tool (name args &body body)
  "Define a new cli tool with a NAME-error condition, a *NAME* variable, and a
run-NAME function.

ARGS and BODY are parsed as the args and body of the run-NAME function."
  (with-gensyms (var err run)
    (let ((%name (string name)))
      (setf 
       var (symbolicate #\* %name #\*)
       err (symbolicate %name "-ERROR")
       run (symbolicate "RUN-" %name))
      `(progn
         (defvar ,var 
           (find-exe ,(etypecase name
                        (string name)
                        (symbol (string-downcase %name)))))
         ,@(when var `((pushnew ,name *cli-tools*)))
         (deferror ,err (simple-error) () (:auto t))
         (defun ,run ,args ,@body)))))

