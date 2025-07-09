;;; pkg.lisp --- CLI Tools

;; Convenience functions for working with common CLI programs

;;; Code:
(in-package :cli/int)

(defparameter *cli-tool-packages* `(,(package-name *package*)))
(setq *defpkg-hook* (lambda (x) (pushnew (package-name x) *cli-tool-packages* :test 'string=)))

(defpkg :cli/tools/proto
  (:use :cl :std :cli/env :config :ast)
  (:export :define-cli-tool :*cli-tools* :cli-tool-config
   :cli-tool-error))

(defpkg :cli/tools/term
  (:use :cl :std :cli/tools/proto :cli/env :config :toml :ast)
  (:export
   :*term* :*alacritty-config-path*
   :alacritty-config :term-config
   :run-term :with-term
   :term-error :load-alacritty-config
   :*scriptreplay*
   :*script*
   :run-script :run-scriptreplay
   :run-fbterm :*fbterm* :fbterm-error))

(defpkg :cli/tools/fs
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   #:fs-error))

(defpkg :cli/tools/tmux
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

(defpkg :cli/tools/cc
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*cc*
   :*ld*
   :run-cc
   :run-ld
   :run-gdb
   :gdb-error
   :*gdb*
   :*lldb*
   :lldb-error
   :run-lldb
   :cc-error
   :*nvcc*
   :run-nvcc
   :nvcc-error))

(defpkg :cli/tools/build
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*make*
   :run-make
   :*cmake*
   :run-cmake
   :*meson*
   :run-meson
   :*ninja*
   :run-ninja))

(defpkg :cli/tools/media
  (:use :cl :std :cli/tools/proto :cli/env :config :ini :ast)
  (:export
   :*ffmpeg*
   :run-ffmpeg
   :ffmpeg-error
   :*mpv*
   :run-mpv
   :mpv-error
   :list-ffmpeg-codecs
   :list-ffmpeg-formats
   :exec-picard
   :load-picard-config
   :picard-config
   :*picard-config-path*
   :*picard-commands*
   :ffmpeg-format
   :ffmpeg-codec
   :ffmpeg-codec-props
   :ffmpeg-format-props
   :do-picard
   :picard-error
   :wireplumber-error
   :mpv-config
   :*mpv-config-path*))

(defpkg :cli/tools/go
  (:nicknames :tools/go)
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*go*
   :run-go
   :go-install
   :go-error))

(defpkg :cli/tools/plot
  (:nicknames :tools/plot)
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :open-gnuplot
   :close-gnuplot
   :*gnuplot-process*
   :gnuplot-send
   :with-gnuplot-stream
   :with-gnuplot-term
   :run-dot :dot-error :*dot*))

(defpkg :cli/tools/net
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
   :run-easyrsa
   :easyrsa-init-pki
   :easyrsa-gen-req
   :easyrsay-build-ca
   :*easy-rsa-directory*
   :*easy-rsa-vars-file*
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
   :ytdl-config
   :browser-config
   :chromium-config
   :firefox-config
   :caddy-error
   :*caddy*
   :run-caddy*
   :run-caddy
   :start-caddy
   :ytdl-list
   :ytdl-user-agent
   :ytdl-extractors
   :ytdl-json
   :run-transmission-remote
   :run-transmission-daemon
   :transmission-remote-error
   :*transmission-remote*
   :transmission-daemon-error
   :*transmission-daemon*))

(defpkg :cli/tools/pacman
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :*pacman* :run-pacman :pacman-error
           :pacman-upgrade))

(defpkg :cli/tools/mail
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :mail-error :*mail-program* :run-notmuch :run-offlineimap :*notmuch* :*offlineimap*
           :notmuch-search
           :notmuch-address
           :notmuch-tag
           :notmuch-count
           :notmuch-show))

(defpkg :cli/tools/sys
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :*systemctl* :run-systemd :run-systemctl
   :systemd-error
           :systemctl-stop
   :systemctl-start
   :systemctl-restart
   :systemctl-status
   :systemctl-json))

(defpkg :cli/tools/rust
  (:nicknames :tools/rust)
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   #:cargo-error
   #:*cargo*
   #:*rustup*
   #:run-cargo
   #:rustup-error
   #:run-rustup
   #:cargo-install
   #:cargo-clean))

(defpkg :cli/tools/sbcl
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*sbcl*
   :run-sbcl
   :sbcl-error
   :with-sbcl
   :*sbcl-runtime-options*
   :*sbcl-toplevel-options*))

(defpkg :cli/tools/virt
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :*buildah* :*podman* :*qemu*
   :run-buildah :run-podman :run-qemu :podman-machine-upgrade
   :start-podman-service :find-qemu-exe :run-qemu-img))

(setq *defpkg-hook* nil)

(in-package :cli/tools/proto)

(defconfig cli-tool-config () ())

(defvar *cli-tools* nil)

(define-condition cli-tool-error (simple-error) ())

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
      `(eval-always
         (defvar ,var 
           (find-exe ,(etypecase name
                        (string name)
                        (symbol (string-downcase %name)))))
         ,@(when var `((pushnew ,name *cli-tools*)))
         (deferror ,err (cli-tool-error) () (:auto t))
         (defun ,run ,args ,@body)))))
