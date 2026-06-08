;;; pkg.lisp --- CLI Tools

;; Convenience functions for working with common CLI programs

;;; Code:
(defpkg :cli/tools/proto
  (:use :cl :std :cli/env :config :ast)
  (:export :define-cli-tool :*cli-tools* :cli-tool-config
   :cli-tool-error))

(defpkg :cli/tools/term
  (:nicknames :tools/term)
  (:use :cl :std :cli/tools/proto :cli/env :config :toml :ast)
  (:export
   :*term* :*alacritty-config-path*
   :alacritty-config :term-config
   :run-term :with-term
   :term-error :load-alacritty-config
   :*scriptreplay*
   :*script*
   :run-script :run-scriptreplay
   :run-fbterm :*fbterm* :fbterm-error
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

(defpkg :cli/tools/fs
  (:nicknames :tools/fs)
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   #:fs-error))

(defpkg :cli/tools/cc
  (:nicknames :tools/cc)
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
  (:nicknames :tools/build)
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*make*
   :run-make
   :*cmake*
   :run-cmake
   :*meson*
   :run-meson
   :*ninja*
   :run-ninja
   #:cargo-error
   #:*cargo*
   #:*rustup*
   #:run-cargo
   #:rustup-error
   #:run-rustup
   #:cargo-install
   #:cargo-clean
   #:*go*
   #:run-go
   #:go-install
   #:go-error))

(defpkg :cli/tools/media
  (:use :cl :std :cli/tools/proto :cli/env :config :ini :ast)
  (:export
   :*flamegraph.pl*
   :flamegraph
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
  (:use :cl :std :cli/tools/proto :cli/env :uri :config :ast :ini)
  (:import-from :std/os :with-umask)
  (:export
   :default-network-device
   :net-sys-stat-read
   :net-sys-read
   :network-mtu
   :*net-last-rx* :*net-last-tx*
   :*net-last-time* :*net-time*
   :*net-rx* :*net-tx*
   :net-usage
   :fmt-net-usage
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
  (:use :cl :std :cli/tools/proto :cli/env :config :ast)
  (:export :*pacman* :run-pacman :pacman-error
   :pacman-upgrade :pacman-config :load-pacman-config :makepkg 
   :makepkg-config :load-makepkg-config :makepkg-error))

(defpkg :cli/tools/mail
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :mail-error :*mail-program* :run-notmuch :run-offlineimap :*notmuch* :*offlineimap*
           :notmuch-search
           :notmuch-address
           :notmuch-tag
           :notmuch-count
           :notmuch-show))

(defpkg :cli/tools/sys
  (:use :cl :std :cli/tools/proto :cli/env :ini)
  (:export :*systemctl* :run-systemd :run-systemctl
   :systemd-error :systemctl-stop
   :systemctl-start :systemctl-restart
   :systemctl-status :systemctl-json
   :systemd-units
   :*machinectl* :run-machinectl
   :*homectl* :run-homectl
   :*userdbctl* :run-userdbctl
   :*loginctl* :run-loginctl
   :*networkctl* :run-networkctl
   :*resolvectl* :run-resolvectl
   :*journalctl* :run-journalctl
   :*busctl* :run-busctl
   :*perf* :run-perf :perf-record :perf-inject-jit))

(defpkg :cli/tools/sbcl
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export
   :*sbcl*
   :run-sbcl
   :sbcl-error
   :with-sbcl
   :with-core
   :run-core
   :*sbcl-runtime-options*
   :*sbcl-toplevel-options*))

(defpkg :cli/tools/virt
  (:use :cl :std :cli/tools/proto :cli/env)
  (:export :*buildah* :*podman* :*qemu*
   :run-buildah :run-podman :run-qemu :podman-machine-upgrade
   :start-podman-service :find-qemu-exe :run-xvfb-run :run-archiso 
   :mkarchiso :qemu-system :qemu-img :qemu-img-cmd
   :qemu-img-format :qemu-type-opt :qemu-system-display-type))

(defpkg :cli/tools
  (:nicknames :tools)
  (:use :cl :std)
  (:use-reexport . #.*component-packages*))
