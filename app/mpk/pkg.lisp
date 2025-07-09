;;; pkg.lisp --- MPK Packages

;; 

;;; Commentary:

;; TODO: https://github.com/uways/oggify

;; ref: https://github.com/schismtracker/schismtracker/wiki/ITTECH.TXT

;;; Code:
(defpackage :mpk/mpd
  (:use :cl :std :sb-bsd-sockets :net/core :config :time :id :ast)
  (:nicknames :mpd)
  (:export
   :with-mpc
   :ensure-mpd
   :mpc-ping
   :mpc-kill
   :mpc-status
   :mpc-playing
   :mpc-pause
   :mpc-play
   :mpc-previous
   :mpc-next
   :mpc-crossfade
   :mpc-add
   :mpc-tag-types
   :mpc-url-handlers
   :mpc-seek
   :mpc-seek-id
   :mpc-add-id
   :mpc-move
   :mpc-move-id
   :mpc-swap
   :mpc-swap-id
   :mpc-clear
   :mpc-delete-track
   :mpc-delete-id
   :mpc-save-playlist
   :mpc-load-playlist
   :mpc-rename-playlist
   :mpc-playlist-info
   :mpc-playlist-changes
   :mpc-shuffle
   :mpc-stop
   :mpc-list-playlist
   :mpc-list-playlist-info
   :mpc-add-to-playlist
   :mpc-clear-playlist
   :mpc-delete-from-playlist
   :mpc-move-in-playlist
   :mpc-find-in-current-playlist
   :mpc-search-in-current-playlist
   :mpc-update
   :mpc-rescan
   :mpc-stats
   :mpc-list-all
   :mpc-list-info
   :mpc-list-all-info
   :mpc-find-tracks
   :mpc-search-tracks
   :mpc-list-metadata
   :mpc-count-tracks
   :playlist
   :track
   :file
   :title
   :artist
   :albumartist
   :album
   ;; :date
   :genre
   :composer
   :position-in-playlist
   :mpd-error
   :mpc-artists
   :mpc-albums
   :mpc-songs
   :mpc-uptime
   :mpc-playtime
   :mpc-db-playtime
   :mpc-db-update
   :mpd-config
   :ensure-mpc
   :mpc-connect))

(defpackage :mpk
  (:use :cl :std :log :id :config :ast :cli/tools/net :cli/tools/media :time :dsp)
  (:export
   #:*mpk-directory*
   #:mpk-path
   :mpk-config :load-mpkrc
   #:mpk-ensure-directories
   #:*mpk-user-directory*
   #:*mpk-media-directory*
   #:*mpk-media-sources*
   #:*known-media-types*
   #:*mpk-media-types*
   #:mpk-previous
   #:mpk-shuffle
   #:mpk-stop
   #:mpk-pause
   #:mpk-play
   #:*mpk-data-directory*
   #:*mpk-cache-directory*
   #:mpk-media-collection
   #:*mpk-media-collections*
   #:mpk-user-path
   #:mpk-media-path
   #:mpk-music-path
   #:mpk-data-path
   #:*mpc*
   #:*music-metadata*
   #:mpk-component
   #:mpk-toggle
   #:find-mpk-symbol
   #:*user-mpkrc*
   #:init-mpkrc
   #:mpk-prev
   #:mpk-next
   #:*mpk-db-id-seed*
   #:*mpk-db-meta-directory*
   #:*mpk-db-directory*
   #:mpk-cache-path
   #:mpk-init
   #:get-music-metadata
   #:get-music-metadata*
   #:mpk-db-path
   #:*movies-metadata*
   #:metadata-scan-directory))

(defpackage :mpk/db
  (:use :cl :std :log :rdb :dsp/aud :dsp/gst :mpk :schema :db :id :uuid :config)
  (:import-from :sb-ext :string-to-octets)
  (:export :*mpk-db* :*mpk-db-schema* :mpk-db 
   :mpk-db-init :mpk-db-shutdown
   :mpk-db-info
   :ingest-metadata-sst
   :make-metadata-sst
   :get-metadata*
   :*mpk-db-table*
   :update-music-metadata
   :mpk-db-config))

(defpackage :mpk/metro
  (:use :cl :std :log :mpk :schema :db :id :config :net/codec/osc :midi)
  (:import-from :sb-ext :string-to-octets)
  (:export :*mpk-metro*
   :mpk-metro-init :mpk-metro-shutdown
   :*mpk-metro-table*
   :metro
   :metro-config))

(pkg:defpkg :mpk-user
  (:use :core-lisp)
  (:use-reexport :mpk :mpk/db :mpk/mpd))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk :cli :clap)
  (:export
   #:*mpk-cli*))

(defpackage :mpk/gui
  (:use :cl :std :log :mpk :gui))
