;;; pkg.lisp --- MPK Packages

;; 

;;; Commentary:

;; TODO: https://github.com/uways/oggify

;; ref: https://github.com/schismtracker/schismtracker/wiki/ITTECH.TXT

;;; Code:
(defpackage :mpk/mpd
  (:use :cl :std :sb-bsd-sockets :net/core :net/util :config)
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
   :mpc-searchplay
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
   :date
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
   #:mpk-music-metadata-scan
   #:*music-metadata*
   #:mpk-component
   #:mpk-toggle
   #:find-mpk-symbol
   #:*user-mpkrc*
   #:init-mpkrc
   #:mpk-prev
   #:mpk-next))

(defpackage :mpk/db
  (:use :cl :std :log :rdb :dsp/aud :dsp/gst :mpk :schema :db :id :uuid :config)
  (:export :*mpk-db-directory* :*mpk-db* :*mpk-db-schema*
   :mpk-db :mpk-db-init))

(pkg:defpkg :mpk-user
  (:use :cl :std :cl-user :std-user :sb-ext)
  (:use-reexport :mpk :mpk/db :mpk/mpd))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk :cli)
  (:export
   #:*mpk-cli*))

(defpackage :mpk/gui
  (:use :cl :std :log :mpk :gui))
