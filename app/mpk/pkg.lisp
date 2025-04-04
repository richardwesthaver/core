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
   :mpd-ping
   :mpd-kill
   :mpd-status
   :mpd-now-playing
   :mpd-pause
   :mpd-play
   :mpd-previous
   :mpd-next
   :mpd-crossfade
   :mpd-add
   :mpd-add-id
   :mpd-move
   :mpd-move-id
   :mpd-swap
   :mpd-swap-id
   :mpd-clear
   :mpd-delete-track
   :mpd-delete-id
   :mpd-save-playlist
   :mpd-load-playlist
   :mpd-rename-playlist
   :mpd-playlist-info
   :mpd-playlist-changes
   :mpd-shuffle
   :mpd-list-playlist
   :mpd-list-playlist-info
   :mpd-add-to-playlist
   :mpd-clear-playlist
   :mpd-delete-from-playlist
   :mpd-move-in-playlist
   :mpd-find-in-current-playlist
   :mpd-search-in-current-playlist
   :mpd-update
   :mpd-rescan
   :mpd-stats
   :mpd-list-all
   :mpd-list-info
   :mpd-list-all-info
   :mpd-find-tracks
   :mpd-search-tracks
   :mpd-list-metadata
   :mpd-count-tracks
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
   :mpd-artists
   :mpd-albums
   :mpd-songs
   :mpd-uptime
   :mpd-playtime
   :mpd-db-playtime
   :mpd-db-update
   :mpd-config))

(defpackage :mpk
  (:use :cl :std :log :id :config :ast :cli/tools/net :cli/tools/media :time)
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
   #:*mpc*))

(defpackage :mpk/db
  (:nicknames :mdb)
  (:use :cl :std :log :rdb :dsp/aud :dsp/gst :mpk :schema :db :id :uuid)
  (:export :*mdb-directory* :*mdb* :*mdb-schema*
   :mdb :init-mdb))

(pkg:defpkg :mpk-user
  (:use :cl :std :cl-user :std-user :sb-ext)
  (:use-reexport :mpk :mpk/db :mpk/mpd))

(defpackage :mpk/cli
  (:use :cl :std :log :mpk :cli)
  (:export
   #:*mpk-cli*))
