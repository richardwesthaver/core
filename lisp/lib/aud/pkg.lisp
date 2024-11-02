(defpackage :aud
  (:use :cl :std :dat/midi :sndfile :alsa))

(defpackage :aud/music
  (:use :cl :std :aud))

(defpackage :aud/mpd
  (:use :cl :std :sb-bsd-sockets :net/core :net/util)
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
   :mpd-db-update))
