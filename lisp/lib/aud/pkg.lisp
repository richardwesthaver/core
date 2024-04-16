(defpackage :aud
  (:use :cl :std :dat/midi :obj/music :sndfile :alsa))

(defpackage :aud/mpd
  (:use :cl :std :sb-bsd-sockets :net/core :net/util)
  (:nicknames :mpd)
  (:export
   :*default-host*
   :*default-port*
   :connect
   :disconnect
   :password
   :with-mpc
   :disable-output
   :enable-output
   :outputs
   :ensure-mpd
   :ping
   :kill
   :status

   :now-playing
   :pause
   :play
   :stop
   :previous
   :next
   :crossfade

   :add
   :add-id
   :move
   :move-id
   :swap
   :swap-id
   :clear
   :delete-track
   :delete-id
   :save-playlist
   :load-playlist
   :rename-playlist
   :playlist-info
   :playlist-changes
   :shuffle
   :list-playlist
   :list-playlist-info
   :add-to-playlist
   :clear-playlist
   :delete-from-playlist
   :move-in-playlist
   :find-in-current-playlist
   :search-in-current-playlist

   :update

   :list-all
   :list-info
   :list-all-info
   :find-tracks
   :search-tracks
   :list-metadata
   :count-tracks

   :commands
   :not-commands
   :tag-types
   :url-handlers

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
   :id

   :mpd-error
   :protocol-mismatch
   :bad-argument
   :incorrect-password
   :not-permitted
   :unknown-command
   :not-exist
   :playlist-size-exceed
   :already-updating
   :exist

   :volume
   :repeat
   :randomized
   :playlist-version
   :playlist-length
   :xfade
   :state
   :audio
   :bitrate
   :duration
   :songid
   :song
   :nextsongid
   :nextsong
   :elapsed
   :mixrampdb
   :consume
   :single

   :artists
   :albums
   :songs
   :uptime
   :playtime
   :db-playtime
   :db-update))

(in-package :aud)
(ignore-errors
 (load-asound)
 (load-sndfile))
