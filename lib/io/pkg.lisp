;;; io/pkg.lisp --- high-level IO API

;;

;;; Commentary:

;; pay close attention to the spec for opportunities to replace io
;; primitives -- for example WITH-OPEN-FILE accepts a :CLASS keyword
;; argument, which defaults to SB-SYS:FD-STREAM.

;; this package would be responsible for providing an alternative
;; class, something like IO-STREAM.

;; first-class backend is IO_URING. everything else defers to whatever is
;; most portable (on Windows and Mac). The URING system provides low
;; level bindings to IO_URING.

;; We implement a high-level API for IO. The API is meant to be as simple
;; as possible, while still providing a great deal of control over the
;; primitive IO queues.

;; Speed is the priority.

;; - We will probably need a specialized vector class mapping/displaced directly to URING::IOVECs
;; - io-stream doesn't share or inherit structure from sb-sys:fd-stream
;; - io-socket doesn't share or inherit structure from sb-bsd-sockets:socket
;; - prioritizing UDP/datagram/framed sockets over TCP/streamed
;; - testing:
;;   - use NET/CODEC/TLV:TLV for passing simple messages from SQ->CQ
;;   - use IO-OP-NOP for estimating throughput and comparing implementations (rust/tokio)

;;; Code:
(defpkg :io/proto
  (:use :cl :std/condition)
  (:export :io-error
   :output :input
   :output-size :input-size
   :output-buffer :input-buffer
   :input-position :output-position
   :output-available-p :input-available-p
   :fill-buffer :header
   :header-type :header-length
   :offset :snapshot
   :buffer :size))

(defpkg :io/swap-bytes
  (:use :cl :sb-c :sb-assem)
  (:shadowing-import-from :std :component :call)
  (:use :std)
  (:export #:swap-bytes-16 #:swap-bytes-32 #:swap-bytes-64
           #:htons #:ntohs #:htonl #:ntohl #:htonq #:ntohq
           #:endianness #:+endianness+ #:find-swap-byte-function))

(defpkg :io/stream
  (:use :std-lisp :io/proto :sb-gray :std/meta :sys :sb-alien)
  (:export :io-stream-error :io-stream :make-bound-stream
   :bound-input-stream :ensure-file-position
   :peeking-input-stream :peeked
   :peeked-count :peeked-size
   :make-flex-stream :make-in-memory-output-stream
   :make-in-memory-input-stream :with-output-to-sequence
   :with-input-from-sequence :output-stream-sequence-length
   :get-output-stream-sequence :peek-byte
   :peeked :flex-stream
   :flex-input-stream :flex-output-stream
   :flex-io-stream :make-decoding-stream
   :decoding-stream :+buffer-size+
   :alien-stream
   :buffered-stream :buffer-stream
   :dec-fill-buffer :needs-to-fill-buffer-p
   :read-buffer :write-buffer :read-buffer-byte :write-buffer-byte
   :read-buffer-uint32 :read-buffer-int32 :read-buffer-fixnum32 :read-buffer-fixnum64
   :write-buffer-uint32 :write-buffer-int32 :write-buffer-fixnum32 :write-buffer-fixnum64
   :read-buffer-oid :write-buffer-oid :read-buffer-float :write-buffer-float
   :read-buffer-double :write-buffer-double :read-buffer-octet-vector :write-buffer-octet-vector
   :read-buffer-to-offset :write-buffer-from-offset :reset-buffer-stream :with-buffer-streams
   :resize-buffer-stream))

(defpkg :io/static
  (:use :cl :std :sb-alien :io/stream :io/proto)
  (:shadow :constantp)
  (:export
   ;; Constructors and destructors
   :make-static-vector
   :free-static-vector
   :with-static-vector
   :with-static-vectors
   ;; Accessors
   :static-vector-pointer
   ;; Alien wrapper type
   :static-vector
   ;; Foreign memory operations
   :replace-foreign-memory
   :fill-foreign-memory
   :static-stream
   :*default-static-stream-size*
   :with-static-stream
   :with-static-streams
   :reset-static-stream))

(defpkg :io/fast
  (:use :cl :std :io/proto :io/stream :sb-gray)
  (:import-from :io/static :make-static-vector)
  (:import-from :std/macs :once-only)
  (:export
   #:fast-read-byte #:fast-write-byte
   #:fast-read-sequence #:fast-write-sequence
   #:with-fast-input #:with-fast-output
   #:write8 #:writeu8
   #:write8-le #:writeu8-le #:write8-be #:writeu8-be
   #:write16-le #:writeu16-le #:write16-be #:writeu16-be
   #:write24-le #:writeu24-le #:write24-be #:writeu24-be
   #:write32-le #:writeu32-le #:write32-be #:writeu32-be
   #:write64-le #:writeu64-le #:write64-be #:writeu64-be
   #:write128-le #:writeu128-le #:write128-be #:writeu128-be
   #:read8 #:readu8
   #:read8-le #:readu8-le #:read8-be #:readu8-be
   #:read16-le #:readu16-le #:read16-be #:readu16-be
   #:read32-le #:readu32-le #:read32-be #:readu32-be
   #:read64-le #:readu64-le #:read64-be #:readu64-be
   #:read128-le #:readu128-le #:read128-be #:readu128-be
   #:fast-output-stream #:fast-input-stream))

(defpkg :io/uring
  (:use :cl :uring :io/proto)
  (:import-from :sb-alien :addr)
  (:import-from :std :deferror :eval-always))

(defpkg :io/chunky
  (:nicknames :chunky)
  (:use :cl :std/stream :io/proto :io/stream :sb-gray :std/meta)
  (:import-from :std :deferror :when-let :define-constant :eval-always :+crlf+ :+hex-digits+)
  (:export
   #:output-chunking-p
   #:chunked-input-stream
   #:chunked-stream
   :chunked-output-stream
   #:+default-chunked-output-size+
   #:input-chunking-p
   #:simple-chunked-input-stream
   #:chunked-input-stream-extensions
   #:chunked-input-stream-trailers
   #:signal-eof
   #:expecting-crlf-p
   #:chunked-io-stream
   #:make-chunked-stream
   #:block-stream
   #:block-io-stream
   #:block-output-stream
   #:block-input-stream
   #:read-char*
   #:unread-char*
   #:peek-char*
   #:assert-char
   #:assert-crlf
   #:with-character-stream-semantics
   #:*accept-bogus-eols*
   #:*treat-semicolon-as-continuation*))

(defpkg :io/sys
  (:use :std-lisp :sys :io/proto)
  (:import-from :sb-impl :get-errno :strerror)
  (:import-from :sb-posix :syscall-error :syscall-errno :syscall-name)
  (:export :sys-condition :sys-error :syscall-error
   :repeat-upon-condition :repeat-decreasing-timeout 
   :repeat-upon-condition-decreasing-timeout :timeval-from-timeout
   :timespec-from-timeout :timeout-ms :decode-timeout :io-syscall
   :make-io-timer :io-timer :reschedule-timer :dispatch-timer
   :peek-schedule :reset-io-timer :io-timer-name :timer-reschedulable-p
   :schedule-io-timer :unschedule-io-timer :time-to-next-timer :clamp-timeout
   :reschedule-timer-relative-to-now :expire-pending-timers :io-result :io-syscall*
   :fd :poll-error :poll-timeout :wait-until-fd-ready
   :get-monotonic-time))

(defpkg :io/socket
  (:use :cl :io/proto :sb-alien :io/swap-bytes :std)
  (:export :io-socket-error 
   :io-socket :sockopt-receive-timeout :sockopt-send-timeout :sockopt-linger
   :integer-to-dotted :dotted-to-vector
   :vector-to-dotted :dotted-to-integer
   :vector-to-ipv6-host
   :ip-header :icmp-header
   :write-ip-header :write-icmp-header
   :sockopt-peercred :unknown-interface
   :sockaddr-nl :af-netlink
   :netlink-proto :netlink-proto*
   :nlm-f :nlm-f*
   :nlmsg :nlmsg*
   :nlmsghdr :nlmsgerr
   :list-network-interfaces :lookup-interface))

(defpkg :io/flate
  (:use :cl :io/proto :sb-gray)
  (:import-from :std :deferror :eval-always)
  (:import-from :std/stream :wrapped-stream)
  (:export :flate-error :compression-error :decompression-error
   :*compression-buffer-size* :decompression-buffer-size* :finish-compression :finish-decompression
   :reset-compressor :reset-decompressor
   :compress-object :decompress-object :compress :decompress
   :compressor :compressing-stream :decompressor :decompressing-stream
   :decompressing-deflate-stream :compressing-deflate-stream
   :make-decompressing-stream :make-compressing-stream
   :*decompression-buffer-size* :*compression-level*
   :compress-with :decompress-with
   :compression-level :*compressor*
   :*decompressor* :*preferred-compression-type*
   :*compression-types* :compress-octet-vector
   :decompress-octet-vector :flush :compress-octet :with-compressor
   :with-compressing-stream :with-decompressing-stream
   :compress-stream :compress-file :compress-buffer :decompress-stream
   :decompress-file :decompress-buffer))

(defpkg :io/zstd
  (:use :cl :std :io/proto :io/flate :sb-alien :zstd :sb-gray :io/static)
  (:import-from :zstd :zstd-createdstream :zstd-createcstream
   :zstd-dstream :zstd-cstream :zstd-freecstream :zstd-freedstream
   :with-zstd-dstream :with-zstd-cstream :zstd-initcstream :zstd-initdstream
   :zstd-compressstream2 :zstd-decompressstream
   :allocate-zstd-inbuffer :allocate-zstd-outbuffer :zstd-outbuffer :zstd-inbuffer
   :zstd-inbuffer-src :zstd-inbuffer-size :zstd-outbuffer-dst :zstd-outbuffer-size
   :zstd-enddirective :zstd-dstreaminsize :zstd-dstreamoutsize :zstd-cstreaminsize 
   :zstd-cstreamoutsize :zstd-inbuffer-pos :zstd-outbuffer-pos)
  (:import-from :std :deferror :eval-always)
  (:export :zstd-error :zstd-compressor :zstd-decompressor
   :with-zstd-output :with-zstd-input
   :with-zstd-buffer :with-zstd-stream))

(defpkg :io/deflate
  (:use :cl :std :io/proto :io/flate)
  (:import-from :std :deferror :eval-always)
  (:import-from :sb-gray :stream-force-output :stream-finish-output
   :stream-write-sequence)
  (:import-from :ironclad :make-digest :produce-digest :update-digest :copy-digest)
  (:export :inflate-state :bzip2-state :make-dstate 
   :finish-dstate :make-inflate-state :finish-inflate-state :deflate
   :zlib :gzip :bzip2 :invalid-format-error
   #:invalid-checksum-error
   #:premature-end-of-stream
   #:inflate-error
   #:invalid-zlib-header-error
   #:invalid-gzip-header-error
   #:reserved-block-type-error
   #:invalid-stored-block-length-error
   #:bzip2-error
   #:invalid-bzip2-data :deflate-compressor
   :zlib-compressor :gzip-compressor))

(defpkg :io/lzw
  (:use :cl :std :io/proto :io/flate)
  (:export :lzw-error :lzw-compressor :lzw-decompressor))

(defpkg :io/kbd
  (:nicknames :kbd)
  (:use :cl :std :io/proto :xkb :evdev :sb-alien)
  (:export :kbd-error
   :load-kbd-libs :keyboard
   :keysym :define-keysym
   :define-keysym-sets :keysym-set
   :keysym-set-name :load-xkb-keysyms
   :define-keysym-names :keysym-name
   :keysym-name-code :keysym-from-name
   :name-from-keysym :device-read-event
   :print-device-input-info :make-keyboard-from-dev
   :keyboard-device-p :new-device-from-path
   :evdev-bit-p :kbd-code-name
   :get-keyboards :undefine-keysym
   :key :keymap
   :keyseq
   :keybind :keymod
   :keymod-control :keymod-meta :keymod-alt :keymod-shift
   :keymod-super :keymod-hyper :keymod-altgr :keymod-numlock
   :key-control :key-meta
   :key-alt :key-shift
   :key-super :key-hyper
   :key-mods-p :keysym-cased-p
   :keysym-downcase :define-keysym
   :character-set-switch-keysym :left-shift-keysym
   :right-shift-keysym :left-control-keysym
   :right-control-keysym :caps-lock-keysym
   :shift-lock-keysym :left-meta-keysym
   :right-meta-keysym :left-alt-keysym
   :right-alt-keysym :left-super-keysym
   :right-super-keysym :left-hyper-keysym
   :right-hyper-keysym :altgr-key
   :key-sym :key-mod
   :*keymap-hook* :define-key
   :sparse-keymap :keybind-cmd
   :lookup-key :lookup-cmd
   :keybind-key :kbd
   :lookup-keyseq :search-keymap
   :deref-keymaps :keymap-symbol-p
   :keymap-or-keymap-symbol-p :print-key
   :keymap-p :parse-key :parse-keyseq :+unbound-key+
   :key-shift :*default-keysym-translate-mask*
   :keysym-code-name :print-keyseq
   :*dead-keysym-name-table* :*name-keysym-table*
   :keysyms-from-character :*keysym-character-table*
   :char-map :char-map-char :char-map-lower :char-map-mods 
   :char-map-mask :make-key
   :define-keymap :make-keymod
   :key-numlock :numlock-key
   :key-altgr :key=
   :find-key :key-eq
   :kbd-config ; late bound in OBJ/CONFIG.LISP
   :prefix-key
   :escape-key :keymaps
   :+unbound-keysym+))

(defpkg :io/xsubseq
  (:use :cl)
  (:import-from :sb-cltl2 :variable-information)
  (:import-from :std/type :octet-vector)
  (:export :xsubseq
   :octet-xsubseq :string-xsubseq
   :concatenated-xsubseqs :null-concatenated-xsubseqs
   :octet-concatenated-xsubseqs :string-concatenated-xsubseqs
   :make-concatenated-xsubseqs :xlength
   :xnconc :xnconcf
   :coerce-to-sequence :coerce-to-string
   :with-xsubseqs))

(defpkg :io/smart-buffer
  (:use :cl :io/xsubseq)
  (:import-from :std :tmp-path)
  (:export :*default-memory-limit*
   :*default-disk-limit* :smart-buffer
   :make-smart-buffer :write-to-buffer
   :finalize-buffer :with-smart-buffer
   :buffer-on-memory-p :delete-stream-file
   :delete-temporary-files :buffer-limit-exceeded))

(defpkg :io/disk
  (:nicknames :disk)
  (:use :cl :std :io/proto :btrfs :sb-alien)
  (:shadowing-import-from :std/os :dir :fsname :opts :freq :passno)
  (:export
   #:*default-filesystem*
   #:*filesystem-backends*
   #:disk-condition
   #:load-filesystem-backend
   :disk
   :disk-partition
   #:disk-snapshot
   #:disk-subvolume
   :list-disks
   :list-disk-info
   :disk-space
   :disk-total-space
   :disk-available-space
   :disk-free-space
   #:statvfs
   #:disk-info
   #:mountpoint-get
   #:mountpoint-device
   #:mountpoint-fstype
   #:mountpoint-options
   #:fsblkcnt-t
   #:fsfilcnt-t
   #:disk-use-percent
   #:mountpoint-directory))

(defpkg :io/disk/btrfs
  (:nicknames :disk/btrfs)
  (:use :cl :std :io/proto :btrfs :io/disk :sb-alien)
  (:export
   :btrfs-subvolume
   :btrfs-disk
   :btrfs-subvolumes
   :btrfs-default-subvolume
   :btrfs-snapshot
   :subvolume-valid-p
   :btrfs-partition
   :btrfs-simple-error
   :btrfs-error
   :load-btrfs-libs))

(defpkg :io/mux
  (:use :std-lisp :io/sys)
  (:export :event-base :event-dispatch :set-io-handler 
   :remove-fd-handlers :set-io-handler :set-error-handler :add-timer
   :remove-timer :exit-event-loop :event-base-empty-p :with-event-base
   :fd-entry :monitor-fd :update-fd :unmonitor-fd
   :epoll-multiplexer :multiplexer :*multiplexers* :*default-multiplexer*
   :fd-monitored-p))

(defpkg :io
  (:use :cl)
  (:use-reexport :io/proto :io/uring :io/flate 
   :io/zstd :io/stream :io/socket :io/chunky 
   :io/smart-buffer :io/static :io/xsubseq))

(defpkg :io-user
  (:use :std-lisp :io))
