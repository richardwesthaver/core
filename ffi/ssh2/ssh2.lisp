;;; ffi.lisp --- SSH2 FFI

;; 

;;; Code:
(in-package :ssh2)
;;; Types
(define-alien-type libssh2-socket int)
(define-opaque libssh2-session %libssh2-session)
(define-opaque libssh2-channel %libssh2-channel)
(define-opaque libssh2-listener %libssh2-listener)
(define-opaque libssh2-knownhosts %libssh2-knownhosts)
(define-opaque libssh2-agent %libssh2-agent)
(define-alien-enum (libssh2-error)
  :NONE +LIBSSH2-ERROR-NONE+
  :SOCKET-NONE +LIBSSH2-ERROR-SOCKET-NONE+
  :BANNER-RECV +LIBSSH2-ERROR-BANNER-RECV+
  :BANNER-SEND +LIBSSH2-ERROR-BANNER-SEND+
  :INVALID-MAC +LIBSSH2-ERROR-INVALID-MAC+
  :KEX-FAILURE +LIBSSH2-ERROR-KEX-FAILURE+
  :ALLOC +LIBSSH2-ERROR-ALLOC+
  :SOCKET-SEND +LIBSSH2-ERROR-SOCKET-SEND+
  :KEY-EXCHANGE-FAILURE +LIBSSH2-ERROR-KEY-EXCHANGE-FAILURE+
  :TIMEOUT +LIBSSH2-ERROR-TIMEOUT+
  :HOSTKEY-INIT +LIBSSH2-ERROR-HOSTKEY-INIT+
  :HOSTKEY-SIGN +LIBSSH2-ERROR-HOSTKEY-SIGN+
  :DECRYPT +LIBSSH2-ERROR-DECRYPT+
  :SOCKET-DISCONNECT +LIBSSH2-ERROR-SOCKET-DISCONNECT+
  :PROTO +LIBSSH2-ERROR-PROTO+
  :PASSWORD-EXPIRED +LIBSSH2-ERROR-PASSWORD-EXPIRED+
  :FILE +LIBSSH2-ERROR-FILE+
  :METHOD-NONE +LIBSSH2-ERROR-METHOD-NONE+
  :AUTHENTICATION-FAILED +LIBSSH2-ERROR-AUTHENTICATION-FAILED+
  :PUBLICKEY-UNVERIFIED +LIBSSH2-ERROR-PUBLICKEY-UNVERIFIED+
  :CHANNEL-OUTOFORDER +LIBSSH2-ERROR-CHANNEL-OUTOFORDER+
  :CHANNEL-FAILURE +LIBSSH2-ERROR-CHANNEL-FAILURE+
  :CHANNEL-REQUEST-DENIED +LIBSSH2-ERROR-CHANNEL-REQUEST-DENIED+
  :CHANNEL-UNKNOWN +LIBSSH2-ERROR-CHANNEL-UNKNOWN+
  :CHANNEL-WINDOW-EXCEEDED +LIBSSH2-ERROR-CHANNEL-WINDOW-EXCEEDED+
  :CHANNEL-PACKET-EXCEEDED +LIBSSH2-ERROR-CHANNEL-PACKET-EXCEEDED+
  :CHANNEL-CLOSED +LIBSSH2-ERROR-CHANNEL-CLOSED+
  :CHANNEL-EOF-SENT +LIBSSH2-ERROR-CHANNEL-EOF-SENT+
  :SCP-PROTOCOL +LIBSSH2-ERROR-SCP-PROTOCOL+
  :ZLIB +LIBSSH2-ERROR-ZLIB+
  :SOCKET-TIMEOUT +LIBSSH2-ERROR-SOCKET-TIMEOUT+
  :SFTP-PROTOCOL +LIBSSH2-ERROR-SFTP-PROTOCOL+
  :REQUEST-DENIED +LIBSSH2-ERROR-REQUEST-DENIED+
  :METHOD-NOT-SUPPORTED +LIBSSH2-ERROR-METHOD-NOT-SUPPORTED+
  :INVAL +LIBSSH2-ERROR-INVAL+
  :INVALID-POLL-TYPE +LIBSSH2-ERROR-INVALID-POLL-TYPE+
  :PUBLICKEY-PROTOCOL +LIBSSH2-ERROR-PUBLICKEY-PROTOCOL+
  :EAGAIN +LIBSSH2-ERROR-EAGAIN+
  :BUFFER-TOO-SMALL +LIBSSH2-ERROR-BUFFER-TOO-SMALL+
  :BAD-USE +LIBSSH2-ERROR-BAD-USE+
  :COMPRESS +LIBSSH2-ERROR-COMPRESS+
  :OUT-OF-BOUNDARY +LIBSSH2-ERROR-OUT-OF-BOUNDARY+
  :AGENT-PROTOCOL +LIBSSH2-ERROR-AGENT-PROTOCOL+
  :SOCKET-RECV +LIBSSH2-ERROR-SOCKET-RECV+
  :ENCRYPT +LIBSSH2-ERROR-ENCRYPT+
  :BAD-SOCKET +LIBSSH2-ERROR-BAD-SOCKET+
  :KNOWN-HOSTS +LIBSSH2-ERROR-KNOWN-HOSTS+)

(define-alien-enum (ssh-disconnect)
  :HOST-NOT-ALLOWED-TO-CONNECT +SSH-DISCONNECT-HOST-NOT-ALLOWED-TO-CONNECT+
  :PROTOCOL-ERROR +SSH-DISCONNECT-PROTOCOL-ERROR+
  :KEY-EXCHANGE-FAILED +SSH-DISCONNECT-KEY-EXCHANGE-FAILED+
  :RESERVED +SSH-DISCONNECT-RESERVED+
  :MAC-ERROR +SSH-DISCONNECT-MAC-ERROR+
  :COMPRESSION-ERROR +SSH-DISCONNECT-COMPRESSION-ERROR+
  :SERVICE-NOT-AVAILABLE +SSH-DISCONNECT-SERVICE-NOT-AVAILABLE+
  :PROTOCOL-VERSION-NOT-SUPPORTED +SSH-DISCONNECT-PROTOCOL-VERSION-NOT-SUPPORTED+
  :HOST-KEY-NOT-VERIFIABLE +SSH-DISCONNECT-HOST-KEY-NOT-VERIFIABLE+
  :CONNECTION-LOST +SSH-DISCONNECT-CONNECTION-LOST+
  :BY-APPLICATION +SSH-DISCONNECT-BY-APPLICATION+
  :TOO-MANY-CONNECTIONS +SSH-DISCONNECT-TOO-MANY-CONNECTIONS+
  :AUTH-CANCELLED-BY-USER +SSH-DISCONNECT-AUTH-CANCELLED-BY-USER+
  :NO-MORE-AUTH-METHODS-AVAILABLE +SSH-DISCONNECT-NO-MORE-AUTH-METHODS-AVAILABLE+
  :ILLEGAL-USER-NAME +SSH-DISCONNECT-ILLEGAL-USER-NAME+)

(define-alien-enum (libssh2-callback)
  :ignore 0
  :debug 1
  :disconnect 2
  :macerror 3
  :x11 4
  :send 5
  :recv 6
  :authagent 7
  :authagent-identities 8
  :authagent-sign 9)

(define-alien-enum (libssh2-method)
  :kex 0
  :hostkey 1
  :crypt-cs 2
  :crypt-sc 3
  :mac-cs 4
  :mac-sc 5
  :comp-cs 6
  :comp-sc 7
  :lang-cs 8
  :lang-sc 9
  :sign-algo 10)

(define-alien-enum (libssh2-flag)
  :sigpipe 1
  :compress 2
  :quote-paths 3)

(define-alien-enum (libssh2-pollfd)
  :socket 1
  :channel 2
  :listener 3
  :pollin #x0001
  :pollpri #x0002
  :pollout #x0004
  :pollerr #x0008
  :pollhup #x0010
  :session-closed #x0010
  :pollnval #x0020
  :pollex #x0040
  :channel-closed #x0080
  :listener-closed #x0080)

(define-alien-enum (libssh2-hostkey-type)
  :unknown 0
  :rsa 1
  :dss 2 ;; deprecated
  :ecdsa-256 3
  :ecdsa-384 4
  :ecdsa-521 5
  :ed25519 6)

(define-alien-enum (libssh2-session-block)
  :inbound #x0001
  :outbound #x0002)

(define-alien-enum (libssh2-hostkey-hash-type)
  :md5 1
  :sha1 2
  :sha256 3)

;;; Sessions
(defar libssh2-init int (flags int))

(defar libssh2-exit void)

(defar libssh2-free void (session (* t)) (ptr (* t)))

(defar libssh2-session-supported-algs int
  (session (* libssh2-session))
  (method-type int)
  (algs (* (* c-string))))

(defar libssh2-session-init-ex (* libssh2-session)
  (alloc (* t))
  (free (* t))
  (realloc (* t))
  (abstract (* t)))

(definline libssh2-session-init () (libssh2-session-init-ex nil nil nil nil))

(defar libssh2-session-abstract (* (* t)) (sesh (* libssh2-session)))
(defar libssh2-session-callback-set (* t) 
  (sesh (* libssh2-session))
  (cbtype int)
  (callback (* t)))

(defar libssh2-session-banner-set int
  (sesh (* libssh2-session))
  (banner c-string))

(defar libssh2-banner-set int
  (sesh (* libssh2-session))
  (banner c-string))

(defar libssh2-session-startup int
  (sesh (* libssh2-session))
  (sock int))

(defar libssh2-sesssion-handshake int
  (sesh (* libssh2-session))
  (sock libssh2-socket))

(defar libssh2-session-disconnect-ex int
  (sesh (* libssh2-session))
  (reason int)
  (description c-string)
  (lang c-string))

(defar libssh2-session-free int
  (sesh (* libssh2-session)))

(defar libssh2-hostkey-hash c-string
  (sesh (* libssh2-session))
  (hash-type int))

(defar libssh2-session-hostkey c-string
  (sesh (* libssh2-session))
  (len (* size-t))
  (type (* int)))

(defar libssh2-session-method-pref int
  (sesh (* libssh2-session))
  (method-type int)
  (prefs c-string))
(defar libssh2-session-methods c-string
  (sesh (* libssh2-session))
  (method-type int))
(defar libssh2-session-last-error int
  (sesh (* libssh2-session))
  (errmsg (* c-string))
  (errmsg-len (* int))
  (want-buf int))
(defar libssh2-session-last-errno int
  (sesh (* libssh2-session)))
(defar libssh2-session-set-last-error int
  (sesh (* libssh2-session))
  (errcode int)
  (errmsg c-string))
(defar libssh2-session-block-directions int
  (sesh (* libssh2-session)))
(defar libssh2-session-flag int
  (sesh (* libssh2-session))
  (flag int) (value int))
(defar libssh2-session-banner-get c-string
  (sesh (* libssh2-session)))
(defar libssh2-userauth-list c-string
  (sesh (* libssh2-session))
  (username c-string)
  (username-len unsigned-int))
(defar libssh2-userauth-banner int
  (sesh (* libssh2-session))
  (banner (* c-string)))
(defar libssh2-userauth-authenticated int
  (sesh (* libssh2-session)))
(defar libssh2-userauth-password-ex int
  (sesh (* libssh2-session))
  (username c-string)
  (username-len unsigned-int)
  (password c-string)
  (password-len unsigned-int)
  ;; TODO
  (password-change-cb (* t)))
(defar libssh2-userauth-publickey-fromfile-ex int
  (sesh (* libssh2-session))
  (username c-string)
  (username-len unsigned-int)
  (publickey c-string)
  (privatekey c-string)
  (passphrase c-string))
(defar libssh2-userauth-publickey int
  (sesh (* libssh2-session))
  (username c-string)
  (pubkeydata (* unsigned-char))
  (pubkeydata-len size-t)
  ;; TODO
  (sign-callback (* t))
  (abstract (* (* t))))

#+nil
(with-alien ((arr (* c-string)))
  (let ((sesh (libssh2-session-init-ex nil nil nil nil)))
    (libssh2-session-supported-algs sesh 0 (addr arr))
    (c-strings-to-string-list (print arr))))

;;; Channels
(defar libssh2-channel-open-ex (* libssh2-channel)
  (sesh (* libssh2-session))
  (channel-type c-string)
  (channel-type-len unsigned-int)
  (window-size unsigned-int)
  (packet-size unsigned-int)
  (message c-string)
  (message-len unsigned-int))
(defar libssh2-channel-direct-tcpip-ex (* libssh2-channel)
  (sesh (* libssh2-session))
  (host c-string)
  (port int)
  (shost c-string)
  (sport int))
(defar libssh2-channel-direct-streamlocal-ex (* libssh2-channel)
  (sesh (* libssh2-session))
  (socket-path c-string)
  (shost c-string)
  (sport int))
(defar libssh2-channel-forward-listen-ex (* libssh2-listener)
  (sesh (* libssh2-session))
  (host c-string)
  (port int)
  (bound-port (* int))
  (queue-maxsize int))
(defar libssh2-channel-forward-cancel int
  (listener (* libssh2-listener)))

(defar libssh2-channel-forward-accept (* libssh2-channel)
  (listener (* libssh2-listener)))

(defar libssh2-channel-setenv-ex int
  (channel (* libssh2-channel))
  (varname c-string)
  (varname-len unsigned-int)
  (value c-string)
  (value-len unsigned-int))
(defar libssh2-channel-request-auth-agent int
  (channel (* libssh2-channel)))
(defar libssh2-channel-request-pty-ex int
  (channel (* libssh2-channel))
  (term c-string)
  (term-len unsigned-int)
  (modes c-string)
  (modes-len unsigned-int)
  (width int) (height int)
  (width-px int) (height-px int))
(defar libssh2-channel-request-pty-size-ex int
  (channel (* libssh2-channel))
  (width int) (height int)
  (width-px int) (height-px int))
(defar libssh2-channel-x11-req-ex int
  (channel (* libssh2-channel))
  (single-connection int)
  (auth-proto c-string)
  (auth-cookie c-string)
  (screen-number int))
(defar libssh2-channel-signal-ex int
  (channel (* libssh2-channel))
  (signame c-string)
  (signame-len size-t))
(defar libssh2-channel-process-startup int
  (channel (* libssh2-channel))
  (request c-string)
  (request-len unsigned-int)
  (message c-string)
  (message-len unsigned-int))
(defar libssh2-channel-read-ex ssize-t
  (channel (* libssh2-channel))
  (stream-id int)
  (buf (* char))
  (buflen size-t))
(defar libssh2-poll-channel-read int
  (channel (* libssh2-channel))
  (extended int))
(defar libssh2-channel-window-read-ex unsigned-long
  (channel (* libssh2-channel))
  (read-avail (* unsigned-long))
  (window-size-initial (* unsigned-long)))


(defar libssh2-channel-receive-window-adjust2 int
  (channel (* libssh2-channel))
  (adjustment unsigned-long)
  (force unsigned-char)
  (storewindow (* unsigned-int)))

(defar libssh2-channel-write-ex ssize-t
  (channel (* libssh2-channel))
  (stream-id int)
  (buf c-string)
  (buflen size-t))

(defar libssh2-channel-window-write-ex unsigned-long
  (channel (* libssh2-channel))
  (window-size-initial (* unsigned-long)))

(defar libssh2-session-set-blocking void
  (session (* libssh2-session))
  (blocking int))
(defar libssh2-session-get-blocking int
  (sesh (* libssh2-session)))
(defar libssh2-channel-set-blocking void
  (channel (* libssh2-channel))
  (blocking int))
(defar libssh2-session-set-timeout void
  (sesh (* libssh2-session))
  (timeout long))
(defar libssh2-session-get-timeout long
  (sesh (* libssh2-session)))
(defar libssh2-session-set-read-timeout void
  (sesh (* libssh2-session))
  (timeout long))

(defar libssh2-session-get-read-timeout long
  (sesh (* libssh2-session)))

(defar libssh2-channel-handle-extended-data2 int
  (channel (* libssh2-channel))
  (ignore-mode int))

(defar libssh2-channel-flush-ex int
  (channel (* libssh2-channel))
  (streamid int))

(defar libssh2-channel-get-exit-status int (channel (* libssh2-channel)))
(defar libssh2-channel-get-exit-signal int 
  (channel (* libssh2-channel))
  (exitsignal (* c-string))
  (exitsignal-len (* size-t))
  (errmsg (* c-string))
  (errmsg-len (* size-t))
  (langtag (* c-string))
  (langtag-len (* size-t)))
(defar libssh2-channel-send-eof int
  (channel (* libssh2-channel)))

(defar libssh2-channel-eof int
  (channel (* libssh2-channel)))

(defar libssh2-channel-wait-eof int
  (channel (* libssh2-channel)))
(defar libssh2-channel-close int
  (channel (* libssh2-channel)))
(defar libssh2-channel-wait-closed int
  (channel (* libssh2-channel)))
(defar libssh2-channel-free int
  (channel (* libssh2-channel)))

;; (defar libssh2-scp-recv2 (* libssh2-channel)
;;   (sesh (* libssh2-session))
;;   (path c-string)
;;   (sb (* libssh2-struct-stat)))

(defar libssh2-scp-send-ex (* libssh2-channel)
  (sesh (* libssh2-session))
  (path c-string)
  (mod int)
  (size size-t)
  (mtime long)
  (atime long))

;; (defar libssh2-scp-send64 (* libssh2-channel)
;;   (sesh (* libssh2-session))
;;   (path c-string)
;;   (mode int)
;;   (size libssh2-int64)
;;   (mtime time-t)
;;   (atime time-t))

(defar libssh2-version c-string
  (req-version-num int))

(define-alien-enum (libssh2-crypto-engine-type)
  :no-crypto 0
  :openssl 1
  :gcrypt 2
  :mbedtls 3
  :wincng 4
  :os400qc3 5)

(defar libssh2-crypto-engine int)

;;; Knownhosts
(define-alien-type libssh2-knownhost
    (struct libssh2-knownhost
      (magic unsigned-int)
      (node (* t))
      (name c-string)
      (key c-string)
      (typemask int)))

(defar libssh2-knownhost-init (* libssh2-knownhosts)
  (sesh (* libssh2-session)))

;;; Agent
(define-alien-type libssh2-agent-publickey
    (struct libssh2-agent-publickey
      (magic unsigned-int)
      (node (* t))
      (blob (* unsigned-char))
      (blob-len size-t)
      (comment c-string)))
      
(defar libssh2-agent-init (* libssh2-agent)
  (sesh (* libssh2-session)))

(defar libssh2-agent-connect int
  (agent (* libssh2-agent)))

(defar libssh2-agent-list-identities int
  (agent (* libssh2-agent)))

(defar libssh2-agent-free void
  (agent (* libssh2-agent)))
