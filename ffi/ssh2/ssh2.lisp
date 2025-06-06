;;; ffi.lisp --- SSH2 FFI

;; 

;;; Code:
(in-package :ssh2)
(define-alien-type libssh2-socket int)
(define-opaque libssh2-session %libssh2-session)
(define-opaque libssh2-channel %libssh2-channel)
(define-opaque libssh2-listener %libssh2-listener)
(define-opaque libssh2-knownhosts %libssh2-knownhosts)
(define-opaque libssh2-agent %libssh2-agent)

(define-alien-enum (libssh2-error int)
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

(define-alien-enum (ssh-disconnect int)
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

(define-alien-enum (libssh2-callback int)
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

(define-alien-enum (libssh2-method int)
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

(define-alien-enum (libssh2-flag int)
  :sigpipe 1
  :compress 2
  :quote-paths 3)

(define-alien-enum (libssh2-pollfd int)
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

(define-alien-enum (libssh2-hostkey-type int)
  :unknown 0
  :rsa 1
  :dss 2 ;; deprecated
  :ecdsa-256 3
  :ecdsa-384 4
  :ecdsa-521 5
  :ed25519 6)

(define-alien-enum (libssh2-session-block int)
  :inbound #x0001
  :outbound #x0002)

(define-alien-enum (libssh2-hostkey-hash int)
  :md5 1
  :sha1 2
  :sha256 3)

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

#+nil
(with-alien ((arr (* c-string)))
  (let ((sesh (libssh2-session-init-ex nil nil nil nil)))
    (libssh2-session-supported-algs sesh 0 (addr arr))
    (c-strings-to-string-list (print arr))))
