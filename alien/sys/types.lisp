;;; types.lisp --- Sys Types

;; 

;;; Code:
(in-package :sys)

;; errno.h
(define-alien-variable program-invocation-name c-string)
(define-alien-variable program-invocation-short-name c-string)
;; (defar ("__errno_location" errno-location) (* int))

;; convenience enums for errors and signals (grovelled mostly by sb-posix)
(define-alien-enum (err)
  :e2big sb-posix::E2BIG
  :eacces sb-posix::eacces
  :eaddrinuse sb-posix::EADDRINUSE
  :eaddrnotavail sb-posix::EADDRNOTAVAIL
  :eadv sb-posix::eadv
  :eafnosupport sb-posix::EAFNOSUPPORT
  :ealready sb-posix::EALREADY
  :ebade sb-posix::EBADE
  :ebadf sb-posix::EBADF
  :ebadfd sb-posix::EBADFD
  :ebadmsg sb-posix::EBADMSG
  :ebadr sb-posix::EBADR
  :ebadrqc sb-posix::EBADRQC
  :ebadslt sb-posix::EBADSLT
  :ebfont sb-posix::EBFONT
  :ebusy sb-posix::EBUSY
  :ecanceled ecanceled
  :echild sb-posix::ECHILD
  :echrng sb-posix::ECHRNG
  :ecomm sb-posix::ECOMM
  :econnaborted sb-posix::ECONNABORTED
  :econnrefused sb-posix::ECONNREFUSED
  :econnreset sb-posix::ECONNRESET
  :edeadlk sb-posix::EDEADLK
  :edestaddrreq sb-posix::EDESTADDRREQ
  :edom sb-posix::EDOM
  :edotdot sb-posix::EDOTDOT
  :edquot sb-posix::EDQUOT
  :eexist sb-posix::EEXIST
  :efault sb-posix::EFAULT
  :efbig sb-posix::EFBIG
  :ehostdown sb-posix::EHOSTDOWN
  :ehostunreach sb-posix::EHOSTUNREACH
  ;; :ehwpoison sb-posix::EHWPOISON
  :eidrm sb-posix::EIDRM
  :eilseq sb-posix::EILSEQ
  :einprogress sb-posix::EINPROGRESS
  :eintr sb-posix::EINTR
  :einval sb-posix::EINVAL
  :eio sb-posix::EIO
  :eisconn sb-posix::EISCONN
  :eisdir sb-posix::EISDIR
  ;; :eisnam sb-posix::EISNAM
  ;; :ekeyexpired sb-posix::EKEYEXPIRED
  ;; :ekeyrejected sb-posix::EKEYREJECTED
  ;; :ekeyrevoked sb-posix::EKEYREVOKED
  :el2hlt sb-posix::EL2HLT
  :el2nsync sb-posix::EL2NSYNC
  :el3hlt sb-posix::EL3HLT
  :el3rst sb-posix::EL3RST
  :elibacc sb-posix::ELIBACC
  :elibbad sb-posix::ELIBBAD
  :elibexec sb-posix::ELIBEXEC
  :elibmax sb-posix::ELIBMAX
  :elibscn sb-posix::ELIBSCN
  :elnrng sb-posix::ELNRNG
  :eloop sb-posix::ELOOP
  :emediumtype sb-posix::EMEDIUMTYPE
  :emfile sb-posix::EMFILE
  :emlink sb-posix::EMLINK
  :emsgsize sb-posix::EMSGSIZE
  :emultihop sb-posix::EMULTIHOP
  :enametoolong sb-posix::ENAMETOOLONG
  :enavail sb-posix::ENAVAIL
  :enetdown sb-posix::ENETDOWN
  :enetreset sb-posix::ENETRESET
  :enetunreach sb-posix::ENETUNREACH
  :enfile sb-posix::ENFILE
  :enoano sb-posix::ENOANO
  :enobufs sb-posix::ENOBUFS
  :enocsi sb-posix::ENOCSI
  :enodata sb-posix::ENODATA
  :enodev sb-posix::ENODEV
  :enoent sb-posix::ENOENT
  :enoexec sb-posix::ENOEXEC
  ;; :enokey sb-posix::ENOKEY
  :enolck sb-posix::ENOLCK
  :enolink sb-posix::ENOLINK
  :enomedium sb-posix::ENOMEDIUM
  :enomem sb-posix::ENOMEM
  :enomsg sb-posix::ENOMSG
  :enonet sb-posix::ENONET
  :enopkg sb-posix::ENOPKG
  :enoprotoopt sb-posix::ENOPROTOOPT
  :enospc sb-posix::ENOSPC
  :enosr sb-posix::ENOSR
  :enostr sb-posix::ENOSTR
  :enosys sb-posix::ENOSYS
  :enotblk sb-posix::ENOTBLK
  :enotconn sb-posix::ENOTCONN
  :enotdir sb-posix::ENOTDIR
  :enotempty sb-posix::ENOTEMPTY
  :enotnam sb-posix::ENOTNAM
  ;; :enotrecoverable sb-posix::ENOTRECOVERABLE
  :enotsock sb-posix::ENOTSOCK
  :enotsup ENOTSUP
  :enotty sb-posix::ENOTTY
  :enotuniq sb-posix::ENOTUNIQ
  :enxio sb-posix::ENXIO
  ;; :eopnotsupp sb-posix::EOPNOTSUPP
  :eoverflow sb-posix::EOVERFLOW
  ;; :eownerdead sb-posix::EOWNERDEAD
  :eperm sb-posix::EPERM
  :epfnosupport sb-posix::EPFNOSUPPORT
  :epipe sb-posix::EPIPE
  :eproto sb-posix::EPROTO
  :eprotonosupport sb-posix::EPROTONOSUPPORT
  :eprototype sb-posix::EPROTOTYPE
  :erange sb-posix::ERANGE
  :eremchg sb-posix::EREMCHG
  :eremote sb-posix::EREMOTE
  :eremoteio sb-posix::EREMOTEIO
  :erestart sb-posix::ERESTART
  ;; :erfkill sb-posix::ERFKILL
  :erofs sb-posix::EROFS
  :eshutdown sb-posix::ESHUTDOWN
  :esocktnosupport sb-posix::ESOCKTNOSUPPORT
  :espipe sb-posix::ESPIPE
  :esrch sb-posix::ESRCH
  :esrmnt sb-posix::ESRMNT
  :estale sb-posix::ESTALE
  :estrpipe sb-posix::ESTRPIPE
  :etime sb-posix::ETIME
  :etimedout sb-posix::ETIMEDOUT
  :etoomanyrefs sb-posix::ETOOMANYREFS
  :etxtbsy sb-posix::ETXTBSY
  :euclean sb-posix::EUCLEAN
  :eunatch sb-posix::EUNATCH
  :eusers sb-posix::EUSERS
  :ewouldblock sb-posix::EWOULDBLOCK
  :exdev sb-posix::EXDEV
  :exfull sb-posix::EXFULL)

(define-alien-enum (sig :type int)
  :sighup    sb-posix::sighup
  :sigint    sb-posix::sigint
  :sigquit   sb-posix::sigquit
  :sigill    sb-posix::sigill
  :sigabrt   sb-posix::sigabrt
  :sigfpe    sb-posix::sigfpe
  :sigkill   sb-posix::sigkill
  :sigsegv   sb-posix::sigsegv
  :sigpipe   sb-posix::sigpipe
  :sigalrm   sb-posix::sigalrm
  :sigterm   sb-posix::sigterm
  :sigusr1   sb-posix::sigusr1
  :sigusr2   sb-posix::sigusr2
  :sigchld   sb-posix::sigchld
  :sigcont   sb-posix::sigcont
  :sigstop   sb-posix::sigstop
  :sigtstp   sb-posix::sigtstp
  :sigttin   sb-posix::sigttin
  :sigttou   sb-posix::sigttou
  ;; POSIX.1-2001
  :sigbus    sb-posix::sigbus
  ;; :sigpoll
  :sigprof   sb-posix::sigprof
  :sigsys    sb-posix::sigsys
  :sigtrap   sb-posix::sigtrap
  :sigurg    sb-posix::sigurg
  :sigvtalrm sb-posix::sigvtalrm
  :sigxcpu   sb-posix::sigxcpu
  :sigxfsz   sb-posix::sigxfsz
  ;; Other signals
  ;; ((:sigcld    "SIGCLD") :optional t)
  ;; ((:siginfo   "SIGINFO") :optional t)
  ;; ((:siglost   "SIGLOST") :optional t)
  :sigpwr    sb-posix::sigpwr
  :sigio     sb-posix::sigio
  :sigwinch  sb-posix::sigwinch)

(define-alien-type sigaction
    (struct sigaction
      (handler (* t))
      (sigaction (* t))
      (mask unsigned-long)
      (flags int)))

(define-alien-type if-nameindex
  (struct if-nameindex
    (index unsigned-int)
    (name c-string)))

(define-alien-type ifreq
  (struct ifreq
    (name (* char))))

(define-alien-enum (tcp-state)
  :established 1
  :syn-sent 2
  :syn-recv 3
  :fin-wait1 4
  :fin-wait2 5
  :time-wait 6
  :close 7
  :close-wait 8
  :last-ack 9
  :listen 10
  :closing 11)

;; values for tcpi-state
(define-alien-enum (tc-ca-state)
  :open 0
  :disorder 1
  :cwr 2
  :recovery 3
  :loss 4)

(define-alien-type tcp-info
  (struct tcp-info
    (tcpi-state unsigned-char)
    (tcpi-ca-state unsigned-char)
    (tcpi-retransmits unsigned-char)
    (tcpi-probes unsigned-char)
    (tcpi-backoff unsigned-char)
    (tcpi-options unsigned-char)
    (tcpi-snd-wscale unsigned-char)
    (tcpi-rcv-wscale unsigned-char)
    (tcpi-rto unsigned-int)
    (tcpi-ato unsigned-int)
    (tcpi-snd-mss unsigned-int)
    (tcpi-rcv-mss unsigned-int)
    (tcpi-unacked unsigned-int)
    (tcpi-sacked unsigned-int)
    (tcpi-lost unsigned-int)
    (tcpi-retrans unsigned-int)
    (tcpi-fackets unsigned-int)
    ;; Times
    (tcpi-last-data-sent unsigned-int)
    (tcpi-last-ack-sent unsigned-int)
    (tcpi-last-data-recv unsigned-int)
    (tcpi-last-ack-recv unsigned-int)
    ;; Metrics
    (tcpi-pmtu unsigned-int)
    (tcpi-rcv-ssthresh unsigned-int)
    (tcpi-rtt unsigned-int)
    (tcpi-rttvar unsigned-int)
    (tcpi-snd-ssthresh unsigned-int)
    (tcpi-snd-cwnd unsigned-int)
    (tcpi-advmss unsigned-int)
    (tcpi-reordering unsigned-int)

    (tcpi-rcv-rtt unsigned-int)
    (tcpi-rcv-space unsigned-int)

    (tcpi-total-retrans unsigned-int)

    (tcpi-pacing-rate unsigned-long)
    (tcpi-max-pacing-rate unsigned-long)
    (tcpi-bytes-acked unsigned-long)
    (tcpi-bytes-received unsigned-long)
    (tcpi-segs-out unsigned-int)
    (tcpi-segs-in unsigned-int)

    (tcpi-notsent-bytes unsigned-int)
    (tcpi-min-rtt unsigned-int)
    (tcpi-data-segs-in unsigned-int)
    (tcpi-data-segs-out unsigned-int)

    (tcpi-delivery-rate unsigned-long)

    (tcpi-busy-time unsigned-long)
    (tcpi-rwnd-limited unsigned-long)
    (tcpi-sndbuf-limited unsigned-long)

    (tcpi-delivered unsigned-int)
    (tcpi-delivered-ce unsigned-int)

    (tcpi-bytes-sent unsigned-long)
    (tcpi-bytes-retrans unsigned-long)
    (tcpi-dsack-dups unsigned-int)
    (tcpi-reord-seen unsigned-int)


    (tcpi-rcv-ooopack unsigned-int)
  ;; Peer's advertised receive window after scaling (bytes)
    (tcpi-snd-wnd unsigned-int)
    ;; Local advertised receive window after scaling (bytes)
    (tcpi-rcv-wnd unsigned-int)

    (tcpi-rehash unsigned-int)
    ;; Total number of RTO timeouts, including SYN/SYN-ACK and recurring timeouts
    (tcpi-total-rto unsigned-short)
  ;; Total number of RTO recoveries, including any unfinished recovery.
    (tcpi-total-rto-recoveries unsigned-short)
    ;; Total time spent in RTO recoveries in milliseconds, including any unfinished recovery.
    (tcpi-total-rto-time unsigned-int)
    (tcpi-received-ce unsigned-int)
    (tcpi-delivered-e1-bytes unsigned-int)
    (tcpi-delivered-e0-bytes unsigned-int)
    (tcpi-delivered-ce-bytes unsigned-int)
    (tcpi-received-e1-bytes unsigned-int)
    (tcpi-received-e0-bytes unsigned-int)
    (tcpi-received-ce-bytes unsigned-int)
    (tcpi-accecn-fail-mode unsigned-short)
    (tcpi-accecn-opt-seen unsigned-short)))

#+todo
(define-alien-type tcp-md5sig)

(define-alien-type tcp-diag-md5sig
    (struct tcp-diag-md5sig
      (tcpm-family unsigned-char)
      (tcpm-prefixlen unsigned-char)
      (tcpm-keylen unsigned-int)
      (tcpm-addr (array unsigned-int 4))
      (tcpm-key (array unsigned-char #.tcp-md5sig-maxkeylen))))

(define-alien-type tcp-zerocopy-receive
  (struct tcp-zerocopy-receive
    ;; In: address of mapping.  
    (address unsigned-long)
    ;; In/out: number of bytes to map/mapped.  
    (length unsigned-int)
    ;; Out: amount of bytes to skip.  
    (recv-skip-hint unsigned-int)
    ;; Out: amount of bytes in read queue.  
    (inq unsigned-int)
    ;; Out: socket error.  
    (err int)
    ;; On: copybuf address (small reads).  
    (copybuf-address unsigned-long)
    ;; In/Out: copybuf bytes avail/used or error.  
    (copybuf-len int)
    ;; In: flags.  
    (flags unsigned-int)
    ;; Ancillary data.  
    (msg-control unsigned-long)
    (msg-controllen unsigned-long)
    (msg-flag unsigned-int)
    ;; Set to 0 for now.  
    (reserved unsigned-int)))

;; openat2
(define-alien-type open-how
  (struct open-how
    (flags unsigned-long)
    (mode unsigned-long)
    (resolve unsigned-long)))

;; msghdr
(define-alien-type cmsghdr 
  (struct cmsghdr
    (len sockint::socklen-t)
    (level int)
    (type int)))

(define-alien-type linger
    (struct linger
      (onoff int)
      (linger int)))
