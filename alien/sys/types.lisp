;;; types.lisp --- Sys Types

;; 

;;; Code:
(in-package :sys)

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
