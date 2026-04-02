;;; seccomp.lisp --- Linux seccomp low-level bindings

;; 

;;; Code:
(in-package :security)

(define-alien-type scmp-version
    (struct scmp-version
      (major unsigned-int)
      (minor unsigned-int)
      (micro unsigned-int)))

(define-alien-type scmp-filter-ctx (* t))

;; (define-alien-enum scmp-fltatr)
(define-alien-enum (scmp-cmp)
  :min 0
  :ne 1
  :lt 2
  :le 3
  :eq 4
  :ge 5
  :gt 6
  :masked-eq 7
  :max 8)

(define-alien-type scmp-datum unsigned-long)

(define-alien-type scmp-arg-cmp
    (struct scmp-arg-cmp
      (arg unsigned-int)
      (op scmp-cmp)
      (datum-a scmp-datum)
      (datum-b scmp-datum)))

(define-alien-type seccomp-data
    (struct seccomp-data
      (nr int)
      (arch unsigned-int)
      (instruction-pointer unsigned-long)
      (args (array unsigned-long 6))))

(define-alien-type seccomp-notif
    (struct seccomp-notif
      (id unsigned-long)
      (pid unsigned-int)
      (flags unsigned-int)
      (data seccomp-data)))

(define-alien-type seccomp-notif-resp
    (struct seccomp-notif-resp
      (id unsigned-long)
      (val long)
      (error int)
      (flags unsigned-int)))

(defar seccomp-version (* scmp-version))

(defar seccomp-api-get unsigned-int)
(defar seccomp-api-set int (level unsigned-int))
(defar seccomp-init scmp-filter-ctx (def-action unsigned-int))
(defar seccomp-reset int (ctx scmp-filter-ctx) (def-action unsigned-int))
(defar seccomp-release void
  (ctx scmp-filter-ctx))
(defar seccomp-merge int
  (ctx-dst scmp-filter-ctx)
  (ctx-src scmp-filter-ctx))
(defar seccomp-arch-resolve-name unsigned-int
  (arch-name c-string))
(defar seccomp-arch-native unsigned-int)
(defar seccomp-arch-exist int
  (ctx scmp-filter-ctx)
  (arch-token unsigned-int))
(defar seccomp-arch-add int
  (ctx scmp-filter-ctx)
  (arch-token unsigned-int))
(defar seccomp-arch-remove int
  (ctx scmp-filter-ctx)
  (arch-token unsigned-int))
(defar seccomp-load int
  (ctx scmp-filter-ctx))
(defar seccomp-attr-get int
  (ctx scmp-filter-ctx)
  (attr scmp-fltatr)
  (value (* unsigned-int)))
(defar seccomp-attr-set int
  (ctx scmp-filter-ctx)
  (attr scmp-fltatr)
  (value unsigned-int))
(defar seccomp-syscall-resolve-num-arch c-string
  (arch-token unsigned-int)
  (num int))
(defar seccomp-syscall-resolve-name-arch int
  (arch-token unsigned-int)
  (name c-string))
(defar seccomp-syscall-resolve-name-rewrite int
  (arch-token unsigned-int)
  (name c-string))
(defar seccomp-syscall-resolve-name int
  (name c-string))
(defar seccomp-syscall-priority int
  (ctx scmp-filter-ctx)
  (syscall int)
  (priority unsigned-char))

#+nil ;; varargs
(defar seccomp-rule-add int
  (ctx scmp-filter-ctx)
  (action unsigned-int)
  (syscall int)
  (arg-cnt unsigned-int)
  ...)
  
(defar seccomp-rule-add-array int
  (ctx scmp-filter-ctx)
  (action unsigned-int)
  (syscall int)
  (arg-cnt unsigned-int)
  (arg-array (* scmp-arg-cmp)))

#+nil ;; varargs
(defar seccomp-rule-add-exact int
  (ctx scmp-filter-ctx)
  (action unsigned-int)
  (syscall int)
  (arg-cnt unsigned-int))

(defar seccomp-rule-add-exact-array int
  (ctx scmp-filter-ctx)
  (action unsigned-int)
  (syscall int)
  (arg-cnt unsigned-int)
  (arg-array (* scmp-arg-cmp)))

(defar seccomp-notify-alloc int
  (req (* (* seccomp-notif)))
  (resp (* (* seccomp-notif-resp))))

(defar seccomp-notify-free void
  (req (* seccomp-notif))
  (resp (* seccomp-notif-resp)))

(defar seccomp-notify-receive int
  (fd int)
  (req (* seccomp-notif)))

(defar seccomp-notify-respond void
  (fd int)
  (resp (* seccomp-notif-resp)))

(defar seccomp-notify-id-valid int
  (fd int)
  (id unsigned-long))

(defar seccomp-notify-fd int
  (ctx scmp-filter-ctx))

(defar seccomp-export-pfc int
  (ctx scmp-filter-ctx)
  (fd int))

(defar seccomp-export-bpf int
  (ctx scmp-filter-ctx)
  (fd int))

(defar seccomp-export-bpf-mem int
  (ctx scmp-filter-ctx)
  (buf (* t))
  (len (* size-t)))

(defar seccomp-transaction-start int
  (ctx scmp-filter-ctx))

(defar seccomp-transaction-reject void
  (ctx scmp-filter-ctx))

(defar seccomp-transaction-commit int
  (ctx scmp-filter-ctx))

(defar seccomp-precompute int
  (ctx scmp-filter-ctx))
