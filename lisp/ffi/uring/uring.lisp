;;; uring/uring.lisp --- top-level interface

;;

;;; Code:
(in-package :uring)

;; (defconstant +io-syscall-setup+ nr-io-uring-setup) ;425
;; (defconstant +io-syscall-register+ nr-io-uring-register) ;426
;; (defconstant +io-syscall-enter+ nr-io-uring-enter) ;427

(defstruct io-memory-map
  (sq-mmap nil :type mmapped-region)
  (sqe-mmap nil :type mmapped-region)
  (cq-mmap nil :type mmapped-region))

(defstruct io-parameters (params nil :type io-uring-params))

;; io-uring instance
(defstruct io-uring
  (sq nil :type submission-queue)
  (cq nil :type completion-queue)
  (fd nil :type sb-posix:file-descriptor) ;; owned fd
  (params nil :type io-parameters) ;; TODO io-params
  (memory nil :type io-memory-map))

(defstruct io-uring-builder
  (dontfork nil :type boolean)
  (params nil :type io-uring-params))

;;; Syscalls
;; register, setup, enter
