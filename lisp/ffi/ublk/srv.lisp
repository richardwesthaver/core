;;; ublk/srv.lisp --- ublksrv API

;; 

;;; Code:
(in-package :ublk)

(define-alien-type ublksrv-ctrl-dev (struct ublksrv-ctrl-dev))

(define-alien-type ublksrv-queue
    (struct ublksrv-queue
            (q-id int)
            (q-depth int)
            (ring-ptr (* io-uring))
            (dev (* ublksrv-dev))
            (private-data (* t))))

(define-alien-type ublk-io-data
  (struct ublk-io-data
          (tag int)
          (pad unsigned-int)
          (iod (* ublksrv-io-desc))
          (private-data (* t))))

;; TODO 2024-09-29: add all of these as callbacks :C
;; (define-alien-type ublksrv-tgt-type 
;;   (struct ublksrv-tgt-type
;;           (handle-io-async (function int (* ublksrv-queue) (* ublk-io-data)))
;;           (tgt-io-done (function void (* ublksrv-queue) (* ublk-io-data) (* io-uring-cqe)))
;;           (handle-event (function void (* ublksrv-queue)))
;;           (handle-io-background (function void (* ublksrv-queue) int))
;;           (usage-for-add (function void))
;;           (init-tgt (function int (* ublksrv-dev) int int (array c-string)))
;;           (deinit-tgt (function void (* ublksrv-dev)))
;;           (alloc-io-buf (function (* t) (* ublksrv-queue) (* t) int))
;;           (idle-fn (function void (* ublksrv-queue) bool))
;;           (type int)
;;           (ublk-flags unsigned)
;;           (ublksrv-flags unsigned)
;;           (pad unsigned)
;;           (name c-string)
;;           (recovery-tgt (function int (* ublksrv-dev) int))
;;           (init-queue (function int (* ublksrv-queue) (* (* t))))
;;           (deinit-queue (function void (* ublksrv-queue)))
;;           (reserved (array unsigned-long 5))))

(define-alien-type ublksrv-dev-data
  (struct ublksrv-dev-data
          (dev-id int)
          (max-io-buf-bytes unsigned)
          (nr-hw-queues unsigned-short)
          (queue-depth unsigned-short)
          (tgt-type c-string)
          (tgt-ops (* ublksrv-tgt-type))
          (tgt-argc int)
          (tgt-argv (* (c-string)))
          (run-dir c-string)
          (flags unsigned-long)
          (ublksrv-flags unsigned-long)
          (reserved (array unsigned-long 7))))

(define-alien-type ublksrv-tgt-info
  (struct ublksrv-tgt-info
          (dev-size unsigned-long-long)
          (tgt-ring-depth unsigned-int)
          (nr-fds unsigned-int)
          (fds (array int #.+ublksrv-tgt-max-fds+))
          (tgt-data (* t))
          (extra-ios unsigned-int)
          (io-data-size unsigned-int)
          (ops (* ublksrv-tgt-type))
          (iowq-max-workers (array unsigned-int 2))
          (reserved (array unsigned-long 4))))

(define-alien-type ublksrv-dev
  (struct ublksrv-dev
          (tgt ublksrv-tgt-info)))

(define-alien-routine build-user-data (unsigned 64)
  (tag unsigned)
  (op unsigned)
  (tgt-data unsigned)
  (is-target-io unsigned))

(define-alien-routine ublksrv-ctrl-deinit void
  (dev (* ublksrv-ctrl-dev)))
