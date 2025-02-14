;;; ublk/srv.lisp --- ublksrv API

;; 

;;; Code:
(in-package :ublk)

(define-alien-type ublksrv-ctrl-dev (struct ublksrv-ctrl-dev))

;; early def
(define-alien-type ublksrv-tgt-info
  (struct ublksrv-tgt-info
          (dev-size unsigned-long-long)
          (tgt-ring-depth unsigned-int)
          (nr-fds unsigned-int)
          (fds (array int #.+ublksrv-tgt-max-fds+))
          (tgt-data (* t))
          (extra-ios unsigned-int)
          (io-data-size unsigned-int)
          (ops (* (struct nil)))
          (iowq-max-workers (array unsigned-int 2))
          (reserved (array unsigned-long 4))))

(define-alien-type ublksrv-dev
  (struct ublksrv-dev
          (tgt ublksrv-tgt-info)))

(define-alien-type ublksrv-queue
    (struct ublksrv-queue
            (q-id int)
            (q-depth int)
            (ring-ptr (* io-uring))
            (dev (* ublksrv-dev))
            (private-data (* t))))

(std:eval-always
  (define-alien-type ublk-io-data
      (struct ublk-io-data
              (tag int)
              (pad unsigned-int)
              (iod (* ublksrv-io-desc))
              (private-data (* t))))
  (define-alien-type ublksrv-handle-io-async-function (function int (* ublksrv-queue) (* ublk-io-data)))
  (define-alien-type ublksrv-tgt-io-done-function (function void (* ublksrv-queue) (* ublk-io-data) (* io-uring-cqe)))
  (define-alien-type ublksrv-handle-event-function (function void (* ublksrv-queue)))
  (define-alien-type ublksrv-handle-io-background-function (function void (* ublksrv-queue) int))
  (define-alien-type ublksrv-usage-for-add-function (function void))
  (define-alien-type ublksrv-init-tgt-function (function int (* ublksrv-dev) int int (array c-string)))
  (define-alien-type ublksrv-deinit-tgt-function (function void (* ublksrv-dev)))
  (define-alien-type ublksrv-alloc-io-buf-function (function (* t) (* ublksrv-queue) (* t) int))
  (define-alien-type ublksrv-idle-function (function void (* ublksrv-queue) boolean))
  (define-alien-type ublksrv-recovery-tgt-function (function int (* ublksrv-dev) int))
  (define-alien-type ublksrv-init-queue-function (function int (* ublksrv-dev) (* (* t))))
  (define-alien-type ublksrv-deinit-queue-function (function void (* ublksrv-queue)))
  (define-alien-type ublksrv-tgt-type 
      (struct ublksrv-tgt-type
              (handle-io-async (* ublksrv-handle-io-async-function))
              (tgt-io-done (* ublksrv-tgt-io-done-function))
              (handle-event (* ublksrv-handle-event-function))
              (handle-io-background (* ublksrv-handle-io-background-function))
              (usage-for-add (* ublksrv-usage-for-add-function))
              (init-tgt (* ublksrv-init-tgt-function))
              (deinit-tgt (* ublksrv-deinit-tgt-function))
              (alloc-io-buf (* ublksrv-alloc-io-buf-function))
              (idle-fn (* ublksrv-idle-function))
              (type int)
              (ublk-flags unsigned)
              (ublksrv-flags unsigned)
              (pad unsigned)
              (name c-string)
              (recovery-tgt (* ublksrv-recovery-tgt-function))
              (init-queue (* ublksrv-init-queue-function))
              (deinit-queue (* ublksrv-deinit-queue-function))
              (reserved (array unsigned-long 5)))))

(define-alien-type ublksrv-dev-data
  (struct ublksrv-dev-data
          (dev-id int)
          (max-io-buf-bytes unsigned)
          (nr-hw-queues unsigned-short)
          (queue-depth unsigned-short)
          (tgt-type c-string)
          (tgt-ops (* (struct ublksrv-tgt-type)))
          (tgt-argc int)
          (tgt-argv (* (c-string)))
          (run-dir c-string)
          (flags unsigned-long)
          (ublksrv-flags unsigned-long)
          (reserved (array unsigned-long 7))))

(define-alien-routine build-user-data (unsigned 64)
  (tag unsigned)
  (op unsigned)
  (tgt-data unsigned)
  (is-target-io unsigned))

(define-alien-routine ublksrv-ctrl-deinit void
  (dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-ctrl-init (* ublksrv-ctrl-dev)
  (data (* ublksrv-dev-data)))

(define-alien-routine ublksrv-ctrl-get-affinity int
  (ctrl-dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-ctrl-add-dev int
  (dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-ctrl-del-dev int
  (dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-ctrl-get-info int
  (dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-ctrl-stop-dev int
  (dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-ctrl-dump void
  (dev (* ublksrv-ctrl-dev))
  (buf (* char)))

(define-alien-routine ublksrv-ctrl-start-dev int
  (ctrl-dev (* ublksrv-ctrl-dev))
  (daemon-pid int))

(define-alien-routine ublksrv-ctrl-set-params int
  (dev (* ublksrv-ctrl-dev))
  (params (* ublk-params)))

(define-alien-routine ublksrv-ctrl-get-params int
  (dev (* ublksrv-ctrl-dev))
  (params (* ublk-params)))

(define-alien-routine ublksrv-ctrl-start-recovery int
  (dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-ctrl-end-recovery int
  (dev (* ublksrv-ctrl-dev))
  (daemon-pid int))

(define-alien-routine ublksrv-ctrl-get-dev-info (* ublksrv-ctrl-dev-info)
  (dev (* ublksrv-ctrl-dev)))
  
(define-alien-routine ublksrv-ctrl-get-features int
  (dev (* ublksrv-ctrl-dev))
  (features (* unsigned-long)))

(define-alien-routine ublksrv-ctrl-get-run-dir c-string
  (dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-ctrl-prep-recovery void
  (dev (* ublksrv-ctrl-dev))
  (tgt-type c-string)
  (tgt-ops (* (struct ublksrv-tgt-type)))
  (recovery-jbuf c-string))

(define-alien-routine ublksrv-ctrl-get-recovery-jbuf c-string
  (dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-is-recovering boolean
  (ctrl-dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-dev-init (* ublksrv-dev)
  (ctrl-dev (* ublksrv-ctrl-dev)))

(define-alien-routine ublksrv-dev-deinit void
  (dev (* ublksrv-dev)))

(define-alien-routine ublksrv-get-ctrl-dev (* ublksrv-ctrl-dev)
  (dev (* ublksrv-dev)))

(define-alien-routine ublksrv-get-pidfile-fd int
  (dev (* ublksrv-dev)))

(define-alien-routine ublksrv-dev-set-cq-depth void
  (dev (* ublksrv-dev))
  (cq-depth int))

(define-alien-routine ublksrv-dev-get-cq-depth int
  (dev (* ublksrv-dev)))

(define-alien-routine ublksrv-apply-oom-protection void)

(define-alien-type ublksrv-tgt-base-json
  (struct ublksrv-tgt-base-json
          (name (array char #.+ublksrv-tgt-name-max-len+))
          (type int)
          (pad unsigned-int)
          (dev-size unsigned-long-long)
          (reserved (array unsigned-long 8))))

(define-alien-routine ublksrv-json-write-dev-info int
  (dev (* ublksrv-ctrl-dev))
  (buf (* char))
  (len int))

(define-alien-routine ublksrv-json-read-dev-info int
  (json-buf (* char))
  (info (* ublksrv-ctrl-dev-info)))

(define-alien-routine ublksrv-json-write-queue-info int
  (dev (* ublksrv-ctrl-dev))
  (jbuf (* char))
  (len int)
  (qid int)
  (ubq-daemon-tid int))

(define-alien-routine ublksrv-json-read-queue-info int
  (jbuf (* char))
  (qid int)
  (tid (* unsigned))
  (affinity-buf (* char))
  (len int))

(define-alien-routine ublksrv-json-read-target-info int
  (jbuf (* char))
  (tgt-buf (* char))
  (len int))

(define-alien-routine ublksrv-json-read-target-str-info int
  (jbuf (* char))
  (len int)
  (name (* char))
  (val (* long)))

(define-alien-routine ublksrv-json-read-target-ulong-info int
  (jbuf (* char))
  (name (* char))
  (val (* long)))

(define-alien-routine ublksrv-json-write-target-str-info int
  (jbuf (* char))
  (len int)
  (name (* char))
  (val (* char)))

(define-alien-routine ublksrv-json-write-target-long-info int
  (jbuf (* char))
  (len int)
  (name (* char))
  (val long))

(define-alien-routine ublksrv-json-write-target-ulong-info int
  (jbuf (* char))
  (len int)
  (name (* char))
  (val unsigned-long))

(define-alien-routine ublksrv-json-dump void
  (jbuf (* char)))

(define-alien-routine ublksrv-json-read-target-base-info int
  (jbuf (* char))
  (tgt (* ublksrv-tgt-base-json)))

(define-alien-routine ublksrv-json-read-params int
  (p (* ublk-params))
  (jbuf (* char)))

(define-alien-routine ublksrv-json-write-params int
  (p (* ublk-params))
  (jbuf (* char))
  (len int))

(define-alien-routine ublksrv-json-dump-params int
  (jbuf (* char)))

(define-alien-routine ublksrv-json-get-length int (jbuf (* char)))

(define-alien-routine ublksrv-io-private-data (* t)
  (q (* ublksrv-queue))
  (tag int))

(define-alien-routine ublksrv-queue-get-io-data (* ublk-io-data)
  (q (* ublksrv-queue))
  (tag int))

(define-alien-routine ublksrv-queue-get-io-buf (* t)
  (q (* ublksrv-queue))
  (tag int))

(define-alien-routine ublksrv-queue-state unsigned-int
  (q (* ublksrv-queue)))

(define-alien-routine ublksrv-queue-init (* ublksrv-queue)
  (dev (* ublksrv-dev))
  (d-id unsigned-short)
  (queue-data (* t)))

(define-alien-routine ublksrv-queue-deinit void
  (q (* ublksrv-queue)))

(define-alien-routine ublksrv-queue-unconsumed-cqes int
  (q (* ublksrv-queue)))

(define-alien-routine ublksrv-queue-handled-event int
  (q (* ublksrv-queue)))

(define-alien-routine ublksrv-queue-send-event int
  (q (* ublksrv-queue)))

(define-alien-routine ublksrv-get-queue (* ublksrv-queue)
  (dev (* ublksrv-dev))
  (q-id int))

(define-alien-routine ublksrv-process-io int
  (q (* ublksrv-queue)))

(define-alien-routine ublksrv-complete-io int
  (q (* ublksrv-queue))
  (tag unsigned)
  (res int))
