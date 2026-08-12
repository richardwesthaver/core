;;; rocksdb/compaction.lisp --- RocksDB Compaction

;; RocksDB Lisp Compaction Filter API

;;; Commentary:

;; compaction filters are like custom GC rules for the database. compactions
;; run in the background and can be configured via the column-family-options
;; or compactionfilterfactory API.

;; ref: https://github.com/facebook/rocksdb/wiki/Compaction-Filter

#|

* RocksDB snapshots do not guarantee to preserve the state of the DB in the
presence of CompactionFilter. Data seen from a snapshot might disappear after
a table file created with a `CompactionFilter` is installed. If you use
snapshots, think twice about whether you want to use `CompactionFilter` and
whether you are using it in a safe way.

* If multithreaded compaction is being used *and* a single CompactionFilter
instance was supplied via Options::compaction_filter, CompactionFilter
methods may be called from different threads concurrently.  The application
must ensure that such calls are thread-safe. If the CompactionFilter was
created by a factory, then it will only ever be used by a single thread that
is doing the table file creation, and this call does not need to be
thread-safe.  However, multiple filters may be in existence and operating
concurrently.

* The key passed to the filtering methods includes the timestamp if
user-defined timestamps are enabled.

* Exceptions MUST NOT propagate out of overridden functions into RocksDB,
because RocksDB is not exception-safe. This could cause undefined behavior
including data loss, unreported corruption, deadlocks, and more.

|#
;;; Code:
(in-package :rocksdb)

(define-alien-type rocksdb-filter-function
    (function unsigned-char 
        (* t) 
        int
      c-string
      size-t
      c-string
      size-t
      (* (array unsigned-char))
      (* size-t)
      (* unsigned-char)))

(define-alien-type rocksdb-create-compaction-filter-function
    (function (* rocksdb-compactionfilter)
        (* t)
        (* rocksdb-compactionfiltercontext)))

(defar rocksdb-compactionfilter-create (* rocksdb-compactionfilter)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (filter (* rocksdb-filter-function))
  (name (* rocksdb-name-function)))

(defar rocksdb-compactionfilter-set-ignore-snapshots void
  (self (* rocksdb-compactionfilter)) (val unsigned-char))

(defar rocksdb-compactionfilter-destroy void
  (self (* rocksdb-compactionfilter)))

;;; Compaction Filter Context
(defar rocksdb-compactionfiltercontext-is-full-compaction unsigned-char
  (context (* rocksdb-compactionfiltercontext)))

(defar rocksdb-compactionfiltercontext-is-manual-compaction unsigned-char
  (context (* rocksdb-compactionfiltercontext)))

;;; Compaction Filter Factory
(defar rocksdb-compactionfilterfactory-create (* rocksdb-compactionfilterfactory)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (creator (* rocksdb-create-compaction-filter-function))
  (name (* rocksdb-name-function)))

(defar rocksdb-compacitonfilterfactory-destroy void
  (factory (* rocksdb-compactionfilterfactory)))

(locally
    (declare (sb-ext:muffle-conditions style-warning))
  (define-alien-callable rocksdb-filter-never unsigned-char
      ((state (* t))
       (level int)
       (key (array unsigned-char))
       (key-length size-t)
       (existing-val (array unsigned-char))
       (existing-val-length size-t)
       (new-val (* (array unsigned-char)))
       (new-val-length (* size-t))
       (value-changed (* unsigned-char)))
    (declare (ignore state level key key-length existing-val existing-val-length new-val new-val-length value-changed))
    0)
(define-alien-callable rocksdb-create-compaction-filter-never (* rocksdb-compactionfilter)
    ((state (* t))
     (context (* rocksdb-compactionfiltercontext)))
  (rocksdb-compactionfilter-create
   state
   (alien-sap (alien-callable-function 'rocksdb-destructor))
   (alien-sap (alien-callable-function 'rocksdb-filter-never))
   (alien-sap (alien-callable-function 'rocksdb-filter-never-name)))))

(define-alien-callable rocksdb-filter-never-name c-string () "core:never")

;;; Remote Compactions

;; see https://github.com/facebook/rocksdb/wiki/Remote-Compaction

(define-alien-type rocksdb-compaction-service-schedule-cb
    (function (* rocksdb-compactionservice-scheduleresponse)
        (* t) ; state
        (* rocksdb-compactionservice-jobinfo) ; info
        (* unsigned-char) ; input
        size-t)) ; input-length

(define-alien-type rocksdb-compaction-service-wait-cb
    (function int
        (* t) ; state
        (* unsigned-char) ; scheduled-job-id
        (* (* unsigned-char)) ; result
        (* size-t))) ; result-len 

(define-alien-type rocksdb-compaction-service-cancel-awaiting-jobs-cb
    (function void
        (* t))) ; state

(define-alien-type rocksdb-compaction-service-on-installation-cb
    (function void
        (* t) ; state
        (* unsigned-char) ; scheduled-job-id
        int)) ; status

(def-with-errptr rocksdb-open-and-compact c-string
  (db-path c-string)
  (output-directory c-string)
  (input (* char))
  (input-len size-t)
  (output-len (* size-t))
  (override-options (* rocksdb-compaction-service-options-override)))

(def-with-errptr rocksdb-open-and-compact-with-options c-string
  (options (* rocksdb-open-and-compact-options))
  (db-path c-string)
  (output-directory c-string)
  (input (* char))
  (input-len size-t)
  (output-len (* size-t))
  (override-options (* rocksdb-compaction-service-options-override)))

(def-with-errptr rocksdb-compactionservice-scheduleresponse-create (* rocksdb-compactionservice-scheduleresponse)
  (scheduled-job-id c-string)
  (status int))

(def-with-errptr rocksdb-compactionservice-scheduleresponse-create-with-status (* rocksdb-compactionservice-scheduleresponse)
  (status int))

(defar rocksdb-compactionservice-scheduleresponse-getstatus int
  (response (* rocksdb-compactionservice-scheduleresponse)))

(defar rocksdb-compactionservice-scheduleresponse-get-schedule-job-id (* char)
  (response (* rocksdb-compactionservice-scheduleresponse))
  (len (* size-t)))

(defar rocksdb-compactionservice-scheduleresponse-t-destroy void
  (response (* rocksdb-compactionservice-scheduleresponse)))

(defar rocksdb-compactionservice-jobinfo-t-get-db-name (* char)
  (info (* rocksdb-compactionservice-jobinfo))
  (len (* size-t)))

(defar rocksdb-compactionservice-jobinfo-t-get-db-id (* char)
  (info (* rocksdb-compactionservice-jobinfo))
  (len (* size-t)))
(defar rocksdb-compactionservice-jobinfo-t-get-db-session-id (* char)
  (info (* rocksdb-compactionservice-jobinfo))
  (len (* size-t)))
(defar rocksdb-compactionservice-jobinfo-t-get-cf-name (* char)
  (info (* rocksdb-compactionservice-jobinfo))
  (len (* size-t)))
(defar rocksdb-compactionservice-jobinfo-t-get-cf-id (unsigned 32)
  (info (* rocksdb-compactionservice-jobinfo)))
(defar rocksdb-compactionservice-jobinfo-t-get-job-id (unsigned 64)
  (info (* rocksdb-compactionservice-jobinfo)))
(defar rocksdb-compactionservice-jobinfo-t-get-priority int
  (info (* rocksdb-compactionservice-jobinfo)))
(defar rocksdb-compactionservice-jobinfo-t-get-compaction-reason int
  (info (* rocksdb-compactionservice-jobinfo)))
(defar rocksdb-compactionservice-jobinfo-t-get-base-input-level int
  (info (* rocksdb-compactionservice-jobinfo)))
(defar rocksdb-compactionservice-jobinfo-t-get-output-level int
  (info (* rocksdb-compactionservice-jobinfo)))
(defar rocksdb-compactionservice-jobinfo-t-is-full-compaction boolean
  (info (* rocksdb-compactionservice-jobinfo)))
(defar rocksdb-compactionservice-jobinfo-t-is-manual-compaction boolean
  (info (* rocksdb-compactionservice-jobinfo)))
(defar rocksdb-compactionservice-jobinfo-t-is-bottommost-level boolean
  (info (* rocksdb-compactionservice-jobinfo)))

(defar rocksdb-compactionservice-create (* rocksdb-compactionservice)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (schedule (* rocksdb-compaction-service-schedule-cb))
  (name c-string)
  (wait (* rocksdb-compaction-service-wait-cb))
  (cancel-awaiting-jobs (* rocksdb-compaction-service-cancel-awaiting-jobs-cb))
  (on-installation (* rocksdb-compaction-service-on-installation-cb)))
