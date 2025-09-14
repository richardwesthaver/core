;;; listener.lisp --- Rocksdb Event Listener

;; Added: <2025-09-07 Sun>

;;; Code:
(in-package :rocksdb)

;; flushjobinfo
(defar rocksdb-flushjobinfo-cf-name (* char) (info (* rocksdb-flushjobinfo)) (size (* size-t)))
(defar rocksdb-flushjobinfo-file-path (* char) (info (* rocksdb-flushjobinfo)) (size (* size-t)))
(defar rocksdb-flushjobinfo-triggered-writes-slowdown unsigned-char (info (* rocksdb-flushjobinfo)))
(defar rocksdb-flushjobinfo-triggered-writes-stop unsigned-char (info (* rocksdb-flushjobinfo)))
(defar rocksdb-flushjobinfo-largest-seqno unsigned-long (info (* rocksdb-flushjobinfo)))
(defar rocksdb-flushjobinfo-smallest-seqno unsigned-long (info (* rocksdb-flushjobinfo)))
(defar rocksdb-reset-status void (status-ptr (* rocksdb-status-ptr)))

;; compactionjobinfo
(def-with-errptr rocksdb-compactionjobinfo-status (* rocksdb-status-ptr) (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-cf-name (* char) (info (* rocksdb-compactionjobinfo)) (size (* size-t)))
(defar rocksdb-compactionjobinfo-input-files-count size-t (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-input-file-at (* char)
  (info (* rocksdb-compactionjobinfo)) (pos size-t) (size (* size-t)))
(defar rocksdb-compactionjobinfo-output-files-count size-t (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-output-file-at (* char) 
  (info (* rocksdb-compactionjobinfo)) (pos size-t) (size (* size-t)))
(defar rocksdb-compactionjobinfo-elapsed-micros unsigned-long (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-num-corrupt-keys unsigned-long (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-base-input-level int (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-output-level int (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-num-input-files size-t (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-num-input-files-at-output-level size-t (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-input-records unsigned-long (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-output-records unsigned-long (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-total-input-bytes unsigned-long (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-total-output-bytes unsigned-long (info (* rocksdb-compactionjobinfo)))
(defar rocksdb-compactionjobinfo-compaction-reason unsigned-long (info (* rocksdb-compactionjobinfo)))

;; subcompactionjobinfo
(def-with-errptr rocksdb-subcompactionjobinfo-status void (info (* rocksdb-subcompactionjobinfo)))
(defar rocksdb-subcompactionjobinfo-cf-name (* char) (info (* rocksdb-subcompactionjobinfo)) (size (* size-t)))
(defar rocksdb-subcompactionjobinfo-thread-id unsigned-long (info (* rocksdb-subcompactionjobinfo)))
(defar rocksdb-subcompactionjobinfo-base-input-level int (info (* rocksdb-subcompactionjobinfo)))
(defar rocksdb-subcompactionjobinfo-output-level int (info (* rocksdb-subcompactionjobinfo)))

;; externalfileingestioninfo
(defar rocksdb-externalfileingestioninfo-cf-name (* char)
  (info (* rocksdb-externalfileingestioninfo))
  (size (* size-t)))

(defar rocksdb-externalfileingestioninfo-internal-file-path (* char)
  (info (* rocksdb-externalfileingestioninfo))
  (size (* size-t)))

;; writestallinfo
(defar rocksdb-writestallinfo-cf-name (* char) (info (* rocksdb-writestallinfo)) (size (* size-t)))
(defar rocksdb-writestallinfo-cur (* rocksdb-writestallcondition) (info (* rocksdb-writestallinfo)))
(defar rocksdb-writestallinfo-prev (* rocksdb-writestallcondition) (info (* rocksdb-writestallinfo)))

;; memtable
(defar rocksdb-memtableinfo-cf-name (* char) (info (* rocksdb-memtableinfo)) (size (* size-t)))
(defar rocksdb-memtableinfo-first-seqno unsigned-long (info (* rocksdb-memtableinfo)))
(defar rocksdb-memtableinfo-earliest-seqno unsigned-long (info (* rocksdb-memtableinfo)))
(defar rocksdb-memtableinfo-num-entries unsigned-long (info (* rocksdb-memtableinfo)))
(defar rocksdb-memtableinfo-num-deletes unsigned-long (info (* rocksdb-memtableinfo)))

;; callbacks
(define-alien-type on-flush-begin-cb (function void (* t) (* rocksdb) (* rocksdb-flushjobinfo)))
(define-alien-type on-flush-completed-cb (function void (* t) (* rocksdb) (* rocksdb-flushjobinfo)))
(define-alien-type on-compaction-begin-cb (function void (* t) (* rocksdb) (* rocksdb-compactionjobinfo)))
(define-alien-type on-compaction-completed-cb (function void (* t) (* rocksdb) (* rocksdb-compactionjobinfo)))
(define-alien-type on-subcompaction-begin-cb (function void (* t) (* rocksdb-subcompactionjobinfo)))
(define-alien-type on-subcompaction-completed-cb (function void (* t) (* rocksdb-subcompactionjobinfo)))
(define-alien-type on-external-file-ingested-cb (function void (* t) (* rocksdb) (* rocksdb-externalfileingestioninfo)))
(define-alien-type on-background-error-cb (function void (* t) unsigned-int c-string))
(define-alien-type on-stall-conditions-changed-cb (function void (* t) (* rocksdb-writestallinfo)))
(define-alien-type rocksdb-logger-logv-callback (function void (* t) unsigned-int c-string))
(define-alien-type on-memtable-sealed-cb (function void (* t) (* rocksdb-memtableinfo)))

(defmacro rocksdb-info-str (info fn)
  (with-gensyms (str buf size)
    `(with-alien ((,size size-t)
                  (,buf (* char) (,fn ,info (addr ,size))))
       (let ((,str (make-string ,size)))
         (copy-c-string (alien-sap ,buf) ,str)
         ,str))))

(define-alien-callable default-on-flush-begin-cb void
    ((state (* t))
     (db (* rocksdb))
     (info (* rocksdb-flushjobinfo)))
  (std:mumble "beginning flush on ~A (~A)"
              (rocksdb-info-str info rocksdb-flushjobinfo-cf-name)
              (rocksdb-info-str info rocksdb-flushjobinfo-file-path)))

(define-alien-callable default-on-flush-completed-cb void
    ((state (* t))
     (db (* rocksdb))
     (info (* rocksdb-flushjobinfo)))
  (with-alien ((size (* size-t)))
    (std:mumble "completed flush on ~A (~A)" 
                (rocksdb-info-str info rocksdb-flushjobinfo-cf-name)
                (rocksdb-info-str info rocksdb-flushjobinfo-file-path))))

(define-alien-callable default-on-compaction-begin-cb void
    ((state (* t))
     (db (* rocksdb))
     (info (* rocksdb-compactionjobinfo)))
  (std:mumble "beginning compaction on ~A (~A)"
              (rocksdb-info-str info rocksdb-compactionjobinfo-cf-name)
              (rocksdb-compactionjobinfo-input-records info)))

(define-alien-callable default-on-compaction-completed-cb void
    ((state (* t))
     (db (* rocksdb))
     (info (* rocksdb-compactionjobinfo)))
  (std:mumble "completed compaction on ~A (~A)"
              (rocksdb-info-str info rocksdb-compactionjobinfo-cf-name)
              (rocksdb-compactionjobinfo-output-records info)))

(define-alien-callable default-on-subcompaction-begin-cb void
    ((state (* t))
     (info (* rocksdb-subcompactionjobinfo)))
  (std:mumble "beginning subcompaction on ~A"
              (rocksdb-info-str info rocksdb-subcompactionjobinfo-cf-name)))

(define-alien-callable default-on-subcompaction-completed-cb void
    ((state (* t))
     (info (* rocksdb-subcompactionjobinfo)))
  (std:mumble "completed subcompaction on ~A"
              (rocksdb-info-str info rocksdb-subcompactionjobinfo-cf-name)))

(define-alien-callable default-on-external-file-ingested-cb void
    ((state (* t))
     (db (* rocksdb)) 
     (info (* rocksdb-externalfileingestioninfo)))
  (std:mumble "ingesting external file ~A to ~A"
              (rocksdb-info-str info rocksdb-externalfileingestioninfo-internal-file-path)
              (rocksdb-info-str info rocksdb-externalfileingestioninfo-cf-name)))

(define-alien-callable default-on-background-error-cb void 
    ((state (* t))
     (level unsigned-int)
     (msg c-string))
  (std:mumble "background error at level ~A: ~A~&" level msg))

(define-alien-callable default-on-stall-conditions-changed-cb void 
    ((state (* t)) 
     (info (* rocksdb-writestallinfo)))
  (std:mumble "stall conditions changed on ~A"
              (rocksdb-info-str info rocksdb-writestallinfo-cf-name)))

(define-alien-callable default-rocksdb-logger-logv-callback void 
    ((state (* t)) 
     (level unsigned-int) 
     (msg c-string))
  (std:mumble "log.~A: ~A" level msg))

(define-alien-callable default-on-memtable-sealed-cb void 
    ((state (* t)) 
     (info (* rocksdb-memtableinfo)))
  (with-alien ((size size-t)
               (buf (* char) (rocksdb-memtableinfo-cf-name info (addr size))))
    (let ((str (make-string size)))
      (std/alien:copy-c-string (alien-sap buf) str)
      (std:mumble "memtable sealed: ~A." str))))

;; eventlistener
(defar rocksdb-eventlistener-create (* rocksdb-eventlistener)
  (state (* t))
  (destructor (* rocksdb-destructor-function))
  (on-flush-begin (* on-flush-begin-cb))
  (on-flush-completed (* on-flush-completed-cb))
  (on-compaction-begin (* on-compaction-begin-cb))
  (on-compaction-completed (* on-compaction-completed-cb))
  (on-subcompaction-begin (* on-subcompaction-begin-cb))
  (on-subcompaction-completed (* on-subcompaction-completed-cb))
  (on-external-file-ingested (* on-external-file-ingested-cb))
  (on-background-error (* on-background-error-cb))
  (on-stall-conditions-changed (* on-stall-conditions-changed-cb))
  (on-memtable-sealed (* on-memtable-sealed-cb)))

(std:definline default-rocksdb-event-listener ()
  "The default RocksDB event listener used for basic debugging via Lisp. All of
the default callbacks used the MUMBLE function and print to *STANDARD-OUTPUT*."
  (let ((flush-begin (alien-sap (alien-callable-function 'rocksdb-destructor)))
        (flush-completed (alien-sap (alien-callable-function 'default-on-flush-completed-cb)))
        (compaction-begin (alien-sap (alien-callable-function 'default-on-compaction-begin-cb)))
        (compaction-completed (alien-sap (alien-callable-function 'default-on-compaction-completed-cb)))
        (subcompaction-begin (alien-sap (alien-callable-function 'default-on-subcompaction-begin-cb)))
        (subcompaction-completed (alien-sap (alien-callable-function 'default-on-subcompaction-completed-cb)))
        (external-file-ingested (alien-sap (alien-callable-function 'default-on-external-file-ingested-cb)))
        (background-error (alien-sap (alien-callable-function 'default-on-background-error-cb)))
        (stall-conditions-changed (alien-sap (alien-callable-function 'default-on-stall-conditions-changed-cb)))
        (memtable-sealed (alien-sap (alien-callable-function 'default-on-memtable-sealed-cb))))
    (rocksdb-eventlistener-create 
     nil (alien-sap (alien-callable-function 'rocksdb-destructor))
     flush-begin
     flush-completed
     compaction-begin
     compaction-completed
     subcompaction-begin
     subcompaction-completed
     external-file-ingested
     background-error
     stall-conditions-changed
     memtable-sealed)))

(defar rocksdb-eventlistener-destroy void (self (* rocksdb-eventlistener)))
(defar rocksdb-options-add-eventlistener void (opts (* rocksdb-options)) (listener (* rocksdb-eventlistener)))
