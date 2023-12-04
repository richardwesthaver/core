;;; Proto
(defpackage :skel/core/proto
  (:use :cl)
  (:export
   ;; proto
   :sk-run
   :sk-new
   :sk-save
   :sk-tangle
   :sk-weave
   :sk-call
   :sk-print
   :sk-load
   :sk-compile
   :rehash-object
   :sk-transform
   :sk-read-file
   :sk-write
   :sk-writeln
   :sk-write-string
   :sk-write-file
   :sk-read-file
   :sk-install-user-config))

(in-package :skel/core/proto)

(defgeneric sk-run (self))
(defgeneric sk-new (self))
(defgeneric sk-save (self))
(defgeneric sk-tangle (self))
(defgeneric sk-weave (self))
(defgeneric sk-call (self))
(defgeneric sk-print (self))
(defgeneric sk-load (self &key &allow-other-keys))
(defgeneric sk-compile (self stream &key &allow-other-keys))
(defgeneric rehash-object (self))
(defgeneric sk-transform (self other &key &allow-other-keys))
;; TODO 2023-09-22: consider a skelfile-writer struct
(defgeneric sk-read-file (self path))
(defgeneric sk-write (self stream))
(defgeneric sk-writeln (self stream))
(defgeneric sk-write-string (self))
(defgeneric sk-write-file (self &key path &allow-other-keys))
(defgeneric sk-install-user-config (self cfg))
