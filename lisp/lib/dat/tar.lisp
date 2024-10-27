;;; tar.lisp --- Tarballs

;; Unix Tape Archive Formats.

;;; Commentary:

;; wiki: https://en.wikipedia.org/wiki/Tar_(computing)
;; gnu-tar: https://www.gnu.org/software/tar/manual/html_node/Standard.html

;; ustar: https://wiki.osdev.org/USTAR

;; USTAR is the widely-available POSIX standard - PAX never really took off
;; and CPIO is all but dead.

;; impl: https://github.com/froydnj/archive
;; impl: https://gitlab.common-lisp.net/cl-tar

;; rust impl: https://github.com/alexcrichton/tar-rs

;;; Code:
(in-package :dat/tar)

;;; Vars
(defvar *tar-block-bytes* 512)

(defvar *tar-record-blocks* 20)

(defvar *tar-record-bytes* (* *tar-block-bytes* *tar-record-blocks*))

;;; Conditions
(deferror tar-error () () (:auto t))

;;; Objects
(defclass archive () ())
(defclass tar-archive (archive) ())
(defclass tar-entry () ())
