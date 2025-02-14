;;; multiformat/pkg.lisp --- Self-describing values

;; Multiformat Lisp support

;;; Commentary:

;; ref: https://github.com/multiformats/multiformats

;;; Code:
(defpackage :dat/multiaddr
  (:use :cl :std))
(defpackage :dat/multibase
  (:use :cl :std))
(defpackage :dat/multicodec
  (:use :cl :std))
(defpackage :dat/multihash
  (:use :cl :std))
(defpackage :dat/multikey
  (:use :cl :std))

