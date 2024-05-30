(defpackage :skel/ext/asdf
  (:use :cl :std :skel)
  (:export))

(defpackage :skel/ext/net
  (:use :cl :std :skel :net/sans-io :dat/proto :obj/id)
  (:export))

;; requires clouseau
(defpackage :skel/ext/inspect
  (:use :cl :std :skel)
  (:export :sk-inspect))

(defpackage :skel/ext/krypt
  (:use :cl :std :skel :krypt)
  (:export))

(defpackage :skel/ext/packy
  (:use :cl :std :skel :packy)
  (:export))

(defpackage :skel/ext/asdf
  (:use :cl :std :skel :asdf/system)
  (:export))

(defpackage :skel/ext/pod
  (:use :cl :std :skel :pod)
  (:export))

(defpackage :skel/ext/box
  (:use :cl :std :skel :box)
  (:export))
