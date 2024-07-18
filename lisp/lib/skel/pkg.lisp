;;; skel/pkg.lisp --- skeletons

;; Project composition library.

;;; Commentary:

;; The SKEL system consists of a core package and a compiler package -
;; SKEL/CORE and SKEL/COMP respectively. The core contains all of the
;; low-level bits and a CLOS API for interacting with SKEL
;; objects. The compiler package depends on the core and provides
;; primitive compilers for translating SKEL objects into foreign
;; formats. For example, SK-RULE objects may be translated into a
;; corresponding GNU Make Rule. The compiler packages implement the
;; CLOS API defined in the core and may serve as a useful guide for
;; further extending the system yourself.

;; There are some built-in extensions available in addition to the
;; core and compiler - SKEL/VIZ provides an API for generating
;; visualizations of SKEL objects, and SKEL/DEPLOY introduces CI,
;; Release and packaging features.

;;; TODO: 

;;  IMPL 2024-02-12: viz

;;  IMPL 2024-02-12: deploy

;;  IMPL 2024-02-12: ext api

;; 

;;; Code:
(pkg:defpkg :skel/core
  (:use :cl :std)
  (:use-reexport :skel/core/err :skel/core/types :skel/core/proto
   :skel/core/vars :skel/core/header :skel/core/obj :skel/core/util
   :skel/core/vm :dat/sxp))

(pkg:defpkg :skel/comp
  (:use :cl :std)
  (:use-reexport :skel/comp/asd :skel/comp/cargo :skel/comp/makefile
   :skel/comp/container :skel/comp/dir-locals :skel/comp/org))

(pkg:defpkg :skel
  (:nicknames :sk)
  (:use :cl :std)
  (:use-reexport :skel/core :skel/comp))

(pkg:defpkg :sk-user
  (:use :cl :std :std-user :cl-user :log :net :sb-debug :sb-ext :net/proto/dns :cli/tools/sbcl)
  (:use :skel :skel/core :skel/comp))
