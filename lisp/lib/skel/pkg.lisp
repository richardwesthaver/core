;;; skel/pkg.lisp --- Project Skeletons

;; Project composition library.

;;; Commentary:

;; The SKEL system consists of a core package SKEL/CORE, a required compiler
;; package SKEL/COMP, and some default but optional modules SKEL/DB and
;; SKEL/NET. 

;; The core contains all of the low-level bits and a CLOS API for interacting
;; with SKEL objects. 

;; The compiler package depends on the core and provides primitive compilers
;; for translating SKEL objects into foreign formats. For example, SK-RULE
;; objects may be translated into a corresponding GNU Make Rule. The compiler
;; packages implement the CLOS API defined in the core and may serve as a
;; useful guide for further extending the system yourself.

;; SKEL/DB provides a database interface for individual project caches as well
;; as global storage. SKEL/NET provides a wire protocol and client/server for
;; communication amongst any number of remote hosts.

;; Additionally there is a collection of default extensions which may be
;; toggled off via the SK-CONFIG FEATURES slot:

;; - VIZ provides an API for generating visualizations of SKEL objects

;; - PACKY enables package management and distribution.

;; - POD enables Podman API functionality.

;; - BOX enables QEMU/libvirt features.

;; - DEPLOY enables CI/Deploy features.


;; - BOX 

;;; Code:
(pkg:defpkg :skel/core
  (:use :cl :std)
  (:use-reexport :skel/core/condition :skel/core/proto :skel/core/vars 
   :skel/core/header :skel/core/obj :skel/core/util))

(pkg:defpkg :skel/comp
  (:use :cl :std)
  (:use-reexport :skel/comp/asd :skel/comp/cargo :skel/comp/makefile
   :skel/comp/container :skel/comp/dir-locals :skel/comp/org))

(pkg:defpkg :skel/net
  (:use :cl :std)
  (:use-reexport :skel/net/client :skel/net/server))
