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

;; - VIEW provides an API for generating visualizations of SKEL objects

;; - PACKY enables package management and distribution.

;; - POD enables Podman API functionality.

;; - BOX enables QEMU/libvirt features.

;; - DEPLOY enables CI/Deploy features.

;;; Code:
(pkg:defpkg :skel/core
  (:nicknames :sk-core)
  (:use :cl :std)
  (:import-from :ast :*keep-ast*)
  (:use-reexport :skel/core/proto :skel/core/int
   :skel/core/header :skel/core/obj :skel/core/util :skel/core/db :skel/core/log))

(pkg:defpkg :skel/comp
  (:nicknames :sk-comp)
  (:use :cl :std)
  (:use-reexport :skel/comp/asd :skel/comp/cargo :skel/comp/makefile
   :skel/comp/container :skel/comp/dir-locals :skel/comp/org))

(pkg:defpkg :skel/cli
  (:nicknames :sk-cli)
  (:use :cl :std :log :skel/core :sb-ext :cli/clap)
  (:export :*skel-cli* :sk-shell))

(defpackage :skel/net/core
  (:nicknames :sk-net-core)
  (:use :cl :log :std 
   :net/core :net/proto/dns :net/codec/tlv :skel/core/proto 
   :skel/core/obj :net/udp :net/tcp :obj/id 
   :skel/core/db :net/srv/udp
   :skel/core/log
   :dat/proto :dat/sxp :dat/json)
  (:export
   #:*skel-client-port-range*
   #:*skel-port*
   #:*skel-service-port*
   #:*default-skel-service-port*))

(defpackage :skel/net/client
  (:nicknames :sk-client)
  (:use :cl :std :net :skel/net/core)
  (:export))

(defpackage :skel/net/server
  (:nicknames :sk-server)
  (:use :cl :std :net/srv/udp :net/srv/http :sk-net-core :log :skel/core/log :srv)
  (:export))

(pkg:defpkg :skel/net
  (:nicknames :sk-net)
  (:use :cl :std)
  (:use-reexport :skel/net/client :skel/net/server))

(defpackage :skel/srv
  (:use :cl :std :db 
   :store :build :config :skel/core/db 
   :skel/core :skel/core/log :skel/net :net/srv/udp :net/srv/http :srv)
  (:export #:sk-service))

(defpackage :skel/infer
  (:use :cl :std :db :skel/core :skel/srv :dat :nlp :id :config :vc :srv)
  (:export
   #:sk-inference-engine
   #:sk-inference-service
   #:sk-inferred-project))
        
