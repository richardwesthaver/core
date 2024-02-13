;;; skel/tools/deploy.lisp --- skel deploy tool

;; Skel deployment extensions

;;; Commentary:

;; By 'deployment', we mean the final stage of production in our
;; project pipeline, where artifacts are generated for
;; end-users. Deployment can be covered on a per-project basis using
;; the skel build system, but for large product bundles which ship
;; multiple project pipelines this package will have the superior DX.

;;; Code:
(in-package :skel/deploy)
