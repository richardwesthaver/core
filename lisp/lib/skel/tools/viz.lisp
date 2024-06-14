;;; skel/tools/viz.lisp --- skel object visualizer

;; Skel Visualization extensions

;;; Commentary:

;; One of our goals with this package is to extend the
;; introspectability of the Lisp debugger outside of Lisp and onto a
;; graphic medium.

;; We have several visualization backends in mind:

;; - web :: interactive 3d/2d graph, probably based on d3.js, prefer wasm
;; - emacs :: skv.el
;; - svg :: configurable vector graphic backend
;; - txt :: text-based, simplified output -- utf-8 or ascii
;; - native  :: native backend - look into widgets, mcclim, gtk4

;;; Code:
(in-package :skel/tools/viz)
