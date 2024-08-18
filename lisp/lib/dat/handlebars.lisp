;;; handlebars.lisp --- Handlebars Template Format

;; Minimal templating on steroids in Lisp

;;; Commentary

;; handlebars is a popular templating system initially derived from mustache
;; which happened to have some CL bindings:
;; https://github.com/kanru/cl-mustache

;; This package aims to integrate directly with the DAT/HTML package but
;; should be able to be dropped-in to other serde-enabled formats like DAT/XML
;; or DAT/JSON.

;; ref: https://handlebarsjs.com

;;; Code:
(in-package :dat/handlebars)
