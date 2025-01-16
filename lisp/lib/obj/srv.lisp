;;; srv.lisp --- Sans-IO Service Protocol

;; Base Protocol used by any type of state-managed service.

;;; Commentary:

;; As NET/SRV started coming together I realized we need proper isolation of
;; the implementations (UDP, HTTP/S, and EXTernal at TOW) from the core
;; protocol.

;; This package provides as much common functionality as possible and may be
;; further extended by the implementations.

;; Notably, this package does not perform any IO itself, that is totally up to
;; the implementation. The objects in this package consume incoming packets,
;; requests, and events via HANDLE-* functions.

;;;; TODO:

;; request/response? here
;; engine? either here or obj/eng.lisp, build on std/task, std/thread

;; %service-protocol

;; session? prob here or in HTTP/S impl
;; connection? lower-level than session

;; endpoint? closer to service
;; transport? closer to socket
;; routes? build on std/pipe, probably in net/srv/*

;; configs for everything

;;; Code:
(in-package :obj/srv)
