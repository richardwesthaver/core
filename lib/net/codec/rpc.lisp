;;; rpc.lisp --- RPC Protocols

;; Remote Procedure Calls

;;; Commentary:

;; First we must support JSON-RPC, which enables LSP. Then we will use that as
;; a basis for future lispy wire protocols.

;;; Code:
(in-package :net/codec/rpc)
