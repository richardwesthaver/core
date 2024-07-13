;;; leb128.lisp --- Little-Endian Base 128 Variable Encoding

;; (U)LEB128 encoders

;;; Commentary:

;; ref: https://en.wikipedia.org/wiki/LEB128
;; opt: https://arxiv.org/abs/1503.07387 VByte
;; opt: https://arxiv.org/pdf/1709.08990 VByte streaming

;;; Code:
(in-package :std/num)
