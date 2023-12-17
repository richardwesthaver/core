(require :sb-capstone)

(defpackage :pwn/core
  (:use :cl :std :obj :net))

(defpackage :pwn/diz
  (:use :cl :std :obj :sb-capstone :pwn/core))
