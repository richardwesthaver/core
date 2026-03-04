#!/bin/core --script
;; A dummy command script for interactive testing
(load-system :cli)
(load-commands :skel)
(print-table (list-commands))
(print-table (list-command-types))
