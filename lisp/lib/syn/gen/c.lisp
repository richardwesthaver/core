;;; c.lisp --- C Code Generator

;; Lisp -> C

;; Commentary:

;; There are quite a few C Code Generators in the Common Lisp ecosystem, and
;; of course ECL which is itsel a source-to-source Lisp implementation which
;; targets C. This one is probably closest to c-mera.

;; ref: https://github.com/kiselgra/c-mera

;; ref: https://selgrad.org/publications/2014_els_SLWLS.pdf

;; ref: https://selgrad.org/publications/2017_els_LSS.pdf

;;; Code:
(in-package :syn/gen/c)

