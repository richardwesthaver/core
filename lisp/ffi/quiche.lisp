(pkg:defpkg :quiche
  (:use :cl :pkg :alien)
  (:export :quiche-version))
(in-package :quiche)
(load-shared-object #P"/usr/local/lib/libquiche.so")

(define-alien-type quiche-config (struct quiche-config))
(define-alien-routine quiche-config-new quiche-config (version unsigned-int))
(define-alien-routine quiche-version c-string)

