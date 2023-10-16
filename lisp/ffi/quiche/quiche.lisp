(pkg:defpkg :quiche
  (:use :cl :pkg :alien)
  (:export 
   :quiche-version :quiche-config-new))

(in-package :quiche)
(load-shared-object #P"/usr/local/lib/libquiche.so" :dont-save t)

(define-alien-type quiche-config (struct quiche-config))
(define-alien-routine quiche-version c-string)
(define-alien-routine quiche-config-new quiche-config (version unsigned-int))

