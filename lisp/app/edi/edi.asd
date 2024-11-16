;;; ~/comp/core/lisp/app/edi/edi.asd --- Edi Sytem Definitions
(defsystem :edi
  :depends-on (:std :log)
  :components ((:file "pkg")))
