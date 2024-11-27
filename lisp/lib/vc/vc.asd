(defsystem :vc
  :depends-on (:std :cli :obj :net :log :parse)
  :components 
  ((:file "pkg")
   (:file "proto")
   (:file "hg")
   ;; (:module "hg"
   ;;  :components
   ;;  ())
   (:file "git")
   ;; (:module "git"
   ;;  :components
   ;;  ())
   (:file "vc"))
  :in-order-to ((test-op (test-op :vc/tests))))

(defsystem :vc/tests
  :depends-on (:std :rt :vc :io)
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :rt :do-tests :vc)))
