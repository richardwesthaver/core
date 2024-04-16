;;; app.asd --- application library
(defsystem :app
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on 
  (:uiop :cl-ppcre :std :skel :organ :rdb :cli :log
   :bin/skel :bin/organ :bin/homer
   :web/index :web/dash))
