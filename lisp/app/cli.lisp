(uiop:define-package :app/cli
  (:use :cl :std)
  (:use-reexport
   :app/cli/skel
   :app/cli/organ
   :app/cli/homer
   :app/cli/rdb))
