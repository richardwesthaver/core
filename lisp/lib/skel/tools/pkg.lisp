#+gui
(defpkg :skel/tools/viz
  (:use :cl :std :skel :dat/dot :obj/graph)
  (:export :sk-view))

#+deploy
(defpkg :skel/tools/deploy
  (:use :cl :std :skel)
  (:export))
