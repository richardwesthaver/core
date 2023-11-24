(uiop:define-package :nlp/pkg
  (:nicknames :nlp)
  (:use-reexport
   :nlp/data
   :nlp/tokenize
   :nlp/doc
   :nlp/stem/porter
   :nlp/textrank
   :nlp/dbscan
   :nlp/section))
