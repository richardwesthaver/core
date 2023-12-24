;;; lib/dat/xml.lisp --- XML Data Format

;; based on the re-implementation of https://github.com/rpgoldman/xmls

;; our nodes are called XML-NODE and inherit from OBJ/TREE:NODE.

;; XMLS:NODE-NAME == OBJ/TREE:NODE-KEY

;;; Code:
(in-package :dat/xml)
