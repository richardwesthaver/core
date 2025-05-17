;;; dat.lisp --- DAT API

;; 

;;; Code:
(pkg:defpkg :dat
  (:use :cl :std)
  (:use-reexport :dat/proto :dat/csv :dat/arff
   :dat/toml :dat/json :dat/sxp :dat/xml
   :dat/qrcode :dat/midi :dat/svg :dat/dot
   :dat/tar :dat/css :dat/html :dat/base64 
   :dat/gif :dat/mime :dat/parquet :dat/ini))
