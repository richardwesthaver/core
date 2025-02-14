(in-package :pod)

(defclass live-image (id) 
  (annotations architecture author comment config created digest graph-driver 
   healthcheck history labels manifest-type names-history os parent repo-digests 
   repo-tags root-fs size user version virtual-size))

(defclass build-image (id)
  (type fromimage fromimagedigest groupadd config manifest container containerid 
   mountpoint processlabel mountlabel imageannotations imagecreatedby))

(defclass build-image-item (id)
  (names digest created size readonly history))
