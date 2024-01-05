(in-package :pod)

(defclass live-container (id)
  (app-armor-profile args bounding-caps config common-pid-file created 
   dependencies driver effective-caps exec-ids exit-command graph-driver 
   host-config hostname-path hosts-path image image-name is-infra mount-label 
   mounts name namespace network-settings oci-config-path oci-runtime path pod 
   process-label resolv-conf-path restart-count rootfs size-root-fs size-rw state static-dir)
  (:documentation "A container in the Podman runtime environment."))

(defclass build-container (id)
  (builder imageid imagename containername)
  (:documentation "A container in the Buildah OCI environment."))
