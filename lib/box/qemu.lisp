;;; qemu.lisp --- QEMU

;; QEMU support for Common Lisp

;;; Commentary:

;; For now we merely want to be able to configure, build and launch images.

;;; Code:
(in-package :box)

(defconfig qemu-image-config (box-config) 
  ((format :initform :qcow2 :type qemu-img-format)
   (filename  :initform (string (gensym "box")))
   (compression :initform :zstd)
   (size :initform"100M")))

(defmethod make-config ((fmt (eql :qemu-image)) &rest args &key ast &allow-other-keys)
  (let ((cfg (apply 'make-instance 'qemu-image-config args)))
    (when ast (load-ast cfg))
    cfg))

(defmethod load-config ((fmt (eql :qemu-image)) (from pathname) &key build)
  (load-config :box from :type :qemu-image :build build))

(defmethod build ((self qemu-image-config) &key (path *stash*))
  (with-directory (ensure-directories-exist (directory-path path))
    (with-slots (filename format compression size) self
      (apply 'qemu-img :create 
             `(,@(when compression `(,(format nil "-ocompression_type=~A" (string-downcase compression))))
               ,@(when format `("-f" ,(string-downcase format)))
               ,(namestring filename)
               ,@(when size `(,size)))))))

(defconfig qemu-system-config (box-config)
  ((target :initform *machine-target*)
   (image :initform nil)
   machine
   accel
   vmport
   dump-guest-core
   mem-merge
   aes-key-wrap
   dea-key-wrap
   nvdimm
   memory-encryption
   hmat
   spcr
   aux-ram-share
   memory-backend
   cpu
   smp
   numa
   global
   boot
   mem
   mem-path
   mem-prealloc
   language
   audio
   audiodev
   device
   name
   uuid
   ;; block
   fda
   fdb
   hda
   hdb
   hdc
   hdd
   cdrom
   blockdev
   drive
   mtdblock
   sd
   snapshot
   fsdev
   virtfs
   iscsi
   ;; usb
   usb
   usbdevice
   ;; display
   display
   nographic
   spice
   vga
   full-screen
   vnc
   ;; network
   nic
   netdev
   net
   ;; chardev
   chardev
   ;; tpm
   tpmdev
   ;; boot/kernel
   bios
   pflash
   kernel
   shim
   append
   initrd
   dtb
   ;; debug/expert
   serial
   parallel
   monitor
   qmp
   qmp-pretty
   mon
   debugcon
   pidfile
   preconfig
   no-cpu-startup
   overcommit
   gdb
   log
   logfile
   seed
   enable-kvm
   no-reboot
   no-shutdown
   loadvm
   daemonize
   rtc
   option-rom
   icount
   echr
   nodefaults
   semihosting
   sandbox
   no-user-config
   trace
   plugin
   run-with
   msg
   dump-vmstate
   enable-sync-profile
   perfmap
   objects))

(defvar *qemu-system-slots*
  (mapcar (lambda (x) (slot-definition-name x))
          (class-direct-slots (find-class 'qemu-system-config))))

(defmethod make-config ((fmt (eql :qemu-system)) &rest args &key ast &allow-other-keys)
  (let ((cfg (apply 'make-instance 'qemu-system-config args)))
    (when ast (load-ast cfg))
    cfg))

(defmethod load-config ((fmt (eql :qemu-system)) (from pathname) &key build)
  (load-config :box from :type :qemu-system :build build))

(defmethod load-ast :after ((self qemu-system-config))
  (with-slots (image) self
    (when (consp image) (setf (slot-value self 'image) (make-config :qemu-image :ast image)))
    (setf (ast self) nil)
    self))

(defmethod build ((self qemu-system-config) &key (path (merge-pathnames (name self) *stash*)))
  (with-directory (ensure-directories-exist (directory-path path))
    ;; create the image
    (build (slot-value self 'image) :path path)
    self))
