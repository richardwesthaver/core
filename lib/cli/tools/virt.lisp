;;; virt.lisp --- Container and VM CLI Tools

;; 

;;; Code:
(in-package :cli/tools/virt)

(define-cli-tool :xvfb-run (args &key (input t) (output t))
  (let ((proc (sb-ext:run-program *xvfb-run* args :output output :input input)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (xvfb-run-error "XVFB-RUN command failed: ~A ~A" *xvfb-run* args))))

(define-cli-tool :podman (&rest args)
  (let ((proc (sb-ext:run-program *podman* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (podman-error "PODMAN command failed: ~A ~A" *podman* (or args "")))))

(defun podman-machine-upgrade ()
  (run-podman "machine" "ssh" "sudo rpm-ostree upgrade --check"))

;; podman system service --time=0 unix:///tmp/podman.sock (local-socket)
;; podman system service --time=0 tcp://localhost:8888 (inet-socket :stream :tcp)
(defun start-podman-service (addr &optional (protocol :unix) (time 0))
  "Start the Libpod API on ADDR over PROTO which is either :TCP or :UNIX."
  (declare ((member :unix :tcp) protocol))
  (run-podman "system"
              "service"
              (format nil "~(~a~)://~a" protocol addr)
              (format nil "--time=~a" time)))

(define-cli-tool :buildah (args &key (output t) input)
  (let ((proc (sb-ext:run-program *buildah* args :output output :input input)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (buildah-error "BUILDAH command failed: ~A ~A" *buildah* args))))

(define-cli-tool :mkarchiso (args &key (output t) error)
  (let ((proc (sb-ext:run-program *mkarchiso* (or args nil) :output output :error error)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (mkarchiso-error "MKARCHISO command failed: ~A ~A" *mkarchiso* (or args "")))))

(defun mkarchiso (profile-dir 
                  &key config install-dir out-dir work-dir
                       name label publisher cert gpg mbox modes packages
                       delete verbose (output t))
  (cli/tools/virt::run-mkarchiso
   `(,@(when config `("-C" ,config))
     ,@(when install-dir `("-D" ,install-dir))
     ,@(when out-dir `("-o" ,out-dir))
     ,@(when work-dir `("-w" ,work-dir))
     ,@(when name `("-A" ,name))
     ,@(when label `("-L" ,label))
     ,@(when publisher `("-P" ,publisher))
     ,@(when cert `("-c" ,cert))
     ,@(when gpg `("-g" ,gpg))
     ,@(when mbox `("-G" ,mbox))
     ,@(when modes `("-m" ,@modes))
     ,@(when packages `("-p" ,@packages))
     ,@(when delete '("-r"))
     ,@(when verbose '("-v"))
     ,profile-dir)
   :error output
   :output output))

(defun run-archiso (iso &key (uefi t) additional-iso vnc secure-boot disk accessibility (output t))
  "Run the given ISO path with qemu."
  (sb-ext:run-program 
   (cli:find-exe "run_archiso")
   `("-i" ,iso
     ,@(when uefi '("-u"))
     ,@(when additional-iso `("-c" ,additional-iso))
     ,@(when vnc '("-v"))
     ,@(when secure-boot '("-s"))
     ,@(when disk '("-d"))
     ,@(when accessibility '("-a")))
   :output output))

(defun find-qemu-exe (&optional (arch (machine-target-name)))
  (find-exe (concatenate 'string "qemu-system-" arch)))

(defvar *qemu* (find-qemu-exe))

(define-cli-tool :qemu (&rest args)
  (let ((proc (sb-ext:run-program *qemu* (or args nil) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (qemu-error "QEMU command failed: ~A ~A" *qemu* (or args "")))))
  
(deftype qemu-img-cmd () 
  '(member :amend :bench :bitmap 
    :check :commit :compare :convert 
    :create :dd :info :map 
    :measure :snapshot :rebase :resize))

(deftype qemu-img-compression-type () '(member :zstd :zlib))
(deftype qemu-img-format () '(member :qcow :qcow2 :raw))

(defun qemu-img (cmd &rest args)
  (check-type cmd qemu-img-cmd)
  (let ((proc (sb-ext:run-program #1=(find-exe "qemu-img") (cons (string-downcase cmd) (or args nil)) :output t)))
    (unless (eq 0 (sb-ext:process-exit-code proc))
      (qemu-error "QEMU-IMG command failed: ~A ~A" #1# (or args "")))))

(defun qemu-system (&key (arch (machine-target-name))
                         image
                         hda hdb hdc hdd
                         mem cpu boot accel
                         drive
                         usbdevice
                         cdrom
                         vnc
                         (output t))
  "Run the given ISO path with qemu."
  (sb-ext:run-program 
   (find-qemu-exe arch)
   `(,@(when drive `("-drive" ,drive))
     ,@(when vnc `("-vnc" ,vnc))
     ,@(when cdrom `("-cdrom" ,cdrom))
     ,@(when hda `("-hda" ,hda))
     ,@(when hdb `("-hdb" ,hdb))
     ,@(when hdc `("-hdb" ,hdc))
     ,@(when hdd `("-hdb" ,hdd))
     ,@(when boot `("-boot" ,boot))
     ,@(when mem `("-m" ,mem))
     ,@(when cpu `("-cpu" ,cpu))
     ,@(when usbdevice `("-usbdevice" ,usbdevice))
     ,@(when accel `("-accel" ,accel))
     ,@(when image `(,image)))
   :output output
   :error output))
