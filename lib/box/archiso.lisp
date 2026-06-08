;;; box/archiso.lisp --- archiso installation interface

;;

;;; Commentary:

;;; Code:
(in-package :box)
(in-readtable :std)
#|
profile/
├── airootfs/
├── efiboot/
├── syslinux/
├── grub/
├── bootstrap_packages.arch
├── packages.arch
├── pacman.conf
└── profiledef.sh
|#
;;; Types
(deftype airootfs-image-type () '(member :squashfs :ext4+squashfs :erofs))
(deftype archiso-buildmode () '(member :bootstrap :iso :netboot))
(deftype archiso-bootmode () 
  '(member 
    :bios.syslinux.mbr
    :bios.syslinux.eltorito
    :uefi-ia32.grub.esp
    :uefi.ia32.grub.eltorito
    :uefi-x64.grub.esp
    :uefi-x64.grub.eltorito
    :uefi-ia32.systemd-boot.esp
    :uefi-ia32.systemd-boot.eltorito
    :uefi-x64.systemd-boot.esp
    :uefi-x64.systemd-boot.eltorito))

;;; Variables
(defvar *archiso-baseline-directory* #P"/usr/share/archiso/configs/baseline/")
(defvar *archiso-releng-directory* #P"/usr/share/archiso/configs/releng/")

;;; Config
(defconfig archiso-config (box-config)
  ((target :initform *machine-target*)
   (hostname :initform "box" :type string :accessor name)
   (iso-name :initform "archlinux")
   (iso-label :initform "ARCH_$(date --date=\"@${SOURCE_DATE_EPOCH:-$(date +%s)}\" +%Y%m)")
   (iso-publisher :initform "Arch Linux <https://archlinux.org>")
   (iso-application :initform "Arch Linux Live/Rescue DVD")
   (iso-version :initform "$(date --date=\"@${SOURCE_DATE_EPOCH:-$(date +%s)}\" +%Y.%m.%d)")
   (install-dir :initform "arch")
   (buildmodes :initform '(:iso))
   (bootmodes :initform '(:bios.syslinux :uefi.systemd-boot))
   (pacman-conf :initform (merge-pathnames "pacman.conf" *archiso-baseline-directory*))
   (airootfs-image-type :initform "squashfs")
   (airootfs-image-tool-options :initform '("-comp" "xz" "-Xbcj" "x86" "-b" "1M" "-Xdict-size" "1M"))
   (bootstrap-tarball-compression :initform '("zstd" "-c" "-T0" "--auto-threads=logical" "--long" "-19"))
   (file-permissions :initform '(("/etc/shadow" . "0:0:400")))
   (no-pkg-lookups :initform nil :type boolean)
   (packages :initform nil)
   (bootstrap-packages :initform nil)
   (airootfs :initform nil :documentation "Path to a directory containing files to be copied into this box.")))

(defmethod make-config ((fmt (eql :archiso)) &rest args &key ast &allow-other-keys)
  (let ((cfg (apply 'make-instance 'archiso-config args)))
    (when ast 
      (load-ast cfg)
      (setf (ast cfg) nil))
    cfg))

(defmethod load-config ((fmt (eql :archiso)) (from pathname) &key build)
  (load-config :box from :type :archiso :build build))

(defmethod load-ast :after ((self archiso-config))
  (with-slots (packages bootstrap-packages) self
    (setf packages (mapcar #'string-downcase packages)
          bootstrap-packages (mapcar #'string-downcase bootstrap-packages)
          (ast self) nil)
    self))

(defun format-archiso-file-permissions (lst)
  ;; file permissions are an alist
  (mapcar (lambda (x) (format nil "[~S]=~S" (car x) (cdr x))) lst))

(defmethod build ((self archiso-config) &key (path (merge-pathnames (name self) *stash*)))
  "Build an Archiso profile directory at PATH given the configuration SELF."
  (let ((dir (ensure-directories-exist (directory-path path))))
    (with-directory dir
      (let ((efiboot (ensure-directories-exist "efiboot/"))
            ;; (grub (ensure-directories-exist "grub/"))
            (syslinux (ensure-directories-exist "syslinux/")))
        (with-directory efiboot
          (ensure-directories-exist "loader/entries/")
          (with-open-file (loader "loader/loader.conf" :direction :output :if-exists :supersede)
            (write-line "timeout 3" loader)
            (write-line "default 01-archiso-linux.conf" loader))
          (with-open-file (entry "loader/entries/01-archiso-linux.conf" :direction :output :if-exists :supersede)
            (write-line "title   Arch Linux (%ARCH%, UEFI)" entry)
            (write-line "linux   /%INSTALL_DIR%/boot/%ARCH%/vmlinuz-linux" entry)
            (write-line "initrd  /%INSTALL_DIR%/boot/%ARCH%/initramfs-linux.img" entry)
            (write-line "options archisobasedir=%INSTALL_DIR% archisosearchuuid=%ARCHISO_UUID%" entry)))
        (with-directory syslinux
          (with-open-file (sys "syslinux.cfg" :direction :output :if-exists :supersede)
            (write-line "SERIAL 0 115200" sys)
            (write-line "UI menu.c32" sys)
            (write-line "MENU TITLE Arch Linux" sys)
            (write-line "MENU CLEAR" sys)
            (write-line "DEFAULT arch" sys)
            (write-line "TIMEOUT 30" sys)
            (write-line "INCLUDE syslinux-linux.cfg" sys))
          (with-open-file (syslin "syslinux-linux.cfg" :direction :output :if-exists :supersede)
            (write-line "LABEL arch" syslin)
            (write-line "MENU LABEL Arch Linux (%ARCH%, BIOS)" syslin)
            (write-line "LINUX /%INSTALL_DIR%/boot/%ARCH%/vmlinuz-linux" syslin)
            (write-line "INITRD /%INSTALL_DIR%/boot/%ARCH%/initramfs-linux.img" syslin)
            (write-line "APPEND archisobasedir=%INSTALL_DIR% archisosearchuuid=%ARCHISO_UUID%" syslin)))
        (with-open-file (bootstrap "bootstrap_packages" :direction :output :if-exists :supersede)
          (dolist (p (slot-value self 'bootstrap-packages))
            (write-line (string-downcase p) bootstrap)))
        (with-open-file (bootstrap (format nil "packages.~A" (machine-target-name (slot-value self 'target))) :direction :output :if-exists :supersede)
          (dolist (p (slot-value self 'packages))
            (write-line (string-downcase p) bootstrap)))
        (uiop:copy-file (slot-value self 'pacman-conf) "pacman.conf")
        (with-open-file (profiledef "profiledef.sh" :direction :output :if-exists :supersede)
          (write-line "#!/usr/bin/env bash" profiledef)
          ;; (write-line "# shellcheck disable=SC2634" profiledef)
          (with-slots (iso-name iso-label iso-publisher iso-application
                       iso-version install-dir buildmodes bootmodes
                       pacman-conf airootfs-image-type airootfs-image-tool-options bootstrap-tarball-compression
                       file-permissions airootfs)
              self
            (format profiledef "iso_name=~S~%" iso-name)
            (format profiledef "iso_label=\"~A\"~%" iso-label)
            (format profiledef "iso_publisher=~S~%" iso-publisher)
            (format profiledef "iso_application=~S~%" iso-application)
            (format profiledef "iso_version=\"~A\"~%" iso-version)
            (format profiledef "install_dir=~S~%" install-dir)
            (format profiledef "buildmodes=~S~%" (mapcar 'string-downcase buildmodes))
            (format profiledef "bootmodes=~S~%" (mapcar 'string-downcase bootmodes))
            (format profiledef "pacman_conf=\"pacman.conf\"~%")
            (format profiledef "airootfs_image_type=~S~%" (string-downcase airootfs-image-type))
            (format profiledef "airootfs_image_tool_options=~S~%" airootfs-image-tool-options)
            (format profiledef "bootstrap_tarball_compression=~S~%" bootstrap-tarball-compression)
            (format profiledef "file_permissions=~A" (format-archiso-file-permissions file-permissions))
            ;; FIX 2026-04-17: 
            (when airootfs (sb-ext:run-program "/bin/cp" (list "-rf" airootfs "airootfs")))))))))
