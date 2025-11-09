;;; box/archiso.lisp --- archiso installation interface

;;

;;; Code:
(in-package :box/archiso)

#| default config
{
    "__separator__": null,
    "additional-repositories": [],
    "archinstall-language": "English",
    "audio_config": null,
    "bootloader": "Systemd-boot",
    "config_version": "2.6.0",
    "debug": false,
    "disk_config": {
        "config_type": "manual_partitioning",
        "device_modifications": [
            {
                "device": "/dev/sda",
                "partitions": [
                    {
                        "btrfs": [],
                        "flags": [
                            "Boot"
                        ],
                        "fs_type": "fat32",
                        "length": {
                            "sector_size": null,
                            "total_size": null,
                            "unit": "B",
                            "value": 99982592
                        },
                        "mount_options": [],
                        "mountpoint": "/boot",
                        "obj_id": "369f31a8-2781-4d6b-96e7-75680552b7c9",
                        "start": {
                            "sector_size": {
                                "sector_size": null,
                                "total_size": null,
                                "unit": "B",
                                "value": 512
                            },
                            "total_size": null,
                            "unit": "sectors",
                            "value": 34
                        },
                        "status": "create",
                        "type": "primary"
                    },
                    {
                        "btrfs": [],
                        "flags": [],
                        "fs_type": "fat32",
                        "length": {
                            "sector_size": null,
                            "total_size": null,
                            "unit": "B",
                            "value": 100000000
                        },
                        "mount_options": [],
                        "mountpoint": "/efi",
                        "obj_id": "13cf2c96-8b0f-4ade-abaa-c530be589aad",
                        "start": {
                            "sector_size": {
                                "sector_size": null,
                                "total_size": null,
                                "unit": "B",
                                "value": 512
                            },
                            "total_size": {
                                "sector_size": null,
                                "total_size": null,
                                "unit": "B",
                                "value": 16106127360
                            },
                            "unit": "MB",
                            "value": 100
                        },
                        "status": "create",
                        "type": "primary"
                    },
                    {
                        "btrfs": [],
                        "flags": [],
                        "fs_type": "ext4",
                        "length": {
                            "sector_size": null,
                            "total_size": null,
                            "unit": "B",
                            "value": 15805127360
                        },
                        "mount_options": [],
                        "mountpoint": "/",
                        "obj_id": "3e75d045-21a4-429d-897e-8ec19a006e8b",
                        "start": {
                            "sector_size": {
                                "sector_size": null,
                                "total_size": null,
                                "unit": "B",
                                "value": 512
                            },
                            "total_size": {
                                "sector_size": null,
                                "total_size": null,
                                "unit": "B",
                                "value": 16106127360
                            },
                            "unit": "MB",
                            "value": 301
                        },
                        "status": "create",
                        "type": "primary"
                    }
                ],
                "wipe": false
            }
        ]
    },
    "disk_encryption": {
        "encryption_type": "luks",
        "partitions": [
            "3e75d045-21a4-429d-897e-8ec19a006e8b"
        ]
    },
    "hostname": "archlinux",
    "kernels": [
        "linux"
    ],
    "locale_config": {
        "kb_layout": "us",
        "sys_enc": "UTF-8",
        "sys_lang": "en_US"
    },
    "mirror_config": {
        "custom_mirrors": [],
        "mirror_regions": {
            "Sweden": [
                "https://mirror.osbeck.com/archlinux/$repo/os/$arch",
                "https://mirror.bahnhof.net/pub/archlinux/$repo/os/$arch",
                "https://ftp.myrveln.se/pub/linux/archlinux/$repo/os/$arch",
                "https://ftp.lysator.liu.se/pub/archlinux/$repo/os/$arch",
                "https://ftp.ludd.ltu.se/mirrors/archlinux/$repo/os/$arch",
                "https://ftp.acc.umu.se/mirror/archlinux/$repo/os/$arch",
                "http://mirror.bahnhof.net/pub/archlinux/$repo/os/$arch",
                "http://ftpmirror.infania.net/mirror/archlinux/$repo/os/$arch",
                "http://ftp.myrveln.se/pub/linux/archlinux/$repo/os/$arch",
                "http://ftp.lysator.liu.se/pub/archlinux/$repo/os/$arch",
                "http://ftp.acc.umu.se/mirror/archlinux/$repo/os/$arch"
            ]
        }
    },
    "network_config": {},
    "no_pkg_lookups": false,
    "ntp": true,
    "offline": false,
    "packages": [],
    "parallel downloads": 0,
    "profile_config": null,
    "save_config": null,
    "script": "guided",
    "silent": false,
    "swap": true,
    "timezone": "UTC",
    "version": "2.6.0"
}
|#

#|
(dat/proto:serialize
'(("__separator__" NIL) ("additional-repositories" NIL)
 ("archinstall-language" "English") ("audio_config" NIL)
 ("bootloader" "Systemd-boot") ("config_version" "2.6.0")
 ("debug" NIL) ("disk_config" nil)
 ("disk_encryption" nil)
 ("hostname" "archlinux") ("kernels" ("linux"))
 ("locale_config" nil)
 ("mirror_config" nil)
 ("network_config" nil)
 ("no_pkg_lookups" NIL) ("ntp" T) ("offline" NIL) ("packages" NIL)
 ("parallel downloads" 0) ("profile_config" NIL)
 ("save_config" NIL) ("script" "guided") ("silent" NIL) ("swap" T)
  ("timezone" "UTC") ("version" "2.6.0"))
:json)
|#

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
;;;_. Types
(defvar *archiso-releng-directory* #P"/usr/share/archiso/configs/releng/")
(defvar *archiso-baseline-directory* #P"/usr/share/archiso/configs/baseline/")

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

;;;_. Variables
(defvar *archiso-config*)

(defvar *archiso-creds*)

(defvar *default-archiso-profile* :releng)

;;;_. Config
(defconfig archiso-config (box-config)
  ((arch :initform "x86_64" :type string)
   (hostname :initform "box" :type string)
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
    (when ast (load-ast cfg))
    cfg))

(defun format-archiso-file-permissions (lst)
  (mapcar (lambda (x) (format nil "[~S]=~S" (car x) (cdr x))) lst))

(defmethod build ((self archiso-config) &key path)
  "Build an Archiso profile directory at PATH given the configuration SELF."
  (with-directory (ensure-directories-exist (directory-path path))
    (let ((airootfs (ensure-directories-exist "airootfs/"))
          (efiboot (ensure-directories-exist "efiboot/"))
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
      (with-directory airootfs
        (ensure-directories-exist "etc/"))
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
      (with-open-file (bootstrap (format nil "packages.~A" (slot-value self 'arch)) :direction :output :if-exists :supersede)
        (dolist (p (slot-value self 'packages))
          (write-line (string-downcase p) bootstrap)))
      (uiop:copy-file (slot-value self 'pacman-conf) "pacman.conf")
      (with-open-file (profiledef "profiledef.sh" :direction :output :if-exists :supersede)
        (write-line "#!/usr/bin/env bash" profiledef)
        (write-line "# shellcheck disable=SC2634" profiledef)
        (with-slots (iso-name iso-label iso-publisher iso-application
                     iso-version install-dir buildmodes bootmodes
                     pacman-conf airootfs-image-type airootfs-image-tool-options bootstrap-tarball-compression
                     file-permissions) self
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
          (format profiledef "file_permissions=~A" (format-archiso-file-permissions file-permissions)))))))

;;;_. CLI
(defun mkarchiso (profile-dir 
                  &key config install-dir out-dir work-dir
                       name label publisher cert gpg mbox modes packages
                       delete verbose (output t))
  (sb-ext:run-program 
   (cli:find-exe "mkarchiso") 
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
