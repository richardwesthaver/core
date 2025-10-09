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
;;; Types
(defvar *archiso-releng-directory* #P"/usr/share/archiso/configs/releng/")
(defvar *archiso-baseline-directory* #P"/usr/share/archiso/configs/baseline/")

(deftype airootfs-image-type () '(member :squashfs :ext4+squashfs :erofs))
(deftype archiso-build-mode () '(member :bootstrap :iso :netboot))
(deftype archiso-boot-mode () 
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

;;; Vars
(defvar *archiso-config*)

(defvar *archiso-creds*)

(defvar *default-archiso-profile* :releng)

;; TODO 2024-05-31: 
;;; Config
(defconfig archiso-config (box-config)
  ((config-version :initform "2.6.0" :type string)
   (hostname :type string)
   (kernels :initform '("linux") :type list)
   locale-config
   mirror-config
   network-config
   (no-pkg-lookups :initform nil :type boolean)
   (ntp :initform t :type boolean)
   network
   (offline :initform nil :type boolean)
   packages
   (archinstall-language :initform "English" :type string)
   (bootloader :initform "Systemd-boot" :type string)
   (debug :initform nil :type boolean)
   parallel-downloads
   disk-config
   disk-encryption
   profile-config
   save-config
   audio-config
   (additional-repositories :initform nil :type list)
   script
   silent
   (swap :initform t :type boolean)
   timezone
   (version :initform "2.6.0" :type string)))

(defmethod make-config ((fmt (eql :archiso)) &rest args &key ast &allow-other-keys)
  (let ((cfg (apply 'make-instance 'archiso-config args)))
    (when ast (load-ast cfg))
    cfg))

;;; CLI
(defun mkarchiso (profile-dir 
                  &key config install-dir out-dir work-dir
                       name label publisher cert gpg mbox modes packages
                       delete verbose output)
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
   :output output))

(defun run-archiso (iso &key (uefi t) additional-iso vnc secure-boot disk accessibility (output t))
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

