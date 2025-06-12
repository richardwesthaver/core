;;; box/tests.lisp --- Box tests

;;

;;; Code:
(defpackage :box/tests
  (:use :cl :rt :box :box/archiso :sb-bsd-sockets)
  (:import-from :std :in-readtable)
  (:export
   #:*archiso-json*))

(in-package :box/tests)
(defsuite :box)
(in-suite :box)
(in-readtable :std)
(defparameter *archiso-json*
  #"{
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
}"#)

(deftest archiso ()
  (is *archiso-json*))
