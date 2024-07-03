("stdbool.h" "stddef.h" "stdint.h" "sys/time.h" "btrfsutil.h" "btrfs/version.h" "btrfs/ioctl.h")

( ;; util
 (:structure btrfs-util-subvolume-info 
             ("struct btrfs_util_subvolume_info"
              (unsigned-long id "uint64_t" "id")
              (unsigned-long parent-id "uint64_t" "parent_id")
              (unsigned-long dir-id "uint64_t" "dir_id")
              (unsigned-long flags "uint64_t" "flags")
              ((array char) uuid "uint8_t" "uuid")
              ((array char) parent-uuid "uint8_t" "parent_uuid")
              ((array char) received-uuid "uint8_t" "received_uuid")
              (unsigned-long generation "uint64_t" "generation")
              (unsigned-long ctransid "uint64_t" "ctransid")
              (unsigned-long otransid "uint64_t" "otransid")
              (unsigned-long rtransid "uint64_t" "rtransid")
              ((* t) ctime "struct timespec" "ctime")
              ((* t) otime "struct timespec" "otime")
              ((* t) stime "struct timespec" "stime")
              ((* t) rtime "struct timespec" "rtime"))
             nil t)
 (:integer +btrfs-lib-major+ "BTRFS_LIB_MAJOR" t t)
 (:integer +btrfs-lib-minor+ "BTRFS_LIB_MINOR" t t)
 (:integer +btrfs-lib-patchlevel+ "BTRFS_LIB_PATCHLEVEL" t t)
 (:integer +btrfs-lib-version+ "BTRFS_LIB_VERSION" t t)
 (:integer +btrfs-subvol-rdonly+ "BTRFS_SUBVOL_RDONLY" t t)
 (:integer +btrfs-subvol-qgroup-inherit+ "BTRFS_SUBVOL_QGROUP_INHERIT" t t)
 (:integer +btrfs-device-spec-by-id+ "BTRFS_DEVICE_SPEC_BY_ID" t t)
 (:integer +btrfs-subvol-spec-by-id+ "BTRFS_SUBVOL_SPEC_BY_ID" t t)
 (:integer +btrfs-vol-arg-v2-flags-supported+ "BTRFS_VOL_ARG_V2_FLAGS_SUPPORTED" t t)
 (:integer +btrfs-fsize-size+ "BTRFS_FSIZE_SIZE" t t)
 (:integer +btrfs-fsize-size+ "BTRFS_UUID_SIZE" t t)
 (:integer +btrfs-qgroup-inherit-set-limits+ "BTRFS_QGROUP_INHERIT_SET_LIMITS" t t)
 ;; ioctl
 (:structure btrfs-qgroup-limit 
             ("struct btrfs_qgroup_limit"
              ((unsigned 64) flags "__u64" "flags")
              ((unsigned 64) max-referenced "__u64" "max_referenced")
              ((unsigned 64) max-exclusive "__u64" "max_exclusive")
              ((unsigned 64) rsv-referenced "__u64" "rsv_referenced")
              ((unsigned 64) rsv-exclusive "__u64" "rsv_exclusive"))
             nil t))

