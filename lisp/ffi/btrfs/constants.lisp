("stdbool.h" "stddef.h" "stdint.h" "sys/time.h" "btrfsutil.h")

((:enum btrfs-util-error
        ((btrfs-util-ok "BTRFS_UTIL_OK")
	 (btrfs-util-error-stop-iteration "BTRFS_UTIL_ERROR_STOP_ITERATION")
	 (btrfs-util-error-no-memory "BTRFS_UTIL_ERROR_NO_MEMORY")
	 (btrfs-util-error-invalid-argument "BTRFS_UTIL_ERROR_INVALID_ARGUMENT")
	 (btrfs-util-error-not-btrfs "BTRFS_UTIL_ERROR_NOT_BTRFS")
	 (btrfs-util-error-not-subvolume "BTRFS_UTIL_ERROR_NOT_SUBVOLUME")
	 (btrfs-util-error-subvolume-not-found "BTRFS_UTIL_ERROR_SUBVOLUME_NOT_FOUND")
         (btrfs-util-error-open-failed "BTRFS_UTIL_ERROR_OPEN_FAILED")
	 (btrfs-util-error-rmdir-failed "BTRFS_UTIL_ERROR_RMDIR_FAILED")
	 (btrfs-util-error-unlink-failed "BTRFS_UTIL_ERROR_UNLINK_FAILED")
         (btrfs-util-error-stat-failed "BTRFS_UTIL_ERROR_STAT_FAILED")
	 (btrfs-util-error-statfs-failed "BTRFS_UTIL_ERROR_STATFS_FAILED")
	 (btrfs-util-error-search-failed "BTRFS_UTIL_ERROR_SEARCH_FAILED")
	 (btrfs-util-error-ino-lookup-failed "BTRFS_UTIL_ERROR_INO_LOOKUP_FAILED")
	 (btrfs-util-error-subvol-getflags-failed "BTRFS_UTIL_ERROR_SUBVOL_GETFLAGS_FAILED")
	 (btrfs-util-error-subvol-setflags-failed "BTRFS_UTIL_ERROR_SUBVOL_SETFLAGS_FAILED")
	 (btrfs-util-error-subvol-create-failed "BTRFS_UTIL_ERROR_SUBVOL_CREATE_FAILED")
	 (btrfs-util-error-snap-create-failed "BTRFS_UTIL_ERROR_SNAP_CREATE_FAILED")
	 (btrfs-util-error-snap-destroy-failed "BTRFS_UTIL_ERROR_SNAP_DESTROY_FAILED")
	 (btrfs-util-error-default-subvol-failed "BTRFS_UTIL_ERROR_DEFAULT_SUBVOL_FAILED")
	 (btrfs-util-error-sync-failed "BTRFS_UTIL_ERROR_SYNC_FAILED")
	 (btrfs-util-error-start-sync-failed "BTRFS_UTIL_ERROR_START_SYNC_FAILED")
	 (btrfs-util-error-wait-sync-failed "BTRFS_UTIL_ERROR_WAIT_SYNC_FAILED")
	 (btrfs-util-error-get-subvol-info-failed "BTRFS_UTIL_ERROR_GET_SUBVOL_INFO_FAILED")
	 (btrfs-util-error-get-subvol-rootref-failed "BTRFS_UTIL_ERROR_GET_SUBVOL_ROOTREF_FAILED")
	 (btrfs-util-error-ino-lookup-user-failed "BTRFS_UTIL_ERROR_INO_LOOKUP_USER_FAILED")
	 (btrfs-util-error-fs-info-failed "BTRFS_UTIL_ERROR_FS_INFO_FAILED")))

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
              ((* t) rtime "struct timespec" "rtime"))))

;; (:structure btrfs-qgroup-limit 
;;             ("struct btrfs_qgroup_limit"
;;              ((unsigned-int 64) flags "__u64" "flags")
;;              ((unsigned-int 64) max-referenced "__u64" "max_referenced")
;;              ((unsigned-int 64) max-exclusive "__u64" "max_exclusive")
;;              ((unsigned-int 64) rsv-referenced "__u64" "rsv_referenced")
;;              ((unsigned-int 64) rsv-exclusive "__u64" "rsv_exclusive")
;;              ))

