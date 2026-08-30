#!/usr/bin/env -S core --control-stack-size 32 --script
#|Store Utility - Test and inspect RDB Stores.|#
(open-store (make-instance 'rdb-store :spec (rdb::rdb-temp-spec "abc")))
