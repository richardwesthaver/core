#!/usr/bin/env -S core --script
#|Packy test util|#
(in-package :user)
(init :log :level :trace)
(load-aliens :tree-sitter :tree-sitter-bash)
(serde (deserialize #p"~/.stash/scratch/network-audio-controller/aur/netaudio-git/PKGBUILD" :pkgbuild) #p"./PKGBUILD.test")
