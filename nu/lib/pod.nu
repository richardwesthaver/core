# pod.nu

# By default we run podman rootless:
# - https://github.com/containers/podman/blob/main/docs/tutorials/rootless_tutorial.md
# - https://github.com/containers/podman/blob/main/rootless.md

# - install slirp4netns
# - /etc/subuid and /etc/subgid config

export def "enable user" [user?:string] {
  if ($user != null) {su $user}
  systemctl enable --user --now podman
}
export def "machine upgrade" [] {
  podman machine ssh 'sudo rpm-ostree upgrade'
}

export def "service start" [socket?:string] {
  let uid = (id -u)
  let socket = (if $socket == null { $"unix:///run/user/($uid)/podman.sock" } else { $socket })
  podman system service --time=0 $socket
}

export def build [name?:string, --tag(-t):string, --no-cache(-n)] {
  let cf = (if ($name == null) { "Containerfile" } else { $"Containerfile.($name)" })
  let no_cache = (if $no_cache == null { "" } else { "--no-cache" })
  if $tag == null {
    podman build -f $cf $no_cache
  } else {
    podman build -f $cf $no_cache -t $tag
  }
}

export def run [
  image:string="comp/infra/box"
  name:string="box"
  --volume(-v):directory
  --interactive(-i)
  --tty(-t)
  --publish(-p)
  --replace(-r)
  --publish-all(-P)
  --pod
  --rm
] {
  let replace = (if ($replace == true) {"--replace"})
  let volume = (if ($volume != null) {$"--volume ($volume)"})
  let interactive = (if ($interactive == true) {"--interactive"})
  let tty = (if ($tty == true) {"--tty"})
  let args = [$replace --name $name $volume $interactive $tty $image] | filter {$in != null}
  ^podman run ...$args
}
