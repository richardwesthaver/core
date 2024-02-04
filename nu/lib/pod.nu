# pod.nu
export def "machine upgrade" [] {
  podman machine ssh 'sudo rpm-ostree upgrade'
}

export def "service start" [socket?:string] {
  let uid = (id -u)
  let socket = (if $socket == null { $"unix:///run/user/($uid)/podman.sock" } else { $socket })
  podman system service --time=0 $socket
}

export def build [name?:string, --tag(-t):string] {
  let cf = (if ($name == null) { "Containerfile" } else { $"Containerfile.($name)" })
  if ($tag == null) {
    podman build -f $cf
  } else {
    podman build -f $cf -t $tag
  }
}
