# pod.nu
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
  --publish-all(-P)
  --pod
  --rm
] {
  ^podman run $image --name $name
}
