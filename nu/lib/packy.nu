# packy.nu
use vc.nu
export-env {
  $env.STASH = "/usr/local/stash/"
  $env.STORE = "/usr/local/store/"
}

export def "db init" [] {
  stor create -t packy -c { name: str, path: str, origin: str, upstream: str, timestamp: datetime }
}

export def "db insert" [name: string, path: directory, origin: string, upstream: string] {
  stor insert -t packy -d {
    name: $name, 
    path: $path,
    origin: $origin, 
    upstream: $upstream, 
  }
}

export def "db clear" [] {
  stor delete -t packy
}

# export the internal packy db as a JSON document
export def "db export" [] {

}

export def "db import" [dir?:directory=.] {
  let $dir = ($dir | path expand)
  cd $dir
  let vc_type = (vc type)
  if $vc_type == 'git' {
    db insert ($dir | path basename) $dir (git remote get-url origin) (git remote get-url upstream)
  } else if $vc_type == 'hg' {
    db insert ($dir | path basename) $dir (hg path default) (hg path upstream)
  } else { error make {msg: $"directory (pwd) not tracked by VC"} }
}

export def list [] {
  stor open | query db "select * from packy"
}

export def clone [name: string] {
  cd $env.STASH
  let origin = (stor open 
      | query db $'select origin from packy where name = "($name)"'
      | get 0 | get origin
      | into string)
  print $"cloning ($name) from ($origin)..."
  git pull $origin $name
  stor update -t packy -w $"name = ($name)" -u {path: ($env.STASH + $name)}
}

export def "get index" [name:string] {
  http get $"https://packy.compiler.company/($name).json"
  | dfr into-df
  | table -e -i false
}
