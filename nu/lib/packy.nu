# packy.nu
use vc.nu
export-env {
  $env.STASH = "/usr/local/stash/"
  $env.STORE = "/usr/local/store/"
}

export def "db init" [] {
  stor create -t packy -c { name: str, path: str, origin: str, upstream: str, timestamp: datetime }
}

export def "db insert" [name: string, origin: string, upstream: string] {
  stor insert -t packy -d {
    name: $name, 
    origin: $origin, 
    upstream: $upstream, 
  }
}

export def "db clear" [] {
  stor delete -t packy
}

export def list [] {
  stor open | query db "select * from packy" | table -i false
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
