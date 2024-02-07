# vc.nu

# secrets: GITLAB_TOKEN
export-env {
  $env.VC_URL = "https://vc.compiler.company"
  $env.VC_ENDPOINT = $env.VC_URL + "/api/v4/"
  $env.VC_REGISTRY = [[name url];
    ["infra" $"($env.VC_URL)/comp/infra"]
    ["core" $"($env.VC_URL)/comp/core"]]
  $env.VC_PACKY = $env.VC_URL + "/packy/"
}

export def call [
  ...args: string
  --path: string
  --query: string
] {
  http get -H [Authorization $"Bearer ($env.GITLAB_TOKEN)"] $"($env.VC_ENDPOINT)($path)($args|str join)?($query)"
}

# Search files on your GitLab server
export def "query-file" [
  --file: string # file (or path to file if in a subfolder) you want to scan
  --phrase: string # phrase you want to search for
  --branch: string # branch to scan
] {
  let page_size = 100
  let projects = $"($env.VC_URL)/api/v4/projects/"
    # /projects endpoint can return up to $page_size items which is why we need multiple calls to retrieve full list
  let num_of_pages = ((call --path "projects/" --query 'page=1&per_page=1&order_by=id&simple=true'|get id.0|into int) / $page_size|math round)
  seq 1 $num_of_pages|par-each {|page|
    call "projects/" --query $"page=($page)&per_page=($page_size)"|select name id
  }
  |flatten
  |par-each {|repo|
    let payload = (call $repo.id '/repository/files/' $file --query $"ref=($branch)")
    if ($payload|columns|find message|is-empty) {
      $payload
      |get content
      |decode base64
      |lines
      |find $phrase
      |if ($in|length) > 0 {
          echo $"($file) in ($repo.name) repo contains ($phrase) phrase"
        }
    }
  }
}

export def projects [
  --group: string
] {
  if $group != null {
    call "projects/" --query $"group=($group)"
  } else {
    call "projects/"
  }
}

export def ssh-url-for [url: string] {
  #  BUG 2024-01-29: patched in next release
  # $"$env.VC_URL/($repo)" | url parse | update scheme ssh | update username git | url join ## 
  $url | url parse | update scheme 'ssh' | 
  url join | str replace 'ssh://' 'ssh://git@'
}

export def url-for [
  repo:string
  --ssh(-s)
] {
  let url = $"($env.VC_URL)/($repo)"
  $url | if $ssh { ssh-url-for $url } else { $in }
}

export def root [] {
  do {
    do { hg root } | complete 
    | if $in.stdout != "" { $in.stdout } else {
      do { git rev-parse --show-toplevel } | complete
      | if $in.stdout != "" { $in.stdout } else {
        error make { msg: $"directory (pwd) not tracked by VC" }
      }
    }
  }
}

# Print the VC of the current directory.
export def type [path:string="."] {
  do {
    do { hg root } | complete
    | if $in.exit_code == 0 { "hg" } else {
      do { git rev-parse } | complete
      | if $in.exit_code == 0 { "git" } else {
        "none"
      }
    }
  }
}

export def status [] {
  if ('.git/' | path exists) == true {
    git pull origin HEAD
  } else if ('.hg/' | path exists) == true {
    hg pull -u
  } else { error make {msg: $"directory (pwd) not tracked by VC"} }
}

export def "project update" [repo?:string] {
  if $repo != null { cd $repo }
  if ('.git/' | path exists) == true {
    git pull origin HEAD
  } else if ('.hg/' | path exists) == true {
    hg pull -u
  } else { error make {msg: $"directory (pwd) not tracked by VC"} }
}

export def "projects update" [dir?:string] {
  if $dir != null {cd $dir}
  ls | where type == dir | par-each { |it|
    project update $it.name
  }
}

export def "mirror update" [repo?:string] {
  if $repo != null { cd $repo }
  if ('.git/' | path exists) == true {
    git fetch upstream
    git pull upstream HEAD
    git push origin
  } else if ('.hg/' | path exists) == true {
    hg pull -u
    hg push default
  } else { error make {msg: $"directory (pwd) not tracked by VC"} }
}

export def "mirrors update" [dir?:string] {
  if $dir != null {cd $dir}
  ls | where type == dir | par-each { |it|
    mirror update $it.name
  }
}
