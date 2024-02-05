# lisp.nu
use std log
# Lisp compatibility layer for Nushell. Typically targets SBCL compiler.

export-env {
  $env.LISP = sbcl
  $env.LISP_VERSION = null
}

# export extern sbcl [] {}

export def --env version [] { 
  $env.LISP_VERSION = (^$env.LISP --version | split row ' ' | get 1)
  print $env.LISP_VERSION
}

export def "find asd" [name?:string] {
  if ($name == null) {
    ls | where name ends-with .asd | first | $in.name 
  } else {
    $"($name).asd"
  }
}

export def "load asd" [...body:string, --system(-s):string] {
  let asd = (find asd $system)
  let _form = [$"\(progn \(asdf:load-asd \(merge-pathnames \"($asd)\" \(sb-posix:getcwd\)\)\) "]
  let form = (if ($body | is-empty) {
    $_form | first | $in + ')'
  } else {
    ($_form | append $body | append ')'
      | reverse 
      | reduce {|a,b| $a + ' ' + $b})
  })
  log info $"loading (if $system != null {$system} else {'?'}) from ($asd)"
  ^$env.LISP --eval $form
}

# Build a Lisp system
export def build [system?:string] {
  log info $"building ($system)"
  load asd $"\(asdf:make :($system)\)" -s $system
}

# Check if Quicklisp is installed
export def "quicklisp check" [] {

}

# Install Quicklisp
export def "quicklisp install" [path:string="/usr/local/share/quicklisp/", --dist(-d)="quicklisp/latest", --client(-c)="nil"] {
  let file = (mktemp --suffix .lisp)
  http get https://beta.quicklisp.org/quicklisp.lisp | save $file -f
  let form = $"\(quicklisp-quickstart:install :path \"($path)\" :dist-version \"($dist)\" :client-version \"($client)\"\)"
  sbcl --non-interactive --load $file --eval $form
  rm -f $file
}

export def main [] {

}
