# lisp.nu
use std log
# Lisp compatibility layer for Nushell. Typically targets SBCL compiler.

export-env {
  $env.LISP = sbcl
  $env.LISP_VERSION = null
  $env.QUICKLISP_VERSION = null
  $env.QUICKLISP_DIRECTORY = "/usr/local/share/quicklisp/"
  $env.LISP_DIRECTORY = "/usr/local/share/lisp/"
}

# export extern sbcl [] {}

export def --env version [] { 
  $env.LISP_VERSION = (^$env.LISP --version | split row ' ' | get 1)
  print $env.LISP_VERSION
}

export def "find asd" [name?:string] {
  if ($name == null) {
    ls | where name ends-with .asd | first | $in.name 
    | if $in == null { 
      log error "failed to find system definition (*.asd)" 
    } else { $in }
  } else {
    $name | path parse | update extension asd | path join
  }
}

export def "load asd" [
  --interactive(-i)
  --system(-s):string
  ...body:string
] {
  let asd = (find asd $system)
  let _form = [$"\(progn \(asdf:load-asd \(merge-pathnames \"($asd)\" \(sb-posix:getcwd\)\)\) "]
  let form = (if ($body | is-empty) {
    $_form | first | $in + ')'
  } else {
    ($_form | append $body | append ')'
      | reverse 
      | reduce {|a,b| $a + ' ' + $b})
  })
  log debug $"loading ($asd)"
  if $interactive == true {
    ^$env.LISP --eval $form
  } else {
    ^$env.LISP --noinform --non-interactive --eval $form
  }
}

# Build a Lisp system
export def build [
  system:string
  --interactive(-i)
  --asd(-d):string
] {
  log debug $"building ($system) in ($asd)"
  let form = $"\(ql:quickload :($system)\) \(asdf:make :($system)\)"
  if $interactive == true {
    load asd -s $asd -i $form
  } else {
    load asd -s $asd $form
  }
}

# Check if Quicklisp is installed
export def "quicklisp check" [] {
  ^$env.LISP 
}

# Install Quicklisp
export def "quicklisp install" [path:string="/usr/local/share/quicklisp/", --dist(-d)="quicklisp/latest", --client(-c)="nil"] {
  let file = (mktemp --suffix .lisp)
  http get https://beta.quicklisp.org/quicklisp.lisp | save $file -f
  let form = $"\(quicklisp-quickstart:install :path \"($path)\" :dist-version \"($dist)\" :client-version \"($client)\"\)"
  sbcl --non-interactive --load $file --eval $form
  rm -f $file
}

export def main [...args] {
  ^$env.LISP ...args
}
