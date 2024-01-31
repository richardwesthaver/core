# lisp.nu

# Lisp compatibility layer for Nushell. Typically targets SBCL compiler.

export-env {
  $env.LISP = `/usr/local/bin/sbcl`
}

export def version [] { 
  ^$env.LISP --version | split row ' ' | get 1
}

export def build [] {

}

export def "build core" [] {

}

export def main [] {

}
