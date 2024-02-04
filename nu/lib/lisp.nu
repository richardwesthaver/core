# lisp.nu

# Lisp compatibility layer for Nushell. Typically targets SBCL compiler.

export-env {
  $env.LISP = sbcl
  $env.QUICKLISP_DIST_VERSION = "quicklisp/latest"
  $env.QUICKLISP_CLIENT_VERSION = nil
}

export def version [] { 
  ^$env.LISP --version | split row ' ' | get 1
}

export def load-asd [system?:string] {
  let asd = (if ($system == null) {ls | where name ends-with .asd | first | $in.name} else {$"($system).asd"})
  let form = $"\(asdf:load-asd \(merge-pathnames \"($asd)\" \(sb-posix:getcwd\)\)\)"
  print $asd $form
  ^$env.LISP --eval $form
}

export def "build core" [] {

}

export def "quicklisp install" [path:string="/usr/local/share/quicklisp/", --dist(-d)="quicklisp/latest", --client(-c)="nil"] {
  let file = (mktemp --suffix .lisp)
  http get https://beta.quicklisp.org/quicklisp.lisp | save $file -f
  let form = $"\(quicklisp-quickstart:install :path \"($path)\" :dist-version \"($dist)\" :client-version \"($client)\"\)"
  sbcl --non-interactive --load $file --eval $form
  rm -f $file
}

export def main [] {

}
