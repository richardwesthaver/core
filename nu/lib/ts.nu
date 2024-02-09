# ts.nu
use std log;
export-env {
  $env.TREE_SITTER_LANGS = [
  commonlisp bash c cpp css 
  go html java javascript 
  jsdoc json python regex 
  rust yaml nu typescript
  kdl
  ];
  $env.TREE_SITTER_REPOS = [[name url];
    [nu "https://github.com/LhKipp/tree-sitter-nu.git"]
    [commonlisp "https://github.com/theHamsta/tree-sitter-commonlisp.git"]
    [yaml "https://github.com/ikatyang/tree-sitter-yaml.git"]
    [cpp "https://github.com/ruricolist/tree-sitter-cpp.git"]
    [kdl "https://github.com/amaanq/tree-sitter-kdl"]
    [bqn "https://github.com/shnarazk/tree-sitter-bqn"]
  ];
}

export def "lang install" [
  ...langs:string
  --output(-o):string
  --prefix(-p):string = "/usr/local"
  --cc:string = "clang"
  --cxx:string = "clang++"
] {
  let out = (if ($output == null) {mktemp -d} else {$output})
  let ext = (if ((uname) == "Darwin") {"dylib"} else {"so"})
  let langs = (if ($langs | is-empty) { $env.TREE_SITTER_LANGS } else { $langs })
  log info "Installing tree-sitter languages..."
  print ($langs | table -i false)
  mkdir $out
  cd $out
  for lang in $langs {
    log info $"installing ($lang) parser"
    let url = (if ($env.TREE_SITTER_REPOS | where name == $lang | is-empty) {
      $"https://github.com/tree-sitter/tree-sitter-($lang)"
    } else { 
      $env.TREE_SITTER_REPOS | where name == $lang | first | $in.url 
    });
    git clone $url $lang
    cd $"($lang)/src"
    if ("scanner.cc" | path exists) {
      log info "found C++ scanner"
      ^$cxx -I. -fPIC scanner.cc -c -lstdc++
      ^$cc -I. -std=c99 -fPIC parser.c -c
      ^$cxx -shared scanner.o parser.o -o $"($prefix)/lib/libtree-sitter-($lang).($ext)"
    } else if ("scanner.c" | path exists) {
      log info "found C scanner"
      ^$cc -I. -std=c99 -fPIC scanner.c -c
      ^$cc -I. -std=c99 -fPIC parser.c -c
      ^$cc -shared scanner.o parser.o -o $"($prefix)/lib/libtree-sitter-($lang).($ext)"
    } else {
      log info "no scanner found, installing parser only"
      ^$cc -I. -std=c99 -fPIC parser.c -c
      ^$cc -shared parser.o -o $"($prefix)/lib/libtree-sitter-($lang).($ext)"
    }
    mkdir $"($prefix)/share/tree-sitter/($lang)"
    cp grammar.json node-types.json $"($prefix)/share/tree-sitter/($lang)"
    log info $"successfully installed ($lang)"
  }
}
