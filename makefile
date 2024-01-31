### core/makefile
.RECIPEPREFIX := $() #
# after bootstrapping an infra/* image, nu is the default system shell
SHELL=/usr/local/bin/nu -I nu/lib --config nu/ci.nu
CARGO_FLAGS:=--release
.PHONY:rust lisp
rust:;
  cd $@; cargo build $(CARGO_FLAGS)
lisp:;
  cd $@; ls
  
