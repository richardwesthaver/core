### core/makefile
.RECIPEPREFIX := $() #
# after bootstrapping an infra/* image, nu is the default system shell
SHELL=/usr/local/bin/nu -I nu/lib --config nu/ci.nu -e 'use nu/lib/ *'
CARGO_FLAGS:=--release
.PHONY:rust lisp clean test
rust:;
  cd $@; cargo build $(CARGO_FLAGS)
lisp:;
  cd $@; ls
clean:;
  cd rust; cargo clean
  cd lisp; rm -rf **/*.fasl
purge:clean;hg clean
