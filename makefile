### core/makefile
.RECIPEPREFIX := $() 

# after bootstrapping an infra/* image, nu is the default system
# shell but we must inform GNU Make of this change.

# We explicitly set a new configuration file `nu/ci.nu` and load core
# modules so they are available inside rules.
SHELL=/usr/local/bin/nu -I nu/lib/ --config nu/ci.nu 
CARGO_FLAGS:=--release
.PHONY:rust lisp clean test show-env
rust:;
  cd $@; cargo build $(CARGO_FLAGS)
lisp:;
  overlay use $@; version
  cd $@; ls
test:;
  overlay use $@;
clean:;
  cd rust; cargo clean
  cd lisp; rm -rf **/*.fasl
prune:clean;hg clean
show-env:;print $$env
