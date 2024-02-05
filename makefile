### core/makefile
.RECIPEPREFIX := $() 

# after bootstrapping an infra/* image, nu is the default system
# shell but we must inform GNU Make of this change.

# We explicitly set a new configuration file `nu/ci.nu` and load core
# modules so they are available inside rules.
SHELL=/usr/local/bin/nu -I nu/lib/ --config nu/ci.nu 
CARGO_FLAGS?=--release
.PHONY:rust lisp clean test
rust:;
  overlay use $@; version
  cd $@; cargo build $(CARGO_FLAGS)
lisp:;
  overlay use $@; version
  cd $@; lisp build prelude
box:Containerfile;
  overlay use pod
  pod build -t comp/core
test:;
  overlay use $@;
run:;
  overlay use pod; pod run comp/core
clean:;
  cd rust; cargo clean
  cd lisp; rm -rf **/*.fasl
prune:clean;hg clean
