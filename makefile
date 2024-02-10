### core/makefile
.RECIPEPREFIX := $() 

# after bootstrapping an infra/* image, nu is the default system
# shell but we must inform GNU Make of this change.

# We explicitly set a new configuration file `nu/ci.nu` and load core
# modules so they are available inside rules.
SHELL=/usr/local/bin/nu -I nu/lib/ --config nu/config.nu --env-config nu/env.nu
CARGO_FLAGS?=--release
.PHONY:rust lisp clean test
.stash:;mkdir $@
rust:.stash;
  overlay use $@; version
  cd $@; cargo build $(CARGO_FLAGS)
lisp:.stash;
  overlay use $@; version
  cd $@; lisp build prelude; mv -f prelude.fasl ../$</prelude.core
  cd $@/app/bin; lisp build bin/skel; mv -f skel ../../../$</skel
  cd $@/app/bin; lisp build bin/homer; mv -f homer ../../../$</homer
  cd $@/app/bin; lisp build bin/rdb; mv -f rdb ../../../$</rdb
  cd $@/app/bin; lisp build bin/organ; mv -f organ ../../../$</organ
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
  rm -rf .stash/*
prune:clean;hg clean
