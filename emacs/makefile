DOTEMACS:=$(HOME)/.emacs.d

.PHONY: c

install:$(wildcard *.el) lib
	cp -rf $^ $(DOTEMACS)/

clean:;rm -rf $(DOTEMACS)/{*.el,lisp,site-lisp}
