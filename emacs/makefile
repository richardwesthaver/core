DOTEMACS:=$(HOME)/.emacs.d
install:$(wildcard *.el) lib
	mkdir -pv $(DOTEMACS)
	cp -rf $^ $(DOTEMACS)/
.PHONY: c
clean:;rm -rf $(DOTEMACS)/{*.el,lisp,site-lisp}
