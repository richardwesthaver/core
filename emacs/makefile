DOTEMACS:=$(HOME)/.emacs.d
install:$(wildcard *.el) $(DOTEMACS)
	cp -rf $< lib $(DOTEMACS)/
$(DOTEMACS):;mkdir -p $(DOTEMACS)
.PHONY: c
clean:;rm -rf $(DOTEMACS)/{*.el,lisp,site-lisp}
