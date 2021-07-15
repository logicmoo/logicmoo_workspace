EMACS ?= emacs
CASK ?= cask

-include makefile-local

ifdef EMACS
EMACS_ENV=EMACS=$(EMACS)
endif


test: install just-test

just-test:
	$(EMACS_ENV) $(CASK) emacs --batch -q \
	--directory=. \
	--load assess-discover.el \
	--eval '(assess-discover-run-and-exit-batch t)'

install:
	$(EMACS_ENV) $(CASK) install

html:
	texi2html persist.texi

push-elpa:
	git push elpa HEAD:externals/persist

pull-elpa:
	git pull elpa externals/persist
