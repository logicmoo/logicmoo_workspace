EMACS = emacs
EMACS_FLAGS = -q

check:
	$(EMACS) $(EMACS_FLAGS) -batch -l webfeeder.el -l ert -l webfeeder-test.el -f ert-run-tests-batch-and-exit

html:
	$(EMACS) $(EMACS_FLAGS) -batch -l webfeeder.el -l webfeeder-test.el -l webfeeder-test-gen.el -f webfeeder-test-gen

feeds:
	$(EMACS) $(EMACS_FLAGS) -batch -l webfeeder.el -l webfeeder-test.el -l webfeeder-test-gen.el -f webfeeder-test-gen-feeds
