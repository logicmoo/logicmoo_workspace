EMACS ?= emacs
EMACS_FLAGS = --quick --directory .
EMACS_BATCH = $(EMACS) --batch $(EMACS_FLAGS)

EL  := $(wildcard *.el)
ELC := $(patsubst %.el,%.elc,$(EL))

.PHONY: all
all: compile

.PHONY: compile test
compile: $(ELC)

%.elc: %.el
	$(EMACS_BATCH) --eval "(progn (message (emacs-version)) (setq byte-compile-error-on-warn t) (batch-byte-compile))" $<

test:
	$(EMACS_BATCH) --eval "(progn\
	(load-file \"test/system-packages-test.el\") \
	(ert-run-tests-batch-and-exit))"
