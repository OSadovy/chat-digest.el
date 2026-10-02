EMACS ?= emacs

# Packages come from `package-initialize', so telega is the one in use.
BATCH = $(EMACS) -Q --batch --eval '(setq load-prefer-newer t)' \
	--eval '(progn (require (quote package)) (package-initialize))' \
	-L .

EL = $(filter-out %-autoloads.el %-pkg.el, $(wildcard *.el))

.PHONY: all compile test clean

all: compile test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(EL)

test:
	$(BATCH) -l test/chat-digest-tests.el -f ert-run-tests-batch-and-exit

# A stale .elc wins over the source beside it.
clean:
	rm -f *.elc test/*.elc
