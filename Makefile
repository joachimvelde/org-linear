EMACS ?= emacs
BATCH  = $(EMACS) -Q --batch -L .
SRCS   = org-linear-markdown.el org-linear-api.el org-linear-render.el org-linear-edit.el org-linear.el org-linear-agenda.el org-linear-capture.el

.PHONY: all compile test checkdoc clean

all: compile checkdoc test

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $(SRCS)

test:
	$(BATCH) -L test -l test/org-linear-test.el -f ert-run-tests-batch-and-exit

checkdoc:
	$(BATCH) --eval '(dolist (f (list $(foreach s,$(SRCS),"$(s)"))) (checkdoc-file f))'

clean:
	rm -f *.elc test/*.elc
