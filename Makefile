# -*- Makefile -*-
SHELL = /bin/sh
EMACS ?= emacs

clean:
	@rm -f *~
	@rm -f \#*\#
	@rm -f *.elc

test: clean
	@$(EMACS) -batch -Q -L . -l lazyflymake.el -l tests/lazyflymake-tests.el