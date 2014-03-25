# -*- Mode: makefile; -*-

SBCL = sbcl

all:
	$(SBCL) --noinform --load build.lisp

clean:
	@rm -rf *.fasl
	@rm -rf *.core
	@rm -rf cl-zdiff
