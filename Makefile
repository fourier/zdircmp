# -*- Mode: makefile; -*-

SBCL = sbcl

all:
	$(SBCL) --noinform --disable-debugger --load build.lisp

clean:
	@rm -rf *.fasl
	@rm -rf *.core
	@rm -rf cl-zdiff
