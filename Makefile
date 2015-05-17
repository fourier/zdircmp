# -*- Mode: makefile; -*-

SBCL = sbcl --noinform
LW = ~/Development/lw-console
.PHONY: debug
debug: all

.PHONY: release
release: SBCL += --disable-debugger
#--non-interactive
release: all

.DEFAULT_GOAL := release

SOURCES := $(wildcard *.lisp)
BUILD_SRC = build_sbcl.lisp

all: $(SOURCES)
	$(SBCL) --load $(BUILD_SRC)

lw: $(SOURCES)
	$(LW) -build build_lw.lisp

clean:
	@rm -rf *.fasl
	@rm -rf *.core
	@rm -rf zdircmp
