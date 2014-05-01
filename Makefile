# -*- Mode: makefile; -*-

SBCL = sbcl --noinform

.PHONY: debug
debug: all

.PHONY: release
release: SBCL += --disable-debugger
#--non-interactive
release: all

.DEFAULT_GOAL := release

SOURCES := $(wildcard *.lisp)
BUILD_SRC = build.lisp

all: $(SOURCES)
	$(SBCL) --load $(BUILD_SRC)

clean:
	@rm -rf *.fasl
	@rm -rf *.core
	@rm -rf zdircmp
