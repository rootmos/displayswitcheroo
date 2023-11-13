export DESTDIR ?= $(HOME)/.local

.PHONY: build
build:
	$(MAKE) -C src build

.PHONY: install
install:
	./install.sh

.PHONY: doc
doc: build
	$(MAKE) -C doc

.PHONY: clean
clean:
	$(MAKE) -C src clean
	$(MAKE) -C doc clean
