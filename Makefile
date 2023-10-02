export DESTDIR ?= $(HOME)/.local

.PHONY: build
build:
	$(MAKE) -C src build

.PHONY: install
install:
	./install.sh

.PHONY: clean
clean:
	$(MAKE) -C src clean
