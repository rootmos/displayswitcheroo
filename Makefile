CC = gcc
CFLAGS ?= -O1 -Wall -Werror
LOG_LEVEL ?= INFO
CFLAGS += -DLOG_LEVEL=LOG_$(LOG_LEVEL)

.PHONY: run
run: build
	./displayswitcheroo

.PHONY: install
install: build
	install --strip -D -t $(PREFIX)/bin xhook
	$(call service, xhook)

.PHONY: build
build: displayswitcheroo

displayswitcheroo: displayswitcheroo.c r.h
	$(CC) -o $@ $(CFLAGS) $<

.PHONY: clean
clean:
	rm -f displayswitcheroo
