CC = gcc
CFLAGS ?= -O1 -Wall -Werror
LOG_LEVEL ?= INFO
CFLAGS += -DLOG_LEVEL=LOG_$(LOG_LEVEL)

.PHONY: run
run: build
	./displayswitcheroo

.PHONY: build
build: displayswitcheroo

displayswitcheroo: displayswitcheroo.c r.h
	$(CC) -o $@ $(CFLAGS) $< -lX11 -lXrandr

.PHONY: clean
clean:
	rm -f displayswitcheroo
