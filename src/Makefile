ROOT := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
include $(ROOT)/../tools/common.makefile

LUA_PKG ?= lua
CFLAGS += $(shell pkg-config --cflags "$(LUA_PKG)")
LDFLAGS += $(shell pkg-config --libs "$(LUA_PKG)")

X11_PKG ?= x11
CFLAGS += $(shell pkg-config --cflags "$(X11_PKG)")
LDFLAGS += $(shell pkg-config --libs "$(X11_PKG)")

XRANDR_PKG ?= xrandr
CFLAGS += $(shell pkg-config --cflags "$(XRANDR_PKG)")
LDFLAGS += $(shell pkg-config --libs "$(XRANDR_PKG)")

READLINE_PKG ?= readline
READLINE_CFLAGS += $(shell pkg-config --cflags "$(READLINE_PKG)")
READLINE_LDFLAGS += $(shell pkg-config --libs "$(READLINE_PKG)")

.PHONY: build
build: cli.exe x11.so

cli.exe: cli.c x11.c r.c version.c
	$(CC) $(CFLAGS) $(READLINE_CFLAGS) -o$@ $^ $(LDFLAGS) $(READLINE_LDFLAGS)

x11.so: x11.c r.c
	$(CC) -rdynamic -fPIC -shared $(CFLAGS) -o$@ $^ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f *.so *.exe version.*