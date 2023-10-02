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

SRC = x11.c edid.c r.c

cli.exe: cli.c version.c wait.c $(SRC)
	$(CC) $(CFLAGS) $(EXTRA_CFLAGS) $(READLINE_CFLAGS) -o$@ $^ $(LDFLAGS) $(READLINE_LDFLAGS) $(EXTRA_LDFLAGS)

x11.so: $(SRC)
	$(CC) -rdynamic -fPIC -shared $(CFLAGS) -o$@ $^ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f *.so *.exe version.*
