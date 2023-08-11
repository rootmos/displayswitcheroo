CC = gcc
CFLAGS ?= -O1 -Wall -Werror
LDFLAGS ?=

LOG_LEVEL ?= INFO
CFLAGS += -DLOG_LEVEL=LOG_$(LOG_LEVEL)

LUA_PKG ?= lua
CFLAGS += $(shell pkg-config --cflags "$(LUA_PKG)")
LDFLAGS += $(shell pkg-config --libs "$(LUA_PKG)")

X11_PKG ?= x11
CFLAGS += $(shell pkg-config --cflags "$(X11_PKG)")
LDFLAGS += $(shell pkg-config --libs "$(X11_PKG)")

.PHONY: all
all: x11.so

x11.so: x11.c
	$(CC) -rdynamic -fPIC -shared $(CFLAGS) -o$@ $^ $(LDFLAGS)

.PHONY: clean
clean:
	rm -f *.so
