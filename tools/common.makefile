ROOT := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

TOOLS ?= $(realpath $(ROOT))
VERSION ?= $(TOOLS)/version

CC = gcc
PKG_CONFIG ?= pkg-config

CFLAGS ?= -Wall -Werror -O2
LDFLAGS ?=
LOG_LEVEL ?= WARN
EXTRA_CFLAGS ?= -DLOG_LEVEL=LOG_$(LOG_LEVEL)
EXTRA_LDFLAGS ?=

.PHONY: all
all: build

version.c version.h: $(VERSION) $(shell $(VERSION) -i)
	$(VERSION) -c version.c -h version.h
