ROOT := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

TOOLS ?= $(realpath $(ROOT))
VERSION ?= $(TOOLS)/version

CC = gcc
PKG_CONFIG ?= pkg-config

CFLAGS ?= -Wall -Werror -O1
LDFLAGS ?=

EXTRA_CFLAGS ?=
EXTRA_LDFLAGS ?=

LOG_LEVEL ?= INFO
CFLAGS += -DLOG_LEVEL=LOG_$(LOG_LEVEL)

.PHONY: all
all: build

version.c version.h: $(VERSION) $(shell $(VERSION) -i)
	$(VERSION) -c version.c -h version.h
