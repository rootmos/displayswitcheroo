THIS := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
ROOT ?= $(realpath $(THIS)/..)
TOOLS ?= $(ROOT)/tools

export EXE ?= $(ROOT)/src/cli.exe
export APP ?= displayswitcheroo

export XDG_DATA_DIRS ?= $(ROOT)/data

INCLUDE ?= $(TOOLS)/include
USAGE ?= $(TOOLS)/usage

.PHONY: all
all: $(ROOT)/README.md

$(ROOT)/README.md: README.in.md usage.$(APP) output.list
	$(INCLUDE) "$<" "$@"

usage.$(APP): $(EXE)
	$(USAGE) $@

output.%:
	$(EXE) $* > "$@"

.PHONY: clean
clean:
	rm -f usage
