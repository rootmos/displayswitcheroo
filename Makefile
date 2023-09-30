.PHONY: build
build:
	$(MAKE) -C src build

.PHONY: install
install: build
	install src/cli.exe $(HOME)/.local/bin/displayswitcheroo2
	install -D data/displayswitcheroo/list.lua $(HOME)/.local/share/displayswitcheroo/list.lua
	install -D data/displayswitcheroo/displayswitcheroo.lua $(HOME)/.local/share/displayswitcheroo/displayswitcheroo.lua
