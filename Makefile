BUILDFLAG=.buildflag

.PHONY: run
run: $(BUILDFLAG)
	stack exec displayswitcheroo

.PHONY: info
info: $(BUILDFLAG)
	stack exec displayswitcheroo info

$(BUILDFLAG): $(shell git ls-files)
	stack test --pedantic
	touch $@
