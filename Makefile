BUILDFLAG=.buildflag

.PHONY: run
run: $(BUILDFLAG)
	stack exec displayswitcheroo

$(BUILDFLAG): $(shell git ls-files)
	stack test --pedantic
	touch $@
