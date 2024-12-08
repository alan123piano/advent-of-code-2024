.PHONY: all
all: run

.PHONY: run
run:
	cabal run

.PHONY: clean
clean:
	cabal clean

.PHONY: fmt
fmt:
	cabal run ormolu -- --mode inplace $(shell find app -name '*.hs')
