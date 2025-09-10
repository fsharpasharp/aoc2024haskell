# Makefile
SHELL := bash

SCRIPT := gen-barrel.sh
BARREL := src/All.hs
# All Haskell sources except the generated barrel
HS_SRCS := $(shell find src -name '*.hs' ! -path 'src/All.hs')

.PHONY: gen build run test clean repl

# Regenerate barrel when sources or the script change
$(BARREL): $(SCRIPT) $(HS_SRCS)
	@./$(SCRIPT)

gen: $(BARREL)

build: gen
	cabal build

# pass args like: make run ARGS="-- --flag value"
run: gen
	cabal run -- $(ARGS)

test: gen
	cabal test

clean:
	cabal clean

repl:
	cabal repl
