all: build

CABAL ?= cabal

configure:
	$(CABAL) configure

deps:
	$(CABAL) install --only-dependencies

build:
	$(CABAL) build
