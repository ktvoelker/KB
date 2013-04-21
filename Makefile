
GENNAME := gen-css
GEN := dist/build/$(GENNAME)/$(GENNAME)
SOURCES := $(shell find clay -name '*.hs')
OUT := static/main.css

CABALFLAGS := \
	--disable-library-profiling \
	--disable-executable-profiling \
	--disable-optimization \
	--disable-library-for-ghci \
	--disable-executable-stripping \
	--disable-tests \
	--disable-library-coverage \
	--disable-benchmarks \
	--disable-documentation

.PHONY: all clean pristine setup run

all: $(OUT)

$(OUT): $(GEN)
	$(GEN) > $(OUT)

$(GEN): $(SOURCES)
	cabal-dev install-deps $(CABALFLAGS)
	cabal-dev configure
	cabal-dev build

clean:
	-rm $(OUT)
	-cabal-dev clean

pristine: clean
	-rm -rf .ready cabal-dev ENV

setup: pristine .ready

.ready:
	scripts/setup.sh

run: .ready $(OUT)
	scripts/run.sh

