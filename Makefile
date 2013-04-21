
GENNAME := gen-css
GEN := dist/build/$(GENNAME)/$(GENNAME)
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

.PHONY: all clean pristine $(GEN)

all: $(OUT)

$(OUT): $(GEN)
	$(GEN) > $(OUT)

$(GEN):
	cabal-dev install-deps $(CABALFLAGS)
	cabal-dev configure
	cabal-dev build

clean:
	-rm $(OUT)
	-cabal-dev clean

pristine: clean
	-rm -rf cabal-dev

