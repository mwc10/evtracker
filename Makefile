ELM_SOURCES := src/*.elm
DIST_DIR := dist/
ELM_BUNDLE := $(addprefix $(DIST_DIR), main.js)
OPT ?= --optimize

default: optimized

debug: OPT := --debug
debug: $(ELM_BUNDLE)
optimized: OPT := --optimize
optimized: $(ELM_BUNDLE)

$(ELM_BUNDLE): $(ELM_SOURCES)
	elm make $(ELM_SOURCES) --output=$(ELM_BUNDLE) $(OPT)

clean:
	-rm -rf $(ELM_BUNDLE)

.PHONY: default debug optimized clean
