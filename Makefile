SHELL   := /usr/bin/env bash
OUT_DIR ?= bin

build: $(OUT_DIR)
	stack build --copy-bins --local-bin-path=$(OUT_DIR)

clean:
	stack clean

$(OUT_DIR):
	mkdir -p $@
