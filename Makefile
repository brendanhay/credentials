SHELL := /usr/bin/env bash

build:
	stack build --copy-bins

clean:
	stack clean
