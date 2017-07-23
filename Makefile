PR := ./alt.py
# PR := ./miur.py
.DEFAULT_GOAL = main

.PHONY: main
main: PYTHONASYNCIODEBUG=1
main:
	@$(PR)

.PHONY: test
# py.test -s  # print() on screen
test:
	py.test -- $(shell pwd)

DEPS := python-pytest python-pytest-cov python-pytest-mock python-pycallgraph
deps-install:
	sudo pacman -S $(DEPS:%='%') </dev/stdin >/dev/stdout

call-graph:
	pycallgraph graphviz --output-file="/tmp/pycallgraph.png" -- $(PR)
	feh "/tmp/pycallgraph.png"
