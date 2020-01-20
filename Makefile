#
# SPDX-FileCopyrightText: 2016-2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: MIT
#

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


.PHONY: reuse
reuse:
	reuse lint


DEPS := python-pytest python-pytest-cov python-pytest-mock
deps-install:
	sudo pacman -S $(DEPS:%='%') </dev/stdin >/dev/stdout

PRFDEPS := python-pycallgraph python-psutil python-memory_profiler
deps-profile:
	pacaur -S $(PRFDEPS:%='%') </dev/stdin >/dev/stdout

call-graph:
	pycallgraph graphviz --output-file="/tmp/pycallgraph.png" -- $(PR)
	feh "/tmp/pycallgraph.png"

mem-prf:
	python -u -m memory_profiler -- "$(PR)"
