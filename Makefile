#
# SPDX-FileCopyrightText: 2016-2020 Dmytro Kolomoiets <amerlyq@gmail.com> and contributors.
#
# SPDX-License-Identifier: Apache-2.0
#
.DEFAULT_GOAL = main

pkgname := miur
brun := ./main/$(pkgname).py
bdir := _build

.PHONY: main
main: PYTHONASYNCIODEBUG=1
main:
	@$(brun)

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
	pycallgraph graphviz --output-file='$(bdir)/pycallgraph.png' -- '$(brun)'
	feh '$(bdir)/pycallgraph.png'

mem-prf:
	python -u -m memory_profiler -- '$(brun)'


.PHONY: pkg-install
pkg-install: force := 1
pkg-install: install := 1
pkg-install: pkg-build


.PHONY: pkg-build
pkg-build: PKGBUILD
	install -vCDm644 -t '$(bdir)/_pkg' '$<'
	ln -srvfT '$(shell pwd)' $(bdir)/_pkg/src
	env -C '$(bdir)/_pkg' -- makepkg --syncdeps --clean \
	  $(if $(force),--force) $(if $(install),--install) \
	  $(_args) >/dev/tty


## DEBUG: verify threading and ZeroMQ created threads
.PHONY: ps
ps:
	@pstree -ct $(if $(args),-aps) $$(pgrep $(pkgname))


log:
	tail -F '$(bdir)/$(pkgname).log'
