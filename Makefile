PR := ./miur.py
.DEFAULT_GOAL = main

.PHONY: main
main: PYTHONASYNCIODEBUG=1
main:
	@$(PR)

.PHONY: test
# py.test -s  # print() on screen
test:
	py.test -- $(shell pwd)

DEPS := python-pytest python-pytest-cov python-pytest-mock
deps-install:
	sudo pacman -S $(DEPS:%='%') </dev/stdin >/dev/stdout
