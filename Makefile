PR := ./miur.py
.DEFAULT_GOAL = test

.PHONY: main
main: PYTHONASYNCIODEBUG=1
main: test
	@$(PR)

.PHONY: test
# py.test -s  # print() on screen
test:
	py.test -- $(shell pwd)
