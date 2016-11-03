PR := ./miur.py
.DEFAULT_GOAL = main

.PHONY: main
main: PYTHONASYNCIODEBUG=1
main:
	@$(PR)
