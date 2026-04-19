#######
miurtui
#######

Purpose
=======

``miurtui`` contains the terminal UI frontend for miur.
It is responsible for presentation, input handling, and terminal-specific interaction patterns.

Contract
========

``miurtui`` should provide:

* terminal rendering and layout code,
* key/input translation into commands,
* widget and frame presentation,
* TUI entry points.

Boundaries
==========

``miurtui`` should not:

* own the graph domain model,
* own cache policy,
* implement system interpretation directly,
* become the main home of command semantics beyond input translation.

Dependencies
============

``miurtui`` should depend on ``miurview`` and minimal core types.
``miurapp`` may depend on ``miurtui`` for runtime wiring; the dependency should not point the other way.
Lower-level packages should not depend on ``miurtui``.
