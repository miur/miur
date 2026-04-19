#########
miurcache
#########

Purpose
=======

``miurcache`` owns the resident graph state for miur.
It behaves like a working-set cache, but it is also the live DOM-like centerpiece through which resident entities and relations are exposed.

Contract
========

``miurcache`` should provide:

* resident entity and relation state,
* entity identity to resident-slot mapping,
* eviction policy hooks,
* persistence handoff points,
* memory-budget aware cache operations,
* a structured API over the live resident graph.

Boundaries
==========

``miurcache`` should not:

* define the domain model; that belongs to ``miurcore``,
* define system interpretation or entity operations; that belongs to ``miursys``,
* render UI; that belongs to ``miurtui``,
* own application bootstrap/runtime wiring; that belongs to ``miurapp``.

Dependencies
============

``miurcache`` may depend on ``miurcore`` and optional persistence libraries.
Higher layers may depend on ``miurcache``; lower layers should not.
