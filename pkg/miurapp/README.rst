#######
miurapp
#######

Purpose
=======

``miurapp`` is the application/runtime package.
It assembles the live program out of systems, resident state, view state, and a frontend.

Contract
========

``miurapp`` should provide:

* application bootstrap,
* runtime assembly and dependency wiring,
* lifecycle startup/shutdown glue,
* top-level entry points for running miur.

Boundaries
==========

``miurapp`` should not:

* define entity semantics; that belongs to ``miursys``,
* own resident graph/state data; that belongs to ``miurcache``,
* own view projection semantics; that belongs to ``miurview``,
* render terminal widgets directly; that belongs to ``miurtui``.

Dependencies
============

``miurapp`` may depend on the other production packages in order to wire them together.
Lower-level packages should not depend on ``miurapp``.
