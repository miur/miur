########
miurdemo
########

Purpose
=======

``miurdemo`` contains demo scenarios, smoke paths, and integration sketches.
It exists to exercise package boundaries without pushing demo code into production packages.

Contract
========

``miurdemo`` should provide:

* small runnable examples,
* end-to-end smoke entry points,
* sample app/system/entity flows for development.

Boundaries
==========

``miurdemo`` should not:

* become the main application package,
* own reusable business logic that belongs in sibling packages,
* be required by core domain packages.

Dependencies
============

``miurdemo`` may depend on high-level workspace members such as ``miurapp`` to assemble demo flows.
Production packages should not depend on ``miurdemo``.
