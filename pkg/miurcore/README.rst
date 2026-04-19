########
miurcore
########

Purpose
=======

``miurcore`` defines the domain vocabulary shared across the workspace.
It should stay small, stable, and free of frontend or backend policy.

Contract
========

``miurcore`` should provide:

* entity identity as ``system`` plus opaque ``handle``,
* core value objects and type aliases,
* backend-agnostic protocols where needed,
* invariants every other package can rely on.

Boundaries
==========

``miurcore`` should not:

* perform I/O,
* allocate or evict cache slots,
* dispatch commands,
* render UI,
* interpret opaque handles; that belongs to the owning system in ``miursys``.

Dependencies
============

Other workspace packages may depend on ``miurcore``.
``miurcore`` should avoid depending on sibling packages.
