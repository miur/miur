########
miurview
########

Purpose
=======

``miurview`` contains the presentation-facing graph projection model.
It sits between raw domain/cache state and concrete UI rendering.

Contract
========

``miurview`` should provide:

* cursor and selection state,
* framed list/tree projections,
* view-oriented value objects,
* abstractions that a frontend can render without learning backend details.

Boundaries
==========

``miurview`` should not:

* render terminal widgets itself; that belongs to ``miurtui``,
* interpret entity handles or own operations; that belongs to ``miursys`` and concrete systems,
* own cache memory policy; that belongs to ``miurcache``,
* redefine base identity types; that belongs to ``miurcore``.

Dependencies
============

``miurview`` should depend on ``miurcore`` and stay reusable across multiple frontends.
