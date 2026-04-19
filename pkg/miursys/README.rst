#######
miursys
#######

Purpose
=======

``miursys`` defines the system layer for miur.
It owns the common system base, concrete systems for now, and the common frontend through which the rest of miur talks to them.
The local filesystem system currently belongs here as the ``localfs`` submodule.

Contract
========

``miursys`` should provide:

* the base system protocol,
* registration and lookup of systems,
* interpretation of entity handles according to the owning system,
* system-defined operations applicable to entities,
* shared abstractions and in-package concrete systems such as ``localfs``.

Boundaries
==========

``miursys`` should not:

* redefine entity identity; that belongs to ``miurcore``,
* own cache residency policy,
* own UI/view semantics,
* absorb presentation logic that belongs in ``miurview`` or ``miurtui``.

Dependencies
============

``miursys`` may depend on ``miurcore``.
Concrete systems may live inside ``miursys`` as submodules.
