.. SPDX-FileCopyrightText: 2026 Dmytro Kolomoiets <amerlyq+code@gmail.com>

.. SPDX-License-Identifier: CC-BY-SA-4.0

####
miur
####

**MIUR = Modular Interactive Universal exploreR**

MIUR is an experimental *explorer-first graph workbench* for **navigating, previewing, and operating**
on heterogeneous entities through one uniform protocol.

The project treats files, text fragments, process outputs, workflows, annotations, and remote resources as
nodes in a shared graph instead of forcing everything through a filesystem-only view. The goal is to let a
UI move through that graph with stable handles, serializable operations, and incremental data loading.

Concept
=======

MIUR is organized around a few core ideas:

* **Graph-native exploration.** The primary model is a graph of entities and relations rather than a tree
  of paths.
* **Explorer-first UX.** Views, cursors, previews, and commands are first-class parts of the architecture.
* **Uniform access layer.** Different backends should expose data through the same accessor protocol.
* **Incremental materialization.** Only the active working set should stay in memory; old data may be
  regenerated, evicted, or persisted.
* **Serializable operations.** Navigation and actions should be representable as stable commands that can be
  replayed, transported, or logged.

Current Direction
=================

The current design notes point toward a Python 3.14 implementation with:

* a compact in-memory representation for millions of small entities,
* a cache of only the currently observed part of the graph,
* lazy expansion of high-fanout relations,
* persistence for unique or annotated data,
* regeneration of disposable derived data,
* multiple possible frontends, with TUI as the most immediate target.

The background work also emphasizes separating the **memory layout problem** from the **data lifecycle
problem**: compact arrays may hold the active working set, while cache policy and persistence decide what
stays resident.

Repository Notes
================

This repository currently contains the project metadata, background notes, and several older prototypes under
``legacy/``. The legacy material is useful as design history, but it should not be read as the current
implementation architecture.

See also:

* ``./&g/background.nou`` for the initial design discussion.
* ``./docs/ARCH.nou`` for the distilled architecture notes.
* ``./docs/TODO.nou`` for the current implementation priorities.
