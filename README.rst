.. SPDX-FileCopyrightText: 2025 Dmytro Kolomoiets <amerlyq+code@gmail.com>

.. SPDX-License-Identifier: CC-BY-SA-4.0

####
miur
####

Modern/Modal Interface for Uniform Reconnaissance.

**Mission:** interpret everything as graphs and navigate them like a filesystem.

**Motto:** ⌇⡦⡋⣑⣳

- "Everything is a list"
- "Anything can be interpreted as list"
- "Multiple ways to interpret -- is a list too"
- "Making choices is a function"  [delegated to *YOU* by the program]
- "Workflow as a plugin"  -- upload yours and explore others
- "Consistency first, usability second"  [-- everything else is not on the chart]
- "Be frontend to anything out there"  [bridge what you can; reimplement what you must]
- "Tools in the loop"  (interpret output as structures and actions)
- "Explore even itself"  (events, logs, ui, config, workflows, keybindings, runtime, RAM, etc.)
- "Embrace cognitive constrains"  (suggest workflows and give feedback)
- "Bind all events to current contexts"  == that's simply how attention-memory works
- "File Manager is a lie. What you manage -- is your attention."

.. tip::
   It's hilarious, but you can treat any folder of files as a *Lisp* program.
   If you name folders/files after some AST -- you can execute the dir as an app! (homoiconicity)
   By using scripts as function names and symlinks to other files as args,
   you should be able to compose an adhoc application -- something worthy to explore.
   Writing code solely by moving files around -- who would even dare to call it programming? :)


CHANGELOG
=========

TBD

- homoiconic Entity/Action data interpretation
- subscribe/publish change propagation channels
- new miur-relevant argparse


2025-02-14
----------

DONE

- interlace objects (Entitis) and methods (Actions) during navigation
- auto-convert any python object to explorable entity
- preview nodes under cursor and previous history
- inputfield/editbox for FilterBy() with readline/emacs bindings
- draft localhost rootnodes for FS, /proc, pacman
- draft internal rootnodes for env, keybind, demos
- draft protocol rootnodes for web, mpd
- draft dataset rootnode for unicode symbols
- add demos for future UI clients in qt6/qml/sdl3/glfw/imgui/pyqtgraph
- provide ZSH integration and shell aliases
- add clipboard / copy to xclip
- adaptive layout for navi
- list UI got itemwrap, colsep, spacemark, linenum, decortail


2024-12-01
----------

DONE

- list widget (SatelliteViewport)
- class hierarchy for ui
- automatic venv
- PoC file system navigation
- PoC highlighting file by type
- PoC pygmentized in curses
- PoC selectors mainloop (w/o asyncio)
- separate FDs for jupyter, tty, stdin/stdout and logs
- FIXED: no curses resize on SIGWINCH


2024-06-01
----------

DONE

- curses terminal UI
- asyncio based mainloop
- jupyter kernel integration
- bash multi-shebang with aliases
- tty shell_out (async!)
- pipe stdin/stdout (with concurrent tty)
- print to terminal altscreen
- new lightweight logger
- global app singleton
