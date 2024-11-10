.. SPDX-FileCopyrightText: 2024 Dmytro Kolomoiets <amerlyq+code@gmail.com>

.. SPDX-License-Identifier: CC-BY-SA-4.0

####
miur
####

Modern Interface for Uniform Reconnaissance.

.. note::
   It is much **more** than a simple file manager.

**Mission:** interpret everything as graphs and navigate them like you do in filesystem.

&motto ⌇⡦⡋⣑⣳

- "Everything is a list"
- "Anything can be interpreted as Entity"
- "Making choices is a function"  [delegated to *YOU* by the program]
- "Embrace cognitive constrains"
- "All workflows in one catalogue"


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



ONGOING

- separate FDs for jupyter, tty, stdin/stdout and logs
- terminal resize
- selectors mainloop (w/o asyncio)


TBD

- re-IMPL curses ListWidget MVP
- homoiconic Entity/Action data interpretation
- subscribe/publish change propagation channels
- new miur-relevant argparse
