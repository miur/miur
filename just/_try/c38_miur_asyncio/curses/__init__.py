import sys
import os

# ## ERR: no KEY_RESIZE (on SIGWINCH) in interactive python/jupyther
# ##   FIXED: clear vars in __init__.py to enforce HACK before very first "import curses"
# # REF: Curses terminal resize problems when Python is in interactive mode ⌇⡥⠇⡬⡟
# #   https://github.com/python/cpython/issues/46927#
# # NOT:(jupyter): "readlines" in sys.modules
# if "LINES" not in os.environ and "COLS" not in os.environ:
#     os.unsetenv("LINES")
#     os.unsetenv("COLUMNS")
#     # DEBUG: print(os.system('env|sort|grep -e LINES -e COLUMNS'))
