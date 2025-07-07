# RENAME? about.py
# MOVE? ./pkg/{about,version}.py
#   => to make pkg into proper module at the same time
"""
SUMMARY: Modern [re]Invented Unified navigatoR
"""

__appname__ = "miur"

# ALT:BET? specify version only in pyproject.toml, and then access it by
#   ver = importlib.metadata.version("miur")
# FUT: starting from 1.0.0.2027xxxx additionally summarize minor features into major ones
#   WHY: much easier to decide to try if major features were added (or avoid if they changed)
__version__ = "0.571.20250319"
