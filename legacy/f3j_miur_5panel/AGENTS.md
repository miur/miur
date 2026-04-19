# Miur Project Agent Guidelines

Miur is a TUI file manager written in Python 3.14+ using curses. This file provides guidelines for AI agents working on this codebase.

## Project Overview

- **Type**: Python TUI (Terminal User Interface) application
- **Python Version**: 3.14+
- **Main Package**: `src/` (exposed as `miur` package)
- **Entry Point**: `python -m miur` or `miur` command

## Build, Lint, and Test Commands

### Installation

```bash
# Install in editable mode with all dependencies
pip install -e "miur[all,dev]"

# Install just runtime dependencies
pip install -e "miur"
```

### Code Formatting

```bash
# Format code with black
black src/

# Sort imports with isort
isort src/
```

### Type Checking

```bash
# Run mypy for type checking (configured in pyproject.toml)
mypy src/

# Or use the language server (pylsp) with mypy plugin
pylsp
```

### Linting

```bash
# Run pylint
pylint src/
```

### Running the Application

```bash
# Run miur
miur

# Or via python module
python -m miur
python -m miur --help
```

### Testing

```bash
# Run pytest (if tests exist)
pytest

# Run a single test file
pytest t/test_file.py

# Run a single test function
pytest t/test_file.py::test_function_name

# Run tests matching a pattern
pytest -k "test_pattern"
```

## Code Style Guidelines

### Comments

**IMPORTANT**: Always preserve existing comments in the codebase. This project uses extensive inline comments to document design decisions, trade-offs, and future work. Never delete or modify existing comments without explicit permission.

Comments use special prefixes:
- `# FIXME:` - Known issues needing fix
- `# TODO:` - Planned future work
- `# PERF:` - Performance-related notes
- `# BET:` - Better approach suggestions
- `# WARN:` - Warnings about potential issues
- `# RENAME?` - Suggestions for renaming
- `# ALT:` - Alternative approaches
- `# NOTE:` - Important notes

### Imports

- Use `TYPE_CHECKING` block for imports only needed for type hints
- Group imports: standard library, third-party, local
- Use explicit relative imports: `from .module import X`
- Avoid wildcard imports

```python
from typing import TYPE_CHECKING, Callable, Optional

import _curses as C

from .app import AppGlobals, g_app
from .util.logger import log

if TYPE_CHECKING:
    from .ui.navi import NaviWidget
```

### Type Annotations

- Use modern Python type hints (3.14+)
- Use `from typing import override` for method overrides
- Use `| None` instead of `Optional[]` for simple cases
- Use string annotations for forward references when needed

```python
class AppOptions:
    devroot: str | None = None
    color: bool | None = True

type KeyTable = "dict[str | int, Callable[[AppGlobals], None] | KeyTable]"
```

### Naming Conventions

- **Classes**: PascalCase (`AppGlobals`, `RootWidget`)
- **Functions/variables**: snake_case (`handle_input`, `log_state`)
- **Constants**: UPPER_SNAKE_CASE
- **Private members**: prefix with underscore (`_navi()`, `self._ent`)

### Error Handling

- Use specific exception types
- Avoid bare `except:` clauses
- Use `logging` for error reporting instead of print

```python
try:
    KM.handle_input(g)
except Exception as exc:
    from .util.exchook import log_exc
    log_exc(exc)
```

### Class Structure

- Use dataclasses for simple data containers
- Use `@cache` decorator for memoization
- Group related functionality into modules e.g. `entity/`, `ui/`, `util/`, `integ/`

### File Organization

- `src/miur/app.py` - Application state and options
- `src/miur/cli.py` - Command-line argument parsing
- `src/miur/keymap.py` - Keyboard mappings
- `src/miur/miur.py` - Main application logic
- `src/miur/entity/` - File/entity types (fsentry, elfnode, etc.)
- `src/miur/ui/` - UI widgets and views
- `src/miur/util/` - Utility modules (logger, envlevel, etc.)
- `src/miur/integ/` - Integration with external tools (shell, nvim, jupyter)

### Formatting Preferences

- Line length: 88 characters (black default)
- Use trailing commas in multi-line calls
- Use parentheses for line continuation
- Disable formatting for specific lines with `# fmt: off` and `# fmt: on`

### Testing Guidelines

- Place tests in `t/` directory
- Use pytest fixtures
- Test one thing per test function
- Include descriptive docstrings

### Common Patterns

- Global state via `g_app = AppGlobals()`
- Lazy imports via `@cache` decorator for optional dependencies
- Entry point registration through entity catalogue
- Modal keymaps for different interaction modes
