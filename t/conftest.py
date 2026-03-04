"""Pytest configuration for pymake tests."""

import sys
from pathlib import Path


def pytest_configure() -> None:
    """Ensure repository root is importable."""
    root = Path(__file__).resolve().parents[1]
    if str(root) not in sys.path:
        sys.path.insert(0, str(root))
