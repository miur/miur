# SRC: https://stackoverflow.com/questions/79868994/using-hatchling-how-can-i-import-and-use-classes-in-my-package-during-the-build
import pickle
import sys
from pathlib import Path

from hatchling.builders.hooks.plugin.interface import BuildHookInterface


class CustomBuildHook(BuildHookInterface):
    def initialize(self, version, build_data):
        root = Path(self.root)

        # import your package code without installing it
        sys.path.insert(0, str(root / "src"))
        from build_example.example import Example

        text = (root / "src" / "build_example" / "data.txt").read_text(encoding="utf-8")
        obj = Example.parse(text)

        # write generated file into build directory (not repo)
        out_dir = Path(self.directory) / "build_example"
        out_dir.mkdir(parents=True, exist_ok=True)
        out_file = out_dir / "_compiled.pkl"
        out_file.write_bytes(pickle.dumps(obj))

        # put it into the wheel under your package
        build_data.setdefault("force_include", {})
        build_data["force_include"][str(out_file)] = "build_example/_compiled.pkl"
