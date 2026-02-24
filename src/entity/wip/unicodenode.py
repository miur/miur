# %ONELINE: print(''.join([chr(i) for i in range(0x110000) if ord(chr(i)) < 0x10000]))
from typing import override

from ..base.golden import Entities, Entity, Golden
from ..core.text import TextEntry


class UnicodeNode(Golden[str]):
    def __init__(self, text: str, parent: Entity) -> None:
        super().__init__(text, parent)

    @override
    def explore(self) -> Entities:
        import unicodedata

        # BAD:(on .explore): tracemalloc=9MiB -> 59MiB
        #   (>70MiB (pss=162MiB) if you then scroll-around with preview)
        # TODO:ALG: "lazy load" only several initial screens (sliding/jumping window)
        #   ++ ALG: FilterBy() should directly use unicodedata.* API
        #        << PERF: inof always keeping and filtering 10^6 (100MiB) records ourselves
        for ci in range(0x110000):
            c = chr(ci)
            # NOTE: my usual WF is to pick symbol for #nou, so it should be printable
            if not c.isprintable():
                continue
            # OFF:REF: https://www.unicode.org/reports/tr44/#General_Category_Values
            # if unicodedata.category(c) in ('Cs', 'Cu'):
            #     continue
            try:
                name = unicodedata.name(c)
                yield TextEntry(f"U+{ci:04X}│ {c} - {name}", self)
            except ValueError:
                pass
