from typing import Any, Final, NewType, Protocol, cast, override, reveal_type

# BAD:(IntEnum + jurigged): old inst/vars will break or cmp as False against the updated version
# ALT:(auto): py:$ def _generate_next_value_(name, start, count, last_values): return count
# from enum import IntEnum, auto
# class Style(IntEnum):
#     unknown = 0
#     default = auto()
#     ...


class AutoEnumMeta(type):
    _UNUSED: Final = "_UNUSED"
    _MAXID: Final = 1024

    _names_by_id: list[str]
    _next_autoid: int

    @staticmethod
    def _is_member_name(name: str) -> bool:
        return name.isupper() and not name.startswith("_")

    def __new__(
        mcs, name: str, bases: tuple[type, ...], ns: dict[str, Any]
    ) -> "AutoEnumMeta":
        cls = super().__new__(mcs, name, bases, ns)
        cls._names_by_id = ["_ERROR"]
        cls._next_autoid = 1
        for k, v in ns.items():
            if cls._is_member_name(k):
                setattr(cls, k, v)
        return cls

    @override
    def __setattr__(cls, name: str, member_id: int) -> None:
        """Safely catches Jurigged's automated `setattr` passes during runtime file changes."""
        if cls._is_member_name(name):
            nm2id = cls._names_by_id
            if member_id < 0:
                raise ValueError("ERR: efficient AutoEnum requires non-negative ids")
            if member_id == 0:
                i = cls._next_autoid
                while i < len(nm2id) and nm2id[i] is not cls._UNUSED:
                    i += 1
                if i >= cls._MAXID:
                    raise RuntimeError(f"BUG? AutoEnum has >={cls._MAXID} members")
                cls._next_autoid = i + 1
                member_id = i
            if member_id >= len(nm2id):
                nm2id += [cls._UNUSED] * (member_id + 1 - len(nm2id))
            elif nm2id[member_id] is not cls._UNUSED:
                raise ValueError(f"Duplicate member ID {member_id} for {name!r}")
            nm2id[member_id] = name
        super().__setattr__(name, member_id)

    # WARN: jurigged doesn't use delattr() -- so re-added line returns the same value
    @override
    def __delattr__(cls, name: str) -> None:
        member_id = cls.__dict__.get(name, 0)
        if 1 <= member_id < len(cls._names_by_id):
            cls._names_by_id[member_id] = f"_DELETED({name})"
        super().__delattr__(name)

    def get_name(cls, member_id: int) -> str:
        if 1 <= member_id < len(cls._names_by_id):
            name = cls._names_by_id[member_id]
            # ALT: return whatever name (deleted or unset) and fail later
            if name is not cls._UNUSED and not name.startswith("_"):
                return name
        raise ValueError(f"No member matches {member_id!r}")


class TypedEnum[T](Protocol):
    @classmethod
    def __getattr__(cls, name: str) -> T: ...


def _test() -> None:
    """
    >>> type(Style.DEFAULT)
    <class 'int'>
    >>> Style(3)
    'FOOTER'
    >>> (Style.DEFAULT, Style.ITEM, Style.FOOTER)
    (1, 2, 3)
    >>> setattr(Style, "TESTVALUE", 0); Style.TESTVALUE
    4
    >>> pidx = int(Style.ITEM); delattr(Style, "ITEM"); setattr(Style, "ITEM", 0); Style(pidx)
    Traceback (most recent call last):
        ...
    ValueError: No member matches 2
    >>> Style.ITEM
    5
    """

    class _Style(metaclass=AutoEnumMeta):
        DEFAULT = 0
        ITEM = 0
        FOOTER = 0

    StyleId = NewType("StyleId", int)
    Style = cast(TypedEnum[StyleId], _Style)
    print(reveal_type(Style))
    print(reveal_type(Style.DEFAULT))
    # print(Style["ITEM"])
    # print(list(Style))

    # OR: Style = _test()
    # OR: __test__ = {"Style": Style}
    # OR: python -m doctest enum_hotreload.py -v
    # OR: pytest --doctest-modules
    __import__("doctest").testmod(verbose=False, globs=locals())


if __name__ == "__main__":
    _test()
