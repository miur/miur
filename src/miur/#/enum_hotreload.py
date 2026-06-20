"""Small integer IDs that survive Jurigged-style class hot reloads."""

from collections.abc import Iterator
from typing import Any, NewType, override, reveal_type

## ALT:BET? prevent passing StyleId into "int" functions
# Trick Basedpyright: It thinks this is a standard, strict Enum class.
#   At runtime, your metaclass will overwrite this and use pure integers.
# from enum import Enum
# class StyleId(Enum):
#     pass
StyleId = NewType("StyleId", int)

## Global type sentinel instance
# BAD? StyleId vs StyleEnum -- two types to go through code
# MAYBE: make class return "int" for generic impl? OR:USE: generic [T]
AUTO = StyleId(0)


def _is_member_name(name: str) -> bool:
    return name.isupper() and not name.startswith("_")


class MutableEnumMeta(type):
    _names_by_id: list[str | None]
    _next_id: int

    def __new__(
        mcs, name: str, bases: tuple[type, ...], ns: dict[str, Any]
    ) -> "MutableEnumMeta":
        ns["_names_by_id"] = [None]
        ns["_next_id"] = 1

        cls = super().__new__(mcs, name, bases, ns)
        for key in ns:
            if _is_member_name(key):
                cls._register_member(key)
        return cls

    @override
    def __call__(cls, member_id: StyleId | None = None) -> str:
        if member_id is None:
            raise TypeError(
                f"{cls.__name__} is an enum registry and cannot be instantiated"
            )
        if 0 <= member_id < len(cls._names_by_id):
            name = cls._names_by_id[member_id]
            if name is not None:
                return name
        raise ValueError(f"No member matches {member_id!r}")

    @override
    def __setattr__(cls, name: str, _value: object) -> None:
        """Safely catches Jurigged's automated `setattr` passes during runtime file changes."""
        if _is_member_name(name):
            cls._register_member(name)
            return
        super().__setattr__(name, _value)

    # WARN: jurigged doesn't use delattr() -- so re-added line returns the same value
    @override
    def __delattr__(cls, name: str) -> None:
        member_id = cls.__dict__.get(name)
        if isinstance(member_id, int) and member_id < len(cls._names_by_id):
            cls._names_by_id[member_id] = None  # OR: = f"{name}(DELETED)"
        super().__delattr__(name)

    def __iter__(cls) -> Iterator[tuple[str, StyleId]]:
        for name, value in cls.__dict__.items():
            if _is_member_name(name) and isinstance(value, int):
                yield name, StyleId(value)

    def __getitem__(cls, name: str) -> StyleId:
        value = cls.__dict__.get(name)
        if _is_member_name(name) and isinstance(value, int):
            return StyleId(value)  # FIXME: unnecessary ctor?
        raise KeyError(name)

    def _register_member(cls, name: str) -> StyleId:
        existing_id = cls.__dict__.get(name)
        # Protect re-ordered keys: If it's already an active assigned integer, keep it!
        if isinstance(existing_id, int) and existing_id > 0:
            member_id = existing_id
        else:
            # New block or added back: Allocate a clean monotonic integer tracking value
            member_id = cls._next_id
            cls._next_id += 1

        while len(cls._names_by_id) <= member_id:
            cls._names_by_id.append(None)
        cls._names_by_id[member_id] = name

        super().__setattr__(name, StyleId(member_id))
        return StyleId(member_id)  # WTF: again recreates


class MutableEnum(metaclass=MutableEnumMeta):
    __slots__ = ()

    # def __new__(cls) -> Never:
    #     raise TypeError(
    #         f"{cls.__name__} is an enum registry and cannot be instantiated"
    #     )


class Style(MutableEnum):
    UNKNOWN = AUTO
    DEFAULT = AUTO
    ITEM = AUTO
    FOOTER = AUTO


def _demo() -> None:
    # FAIL: not coercible
    _myid: StyleId = 42
    print(Style.__dict__.items())

    # CASE:: type correctness
    def test_sty(val: StyleId) -> None:
        print(val)

    def test_int(val: int) -> None:
        print(val)

    test_sty(Style.DEFAULT)
    test_sty(2)
    test_int(Style.DEFAULT)
    print(reveal_type(Style.DEFAULT))
    print(type(Style.DEFAULT))

    # CASE:: access
    print(Style.DEFAULT, Style.ITEM, Style.FOOTER)  # Output: 1, 2, 3
    print(Style["ITEM"])
    print(list(Style))
    # print(Style(Style.ITEM))

    # CASE:: jurigged loads file with added member
    setattr(Style, "TESTVALUE", AUTO)
    print(Style["TESTVALUE"])  # Output: 4 (Grown monotonically)

    # CASE:: jurigged tries to removed members missing from reloaded file
    #   CHECK: does it even work?
    pidx = int(Style.ITEM)
    delattr(Style, "ITEM")
    setattr(Style, "ITEM", AUTO)
    print(Style["ITEM"])  # Output: 5 (Re-added key with same value always NEW)
    try:
        Style(pidx)
    except ValueError as exc:
        print(exc)  # Output: Index 2 does not exist anymore


def _alt() -> None:
    from typing import Protocol, cast

    # BAD: doesn't work for jurigged, as decorator isn't re-executed
    class TypedEnum[T](Protocol):
        def __getattr__(self, name: str) -> T: ...
    # OR: def enum_styleid(cls: type) -> TypedEnum[StyleId]:
    #     return cast(TypedEnum[StyleId], cls)
    def enum_styleid(cls: type) -> TypedEnum[StyleId]:
        counter = 1
        for key, value in list(cls.__dict__.items()):
            if not key.startswith("_") and value == 0:
                setattr(cls, key, counter)
                counter += 1
        return cast(TypedEnum[StyleId], cls)
    @enum_styleid
    class MyStyles:
        item = 0
        header = 0
        footer = 0
    reveal_type(MyStyles.item)
    reveal_type(type(MyStyles))
    print(type(MyStyles.item))  # <class 'int'> (Zero runtime wrappers!)


if __name__ == "__main__":
    _demo()
    _alt()
