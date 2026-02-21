from typing import Any, override

g_registries: dict[type[AutoRegistered], list[type[AutoRegistered]]] = {}


def entry_cls_names(
    registry: list[type[AutoRegistered]] | None = None,
) -> dict[str, type[AutoRegistered]]:
    reg = registry or g_registries.get(AutoRegistered, [])
    return {x.__name__: x for x in reg}


def entry_cls_aliases(
    registry: list[type[AutoRegistered]] | None = None,
) -> dict[str, type[AutoRegistered]]:
    reg = registry or g_registries.get(AutoRegistered, [])
    return {x.altname or x.__name__.removesuffix("Entry").lower(): x for x in reg}


# RENAME? `Discoverable
class AutoRegistered:
    """Metaclass that auto-registers subclasses."""

    altname: str | None = None
    _registry: list[type[AutoRegistered]] | None = None

    ## [_] FUT: track all *instances* (not classes) and do explicit memory-bound gc-collecting
    # def __new__(cls, *_args: Any, **_kwds: Any) -> Self:
    #     # OFF:REF:(no args/kwds): https://mail.python.org/pipermail/python-dev/2008-February/076854.html
    #     # def __new__[**P](cls, *_args: P.args, **_kwds: P.kwargs) -> Self:
    #     # BAD:(false-positive): https://github.com/pylint-dev/pylint/issues/8325
    #     obj = super().__new__(cls)
    #     print(">>>", obj)
    #     g_entries_cls.append(obj)
    #     return obj

    # Golden: type  # < Forward declaration
    # def __new__(mcs, name: str, bases: tuple, namespace: dict):
    #     cls = super().__new__(mcs, name, bases, namespace)
    #     if name != "Golden":
    #         for base in bases:
    #             base_mro = getattr(base, "__mro__", ())
    #             if Golden in base_mro or (
    #                 hasattr(base, "__mro__")
    #                 and any(b.__name__ == "Golden" for b in base_mro)
    #             ):
    #                 AutoRegistered._registry.append(cls)
    #                 break
    #     return cls

    # ALT: recursively inspect Action.__subclasses__()
    #   REF: https://stackoverflow.com/questions/3862310/how-to-find-all-the-subclasses-of-a-class-given-its-name
    #   REF: https://adamj.eu/tech/2024/05/10/python-all-subclasses/
    # ALT: walk through whole runtime code and inspect everything in existence
    @override
    def __init_subclass__(cls, /, altname: str | None = None, **kwargs: "Any") -> None:
        super().__init_subclass__(**kwargs)

        if cls._registry is None:
            for base in cls.__mro__[1:]:
                if base is AutoRegistered:
                    cls._registry = []
                    g_registries[cls] = cls._registry
                    break
                if isinstance(base, type) and getattr(base, "_registry") is not None:
                    cls._registry = getattr(base, "_registry")
                    break
        if cls._registry is not None:
            cls._registry.append(cls)

        if altname:
            # CASE: making complex entries names easier to use/refer from cmdline
            # USAGE: class FSEntry(Golden, altname='fs'): pass
            cls.altname = altname

        ## MAYBE: map all available `EntryInterperter to original `*Entry data
        ## SEE: :/_try/e31_xlr_entity/e46_ent_3interlace.py
        # global: _g_actions: dict[type, list[type]] = {}
        # ta = tuple(signature(cls.__call__).parameters.values())[1].annotation
        # _g_actions.setdefault(ta, []).append(cls)
