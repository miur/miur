from typing import Any, Iterable, Self, cast

from ..autoreg import get_all_subclasses
from .traits import Entities, Entity, Interpretable


# if TYPE_CHECKING:
#     from .golden import GoldenAny


class InterpretableImpl:
    # PERF: Cache for required attributes per class to avoid re-inspection
    _rq_cache: dict[type, tuple[str, ...]]

    @classmethod
    def _get_required_attrs(cls) -> tuple[str, ...]:
        """PERF: Caches __init__ signatures to avoid inspection overhead."""
        if cls not in cls._rq_cache:
            import inspect

            ## ALT(Base): Golden: _required_fields: tuple[str, ...] = ()
            ##   ex~:(Derived): _required_fields = ("uid", "meta", "priority")
            sig = inspect.signature(cls.__init__)
            cls._rq_cache[cls] = tuple(
                p.name
                for p in sig.parameters.values()
                if p.name not in ("self", "args", "kwargs")
                and p.kind in (p.POSITIONAL_OR_KEYWORD, p.KEYWORD_ONLY)
            )
        return cls._rq_cache[cls]

    @classmethod
    def creatable_from(cls: type[Self], ent: Any) -> bool | None:
        """
        PERF:(fast preliminary check): avoid knowingly failing object construction
        USAGE: override in subclasses for value-dependent logic
          ex~: return super().accepts(subject) and ent.data(0,4) == "\x7fELF"
        Returns:
            True  - Certain this interpretation can handle the object
            False - Certain this interpretation cannot handle the object
            None  - Not sure, force interpret() and treat exceptions as "not possible"
        """
        try:
            # CASE: structural "Poking": does ent have all attributes __init__ expects?
            # ALT:(prematch by typehints): tuple(signature(cls.__call__).parameters.values())[1].annotation
            attrs = cls._get_required_attrs()
            ## ALT:PERF? fastest way to preliminary check several attrs
            # _ = (subject.uid, subject.meta, subject.priority)
            if all(hasattr(ent, a) for a in attrs):
                return True
            # CASE:(uncertain): you don't know until you try to create object
            return None
        except (AttributeError, ValueError, TypeError):  # REMOVE?
            return False

    @classmethod
    def create_from(cls: type[Self], ent: Entity) -> Self:
        """The actual factory. Assumes .eligible() was checked or will raise."""
        # FUT: raise ConversionError(f"Missing field: {e.name}")
        fields = {a: getattr(ent, a) for a in cls._get_required_attrs()}
        return cls(**fields)

    # RENAME? .reinterpret_as ?
    ## MAYBE?(overload): .interpret_as ?
    def coerce_to[T: Interpretable](self, target: type[T]) -> T:
        """Fluent entry point: ex~: my_ent.coerce_to(EntityB)"""
        return target.create_from(self)

    def interp_as(self: Entity) -> Entities:  # OR:  -> Iterator[type[Self]]
        """
        Scans all loaded Entity classes and yields those compatible
        with the current instance state.
        """
        ## ALT:
        # candidates = []
        # for cls in self.__class__.mro()[1].__subclasses__():
        #     try:
        #         cls.make_from(self)
        #         candidates.append(cls)
        #     except (AttributeError, ValueError, ConversionError):
        #         continue
        # return candidates

        from ..core.error import ErrorEntry
        from .golden import Golden

        def _try_cvt[T: Interpretable](cls: type[T]) -> T | ErrorEntry:
            try:
                # FIXME:PERF: return lazy-init (or self-replace) named proxies to .create_from() only on access
                #   and produce results=[ErrorEntry] on error (or self-replace itself by ErrorEntry)
                # e.g::
                # class Proxy:
                #     def __getattr__(self, name):
                #         inst = cls.create_from(target)
                #         setattr(self, "__class__", inst.__class__)
                #         self.__dict__.update(inst.__dict__)
                #         return getattr(inst, name)
                # return Proxy()
                return cls.create_from(self)
            except Exception as exc:
                nm = f".interp_as({cls.__qualname__})"
                return ErrorEntry(name=nm, parent=self, exc=exc)

        # TODO: dif color .creatable_from for known(True)=GREN vs unknown(None)=YELW vs failed()=RED
        deferred: list[type[Entity]] = []
        # HACK: Tell the type checker these are concrete types that satisfy the interface
        subclasses = cast(Iterable[type[Entity]], get_all_subclasses(Golden[Any]))
        for subcls in subclasses:
            sup = subcls.creatable_from(self)
            if sup is False:  # <PERF: skip surely unsupported conversions
                continue
            if sup is None:
                deferred.append(subcls)
                continue
            yield _try_cvt(subcls)

        # RENAME? _try_{possible,unknown}
        def _try_remaining() -> Entities:
            return map(_try_cvt, deferred)

        from ..core.objaction import ObjAction

        yield ObjAction(name="@try_remaining", parent=self, fn=_try_remaining)


# class InterpArbiter:
#     """Decides interpretation order based on type and defaults."""
#
#     DEFAULT_PRIORITY: dict[type, list[str]] = {
#         str: ["elf", "text", "hex"],
#         bytes: ["hex", "text"],
#     }
#
#     user_preferences: dict[type, list[str]] = {}
#
#     def get_ordered(self, obj: object, interps: list[Any]) -> list[Any]:
#         """Return interpretations in priority order."""
#         obj_type = type(obj)
#         base_order = self.DEFAULT_PRIORITY.get(obj_type, [])
#
#         def sort_key(interp: Any) -> int:
#             if interp.name in base_order:
#                 return base_order.index(interp.name)
#             return len(base_order) + 999  # Unknown types at end
#
#         return sorted(interps, key=sort_key)
#
#
# # Global arbiter instance
# _arbiter = InterpArbiter()
#
#
# def get_ordered_interps(obj: object) -> list[Any]:
#     """Helper function to get interpretations in priority order."""
#     return _arbiter.get_ordered(obj, obj.interp_as())
