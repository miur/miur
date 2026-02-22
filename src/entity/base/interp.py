from typing import TYPE_CHECKING, Iterator, Self

if TYPE_CHECKING:
    from .golden import Entities, Entity


class InterpretableMixin:
    # PERF: Cache for required attributes per class to avoid re-inspection
    _req_cache: dict[type, tuple[str, ...]] = {}

    @classmethod
    def _get_required_attrs(cls) -> tuple[str, ...]:
        """PERF: Caches __init__ signatures to avoid inspection overhead."""
        if cls not in cls._req_cache:
            import inspect

            ## ALT(Base): Golden: _required_fields: tuple[str, ...] = ()
            ##   ex~:(Derived): _required_fields = ("uid", "meta", "priority")
            sig = inspect.signature(cls.__init__)
            cls._req_cache[cls] = tuple(
                p.name
                for p in sig.parameters.values()
                if p.name not in ("self", "args", "kwargs")
                and p.kind in (p.POSITIONAL_OR_KEYWORD, p.KEYWORD_ONLY)
            )
        return cls._req_cache[cls]

    @classmethod
    def creatable_from(cls, ent: Entity) -> bool | None:
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
    def create_from(cls, ent: Entity) -> Self:
        """The actual factory. Assumes .eligible() was checked or will raise."""
        # FUT: raise ConversionError(f"Missing field: {e.name}")
        fields = {a: getattr(ent, a) for a in cls._get_required_attrs()}
        return cls(**fields)

    # RENAME? .reinterpret_as ?
    ## MAYBE?(overload): .interpret_as ?
    def coerce_to[T](self, target: type[T]) -> T:
        """Fluent entry point: ex~: my_ent.coerce_to(EntityB)"""
        return target.create_from(self)

    @staticmethod
    def _ensure_entities_are_registered() -> None:
        """Standard recursive loader using importlib.resources."""
        import importlib
        import importlib.resources
        import sys

        pkg_name = __package__.rpartition(".")[0]
        pkg_files = importlib.resources.files(pkg_name)
        for path in pkg_files.rglob("*.py"):
            if path.name == "__init__.py":
                continue
            rel_path = path.relative_to(pkg_files).with_suffix("")
            module_name = f"{pkg_name}.{'.'.join(rel_path.parts)}"
            if module_name not in sys.modules:
                try:
                    importlib.import_module(module_name)
                except ImportError:
                    continue

    def interp_as(self) -> Entities:
        """
        Imports all entity modules ecosystem on first call.
        (This triggers class auto-registration into __subclasses__())
        Then replaces itself with a high-performance version.
        """
        self._ensure_entities_are_registered()
        # NOTE: swap this method to bypass initialization next time
        self.__class__.interp_as = self._orig_interp_as
        return self.interp_as()

    def _orig_interp_as(self) -> Entities:  # OR:  -> Iterator[type[Self]]
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

        # MAYBE: stop inhereting from `AutoRegistered as we have __subclasses__ ?
        def walk(base: type[Entity]) -> Iterator[type[Entity]]:
            """Recursively find all entity types."""
            for sub in base.__subclasses__():
                # if sub is self.__class__:
                #     continue
                if sub.creatable_from(self) is not False:  # True or None
                    yield sub
                yield from walk(sub)

        from ..error import ErrorEntry
        from ..objaction import ObjAction
        from .golden import Entity

        compat: set[str] = set()
        for cls in walk(Entity):
            nm = cls.__name__  # cls.__qualname__
            try:
                yield cls.create_from(self)
                compat.add(nm)
            except Exception as exc:
                yield ErrorEntry(name=f".interp({nm}): {exc}", parent=self)
                continue

    #     yield ObjAction(
    #         name="@try_others",
    #         parent=self,
    #         fn=lambda: self._yield_remaining_interps(compat),
    #     )
    #
    # def _yield_remaining_interps(self, tried: set[str]) -> Entities:
    #     h = self._x.handle
    #     for interp_class in InterpRegistry.all():
    #         interp = interp_class()
    #         if interp.name not in tried:
    #             try:
    #                 yield from interp.interpret(h)
    #             except Exception as exc:
    #                 yield ErrorEntry(name=f".interp({interp.name}): {exc}", parent=self)


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
