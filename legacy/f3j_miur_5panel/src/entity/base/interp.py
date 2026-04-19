from typing import TYPE_CHECKING, Self, final, override

from ..discover import get_all_subclasses

if TYPE_CHECKING:
    from ..core.error import ErrorEntry
    from .golden import Entities, Entity
    from .traits import Interpretable


class InterpretableImpl:
    __slots__: tuple[str, ...] = ()
    _blacklist: set[type] = set()
    _supported: set[type] = set()
    # PERF: Cache for required attributes per class to avoid re-inspection
    _rq_cache: dict[type, tuple[str, ...]] = {}

    @override
    def __init_subclass__(cls, **kwargs: object) -> None:
        super().__init_subclass__(**kwargs)
        # ALT: use single global "registry: dict[type, set[type]]" for cross-queries
        cls._blacklist = set()
        cls._supported = set()

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

    # RENAME? _compatible_with BAD:(different notion): its != .creatable_from
    # TODO: start hardcoding conversions, and then refactor commonality
    # CASE: "compatible" on class level means:
    #   ~ same `Accessor (automatic)
    #   ~ same .handle and `BaseSystem [of Accessor] (automatic)
    #   ~ we have embeeded conversion AccessorA(x).get_Accessor_B() (semi-automatic)
    #   ~ we know/imported ext system which accepts .handle_B, derivable from .handle_A
    #     = TextLinesProjectionSystem(FSAccessor(path, FileSystem(localhost)).read_file())  | OR: memview(...)
    #     >> to make it "automatic", we should compare all FSAccessor API return types
    #        over all known types `*System(...) accepts (i.e. parse typehint/signature)
    # BUT: not all "files" are ELFFile
    #   ~ i.e. per-instance checks will be returning None (unless they are *very* fast)
    #   ~ and everything not PathLike will be glob-blacklisted by default
    @classmethod
    def _creatable_from(cls, ent: object) -> bool | None:
        # CASE: structural "Poking": does ent have all attributes __init__ expects?
        # ALT:(prematch by typehints): tuple(signature(cls.__call__).parameters.values())[1].annotation
        attrs = cls._get_required_attrs()
        if all(hasattr(ent, a) for a in attrs):
            return True
        ## ALT:PERF? fastest way to preliminary check several attrs
        # _ = (ent.uid, ent.meta, ent.priority)
        return False

    @final
    @classmethod
    def creatable_from(cls, ent: object) -> bool | None:
        """
        PERF:(fast preliminary check): avoid knowingly failing object construction
        USAGE: override in subclasses for value-dependent logic
          ex~: return super().creatable_from(ent) and ent.data(0,4) == "\x7fELF"
        Returns:
            True  - should be able to handle the object (still may throw on actual attempt)
            False - blacklisted beforehand or due to repeated failures
            None  - uncertain, until you try costly .interp_as() and check for exceptions
        """
        tp = type(ent)
        if tp in cls._supported:
            return True
        # TODO: glob "*" to blacklist everything which *isn't* a FSFile nor Path nor PathLike-str
        if any(c in cls._blacklist for c in tp.__mro__):
            return False
        try:
            return cls._creatable_from(ent)
        except AttributeError, TypeError:
            # CASE: attr/type issues usually indicate "fundamental incompatibility"
            cls._blacklist.add(tp)
        except ValueError:
            # CASE: value issues are most likely instance-related, so "try again"
            pass
        return False

    # DONE:DECI:XLR: return ErrorEntry directly here, or wrap it one level higher
    #   ::: if I want to be able to override this method -- it should be in stable .interp_as
    @classmethod
    def create_from(cls, ent: object) -> Self:
        """The actual factory. Assumes .eligible() was checked or will raise."""
        # FUT: raise ConversionError(f"Missing field: {e.name}")
        fields = {a: getattr(ent, a) for a in cls._get_required_attrs()}
        try:
            # BET? explicitly raise FailedInterp to distinguish from other errors
            return cls(**fields)  # ADD: , parent=ent
            ## DISABLED: no way to distinguish if whole class vs specific instance are convertible
            # cls._supported.add(type(ent))
        except AttributeError, TypeError:
            # FIXME: auto-blacklist only if classess are really incompatible
            #   (i.e. keep trying FSFile -> ELF for each separate instance)
            cls._blacklist.add(type(ent))
            raise

    # RENAME? .coerce_to .reinterpret_as .try_cvt_to ?
    # BET?(overload): .interp_as()
    @final
    def _lazy_try_interp_as[T: Interpretable](
        self, target_cls: type[T]
    ) -> T | ErrorEntry:
        from ..core.objaction import ObjAction, pyobj_to_actions

        # MAYBE: dif Lazy* UI color for known(True)=GREN vs unknown(None)=YELW vs failed()=RED
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
        # from .action import Action
        # class LazyInterpAction(Action):
        #     def __init__(
        #         self,
        #         name: str,
        #         parent: Entity,
        #         fn: Callable[[], Any],
        #         allowpreview: bool = True,
        #     ) -> None:
        #         super().__init__(
        #             name, parent, sfn=lambda: cvt_to_ents(fn(), parent=self)
        #         )
        #         self.allowpreview = allowpreview

        def _deferred() -> Entities | ErrorEntry:
            try:
                tgt = target_cls.create_from(self)
            except Exception as exc:
                from ..core.error import ErrorEntry

                nm = f".interp_as({target_cls.__qualname__})"
                return ErrorEntry(name=nm, parent=self, exc=exc)  # type: ignore[arg-type]  # pyright: ignore[reportArgumentType]  # pylint:disable=line-too-long
            return pyobj_to_actions(tgt, parent=tgt)

        # MAYBE:NEED: global registry of Entities to give them unique short names to disambiguate them
        #   e.g. check if __name__ alrady exists and then fallback to uniquely shortened __qualname__
        nm = target_cls.__name__
        return ObjAction(name=nm, parent=self, allowpreview=False, fn=_deferred)

    # ARCH:THINK! list of `Actions we return... can be unified as .interp_as(ActionsListing)
    #   i.e. in same way how .explore() gives some listing of chosen .interp_as type
    #   ALSO: list of `Actions itself -- is a curated subset of API from `SystemAccessor e.g. from os.path.*
    @final
    def interp_as(self) -> Entities:
        """
        Scans all loaded Entity classes and yields those compatible
        with the current instance structure/data/state.
        """
        from .golden import Golden

        deferred: list[type[Entity]] = []
        for subcls in get_all_subclasses(Golden):
            sup = subcls.creatable_from(self)
            if sup is False:  # <PERF: 1st step skip surely unsupported conversions
                continue
            if sup is None:
                deferred.append(subcls)
                continue
            # NOTE: it's somewhat confusing to have .name inof .type
            yield self._lazy_try_interp_as(subcls)

        if deferred:
            # RENAME? _try_{possible,unknown}
            def _try_remaining() -> Entities:
                return [self._lazy_try_interp_as(subcls) for subcls in deferred]

            from ..core.objaction import ObjAction

            yield ObjAction(name="@try_remaining", parent=self, fn=_try_remaining)  # type: ignore[arg-type]  # pyright: ignore[reportArgumentType]  # pylint:disable=line-too-long


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
