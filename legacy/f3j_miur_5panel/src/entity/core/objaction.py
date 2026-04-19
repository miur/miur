import inspect
from typing import Any, Callable, Iterable, Mapping, get_type_hints

# from ..util.termansi import num_lo, num_up
from ..base.action import Action
from ..base.golden import Entities, Entity, Golden
from .error import ErrorEntry
from .fsentry import FSAuto
from .text import TextEntry


## ALT
# try:
#     if __import__("sys").flags.isolated:
#         __import__("site").main()  # lazy init for "site" in isolated mode
#     from just.ext.print import cvt_one
# except ImportError:  # ModuleNotFoundError
#     cvt_one = repr
def cvt_to_ents(x: Any, parent: Entity) -> Entities:
    if x is None:
        # MAYBE: test if {fun.type_annotations == "-> None"} and don't print anything
        return [ErrorEntry(parent=parent, name="None ∅")]
    if callable(x):
        return [ObjAction(name=f"{x}()", parent=parent, fn=x)]

    # NOTE: repack/rewrap `Accessor into new Entry (like monadic `Sfn)
    # if isinstance(x, Entities):
    #     return x
    # if isinstance(x, Entity):
    #     return [x]
    if isinstance(x, Golden):
        return [x]
    if isinstance(x, (dict, Mapping)):
        # TODO: yield e.g. FSDir with altname pair -- so you can follow it, and yet have a name
        #   ex~: FSDir(name="name: </path/to/smth>", path=/path/to/smth)
        #   ALSO: do the same for any other object -- actual obj, with replaced name field
        #     MAYBE: make special DictPair or KeyValueEntry(name:str, obj:Entry)
        # lvl += " "
        # for i, k in enumerate(sorted(x), start=1):
        #     for v in cvt_to_ents(x[k], parent, lvl):
        #         yield TextEntry(f"{lvl}{num_up(i):>2s} {k}: {v}", parent)

        ## ALT:THINK: inof Entities here, yield intermediary list of Actions to InterpAs/RepresentAs
        # return ObjAction(
        #     name="[dict_as_pairs]",
        #     parent=parent,
        #     fn=lambda x=x: (f"{k}: {x[k]}" for k in sorted(x)),
        # )
        # FAIL: sorted(x) doesn't work for mixed int/str keys
        return [TextEntry(f"{k}: {x[k]}", parent) for k in x]
    if isinstance(x, tuple):
        # for a in x:
        #     for v in cvt_to_ents(a, parent, lvl):
        #         yield TextEntry(f" ⸱\t{v}", parent)
        # return ObjAction(
        #     name="[tuple_as_list]",
        #     parent=parent,
        #     # fn=lambda x=x: map(str, x),
        #     fn=lambda x=x: iter(x),
        # )
        ## TODO: recursive description w/ or w/o intermediate `Actions
        return [TextEntry(str(a), parent) for a in x]
    if isinstance(x, str):
        if any(x.startswith(p) for p in ("/", "~/", "../")):
            # return [FSAuto(x, parent), TextEntry(x, parent)]
            return [FSAuto(x, parent)]
        return [TextEntry(x, parent)]
    if isinstance(x, int):  # (int, DT, TM, DD)
        return [TextEntry(str(x), parent)]
    if isinstance(x, float):
        return [TextEntry(f"{x:.6f}", parent)]
    # if isinstance(x, Path):
    #     # return repr(str(x))
    #     return FSAuto(x, parent)
    # if isinstance(x, TT):
    #     return dt_hmx(x)
    try:
        it = iter(x)
    except TypeError:
        # yield from pyobj_to_actions(x, parent)
        # for ms, mf in inspect.getmembers(x):
        #     yield TextEntry(f"{lvl}{ms}: {mf}", parent)
        # return ObjAction(
        #     name="[inspect.getmembers]",
        #     parent=parent,
        #     fn=lambda x=x: (f"{ms}: {mf}" for ms, mf in inspect.getmembers(x)),
        # )
        return list(pyobj_to_actions(x, parent))
    # lvl += " "
    # for i, a in enumerate(it, start=1):
    #     for v in cvt_to_ents(a, parent, lvl):
    #         yield TextEntry(f"{lvl}{num_lo(i):>2s} {v}", parent)
    # return ObjAction(
    #     name="[iterate]",
    #     parent=parent,
    #     fn=lambda it=it: (cvt_to_ents(a, parent) for a in it),
    # )
    ## BAD: flattens
    ## TODO: recursive description w/ or w/o intermediate `Actions
    ##   MAYBE: use monadic application ?
    ## FIXME: do generate single iteration only, and interpret next one lazily
    ##   << don't immediately generate whole tree depth until leafs
    return [v for a in it for v in cvt_to_ents(a, parent)]


# RENAME? ActionInterp/Introspect
#   BAD: PyAction is easily confusable in logs with python native errors
class ObjAction(Action):
    def __init__(
        self,
        name: str,
        parent: Entity,
        fn: Callable[[], Any],
        allowpreview: bool = True,
    ) -> None:
        super().__init__(name, parent, sfn=lambda: cvt_to_ents(fn(), parent=self))
        self.allowpreview = allowpreview


def get_hints(fn: Any) -> dict[str, Any]:
    import sys

    extra_ns = {
        "Entity": Golden[Any],
        "Entities": Iterable[Golden[Any]],
        "Golden": Golden,
    }

    module = sys.modules.get(fn.__module__, None)
    injected = {}
    if module is not None:
        for name, val in extra_ns.items():
            if name not in module.__dict__:
                module.__dict__[name] = val
                injected[name] = val
    try:
        return get_type_hints(fn)
    finally:
        # clean up what we injected
        if module is not None:
            for name in injected:
                module.__dict__.pop(name, None)


def get_sig(fn: Any) -> inspect.Signature:
    ## ALT: gather all objects to resolve sigs
    # # entity/base/ns.py — no imports from within entity, pure stdlib
    # from typing import Any
    #
    # _registry: dict[str, Any] = {}
    #
    # def register(**kwargs: Any) -> None:
    #     _registry.update(kwargs)
    #
    # def get_ns() -> dict[str, Any]:
    #     return _registry
    # python# entity/base/golden.py — after defining Entity, Entities
    # from .ns import register
    # register(Entity=Entity, Entities=Entities, Golden=Golden)
    # python# anywhere that needs to resolve annotations
    # import inspect
    # import typing
    # from entity.base.ns import get_ns
    #
    # def get_hints(fn: Any) -> dict[str, Any]:
    #     return typing.get_type_hints(fn, localns=get_ns())
    #
    # def get_sig(fn: Any) -> inspect.Signature:
    #     return inspect.signature(fn, eval_str=True, globals=get_ns())

    # print(sys.modules[fn.__module__].__dict__.get("Entities"))

    try:
        # hints = get_type_hints(fn, globalns=extra_ns, localns=fn.__globals__)
        # merged_ns = {**fn.__globals__, **extra_ns}
        # hints = get_type_hints(fn, globalns=merged_ns)
        hints = get_hints(fn)
    except Exception:
        return inspect.signature(fn, follow_wrapped=False)
    sig = inspect.signature(fn, follow_wrapped=False)
    params = [
        p.replace(annotation=hints.get(name, p.annotation))
        for name, p in sig.parameters.items()
    ]
    return sig.replace(
        parameters=params,
        return_annotation=hints.get("return", sig.return_annotation),
    )


# ALT:XP~: directly produce this list by Golden.explore(), accessed only as:
#   Action.explore(): super(self, Golden).explore() to introspect itself
#   [_] ALT:BET? per-Entity introspection :DEV: .actions() -> Actions
def pyobj_to_actions(obj: Any, parent: Entity) -> Entities:

    # ALT:PERF(slow): @runtime_checkable : isinstance(ent, Explorable)
    #   https://mypy.readthedocs.io/en/latest/protocols.html#using-isinstance-with-protocols
    # [_] ALSO: iterate over readable @property
    for k, v in inspect.getmembers(obj, inspect.ismethod):
        ## WARN: we also may need to exclude methods which aren't applicable to particular file
        #   ~ no reason to text_lines -- if it's a binary file
        #   ~ no reason to code_syntax_lines -- if syntax isn't supported
        #   ~ no reason to .explore(empty_file) -- unless you trying to insert some new lines
        ## OLD:TODO: filter by return type "-> Iterable[Golden]" (or at least "-> Golden" for `Dashboard)
        ##   OR: `Interpret any non-explore as generic TextEntry(str(...))
        if k.startswith("_"):
            continue

        ## FIXED: skip methods I can't auto-execute
        #    FUT: generate list of some possible values for methods to pick in UI and still execute them
        #      IDEA: allow user to "insert" new values into suggested list -- basically "accepting user input"
        try:
            # print("-----------")
            # print(v)
            # sig = inspect.signature(v, follow_wrapped=False)
            sig = get_sig(v)
            # print(sig)
        except Exception as exc:
            from ...util.exchook import log_exc

            log_exc(exc)
        else:
            params = [
                p
                for p in sig.parameters.values()
                if p.default is inspect.Parameter.empty
                and p.kind
                not in (
                    inspect.Parameter.VAR_POSITIONAL,  # *args
                    inspect.Parameter.VAR_KEYWORD,  # **kwargs
                )
            ]
            # print(params)
            if params:
                continue

        # try:
        #     hints = get_hints(v)
        #     ret = hints.get("return")
        #     # filter by return type if needed
        #     if ret is not None and not _matches_entities(ret):
        #         continue
        # except Exception:
        #     continue

        # IDEA: rename {.explore==.default} to show only "L" as 1st `Action in list
        # OR=name=f"{k.capitalize()}:"
        yield ObjAction(name=f".{k}()", parent=parent, fn=v)


## ALT:(signature-prediction):
##   USAGE: yield ObjAction(name=f".{k}()", parent=parent, sfn=wrapent(v))
# def wrapent(v: Callable[[], Any]) -> Callable[[], Entities]:
#     r = inspect.signature(v).return_annotation
#     # assert 0, type(r)
#     # BET? don't even expect to ever return `Entity
#     #   i.e. use Path inof FSEntry (with all its API) and simply cvt returned paths by FSAuto here
#     #   BAD: we need a way to limit all possible Path API to reasonably useful list
#     # ALT: repack/rewrap `Accessor into new Entry (like monadic `Sfn)
#     if r in (Entities, Iterable[Entity], Iterable[Golden[Any]]):
#         return v
#     if r in (Entity, Golden[Any]):
#         return lambda: [v()]
#     # ALT? return ErrorEntry, meaning "you need to generalize your API"
#     return lambda: cvt_to_ents(v(), parent=parent)
