from typing import Any, Callable


# RENAME? @implements
def augment_with[T](*providers: type[Any]) -> Callable[[type[T]], type[T]]:
    """Copies methods from provider classes into the target at class-definition time."""

    def wrapper(target_cls: type[T]) -> type[T]:
        for provider in providers:
            # for name, attr in provider.__dict__.items():
            for name, attr in vars(provider).items():
                if name.startswith("__"):
                    continue

                ## CHECK:(do I even need this?): disallow super() in mixins
                # Get the underlying function to check for super()
                # func = attr
                # if isinstance(attr, (classmethod, staticmethod)):
                #     func = attr.__func__
                #
                # if callable(func) and hasattr(func, "__code__"):
                #     # Check for 'super' in the function's global names
                #     if "super" in func.__code__.co_names:
                #         raise TypeError(
                #             f"Method '{name}' in {mixin.__name__} uses 'super()'. "
                #             "Flattened mixins must call base classes explicitly "
                #             "to maintain a flat MRO."
                #         )

                # CHECK: If you use setattr(LargeClass, "method", Mixin.method), the function object remains
                # identical. If that function contains a super() call, it looks for the __class__ cell,
                # which is hard-coded to Mixin. Since Mixin isn't in the MRO of LargeClass, super() will
                # throw a TypeError at runtime.
                assert name not in vars(target_cls)  # don't overwrite overrides
                setattr(target_cls, name, attr)
        return target_cls

    return wrapper


# ALT:(Protocol):NEED:BAD:PERF:(@runtime_checkable):COS:TEMP: to focus_on(match-case Golden())
# @augment_with(GoldenProtocol, RepresentableImpl, AddressableImpl, ExplorableImpl, InterpretableImpl)
@augment_with(InterpretableImpl)
class Golden[T]:
    __slots__ = ()  # NOTE: slots work fine; augment_with uses setattr on the *class*
    # __slots__ = ("_x", "_parent", "__dict__")

    ## ALT:(augment_with): delegate calls to "mixin" instances (=Composition=)
    # def __init__(self):
    #     self._handler = DataHandler()
    # def __getattr__(self, name):
    #     return getattr(self._handler, name)
