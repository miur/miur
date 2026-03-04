import threading


## ALT:
# def _try_cvt[T: Interpretable](cls: type[T]) -> T | ErrorEntry:
#     class Proxy:
#         def __getattr__(self, name):
#             inst = cls.create_from(target)
#             setattr(self, "__class__", inst.__class__)
#             self.__dict__.update(inst.__dict__)
#             return getattr(inst, name)
#     return Proxy()


class LazyTransform:
    _lock = threading.Lock()

    def __init__(self, target_cls, *args, **kwargs) -> None:
        # Using __dict__ directly avoids triggering any logic
        self.__dict__["_target_cls"] = target_cls
        self.__dict__["_init_args"] = (args, kwargs)

    def _materialize(self) -> None:
        with self._lock:
            # Re-check in case another thread finished while we waited
            if not isinstance(self, self._target_cls):
                cls = self._target_cls
                args, kwargs = self._init_args

                # Create real instance and "coerce" data
                real_instance = cls(*args, **kwargs)

                # ZERO OVERHEAD: Swap the class of 'self'
                # Future lookups use target_cls methods directly
                self.__class__ = cls
                self.__dict__.update(real_instance.__dict__)

                # Cleanup lazy metadata
                del self.__dict__["_target_cls"]
                del self.__dict__["_init_args"]

    def __getattr__(self, name):
        """Triggered only for attributes not in __dict__."""
        self._materialize()
        return getattr(self, name)

    def __dir__(self):
        """Makes dir() match the target class immediately."""
        return dir(self._target_cls)


# --- Example ---
class HeavyObject:
    def __init__(self, val):
        self.val = val
        print("--- HeavyObject fully initialized ---")

    def action(self):
        return f"Action with {self.val}"


# 1. Create proxy: No HeavyObject init yet
lazy_obj = LazyTransform(HeavyObject, 100)

# 2. dir() shows HeavyObject methods (like 'action')
print("action" in dir(lazy_obj))  # True

# 3. Accessing 'action' transforms the object
print(lazy_obj.action())  # Prints "HeavyObject fully..." then "Action with 100"

# 4. Zero overhead: it is now a real HeavyObject
print(type(lazy_obj))  # <class '__main__.HeavyObject'>
